#!/usr/bin/env python3

import ctypes
import subprocess
from typing import Callable, Tuple, TypeVar, List, Dict, Set, NewType

K = TypeVar('K')
V = TypeVar('V')


class Node:
    def __init__(self, kind: str, children: List['Node']) -> None:
        self.kind = kind
        self.children = children

    def __repr__(self) -> str:
        return f'Node({self.kind!r}, {self.children})'

    def print(self, indent: int = 0) -> None:
        print('  ' * indent + self.kind)
        for child in self.children:
            child.print(indent + 1)


class Parser:
    def __init__(self, src: str) -> None:
        self.source = src
        self.position = 0

    def _peek(self) -> str:
        return self.source[self.position]

    def _next(self) -> str:
        self.position += 1
        return self.source[self.position - 1]

    def _expect(self, exp: str) -> None:
        c = self._next()
        assert c == exp

    def _eof(self) -> bool:
        return self.position == len(self.source)

    def _atom(self) -> Node:
        if self._peek() == '(':
            self._next()
            node = self._group()
            self._expect(')')
            return node
        return Node(self._next(), [])

    def _repeat(self) -> Node:
        node = self._atom()
        if not self._eof() and self._peek() in '+*?':
            kind = self._next()
            return Node(kind, [node])
        return node

    def _group(self) -> Node:
        items = []
        while not self._eof() and self._peek() != ')':
            items.append(self._repeat())
        return Node('()', items)

    @staticmethod
    def parse(src: str) -> Node:
        parser = Parser(src)
        node = parser._group()
        assert parser._eof()
        return node


def get_or(d: Dict[K, V], k: K, v: Callable[[], V]) -> V:
    if k in d:
        return d[k]
    r = v()
    d[k] = r
    return r


NfaState = NewType('NfaState', int)


class Nfa:
    def __init__(self, node: Node) -> None:
        self.states: List[Dict[str, Set[NfaState]]] = []

        self.beg, self.end = self._build(node)

    def _state(self) -> NfaState:
        s = NfaState(len(self.states))
        self.states.append({})
        return s

    def _trans(self, frm: NfaState, inp: str, to: NfaState) -> None:
        get_or(self.states[frm], inp, set).add(to)

    def _null(self, frm: NfaState, to: NfaState) -> None:
        self._trans(frm, '', to)

    def _build(self, node: Node) -> Tuple[NfaState, NfaState]:
        if node.kind == '()':
            trans = [self._build(c) for c in node.children]
            for i in range(len(trans) - 1):
                self._null(trans[i][1], trans[i + 1][0])
            return trans[0][0], trans[-1][1]
        if node.kind == '?':
            assert len(node.children) == 1
            beg, end = self._build(node.children[0])
            self._null(beg, end)
            return beg, end
        if node.kind == '*':
            assert len(node.children) == 1
            beg, end = self._build(node.children[0])
            self._null(beg, end)
            self._null(end, beg)
            return beg, end
        if node.kind == '+':
            assert len(node.children) == 1
            beg1, end1 = self._build(node.children[0])
            beg2, end2 = self._build(node.children[0])
            self._null(end1, beg2)
            self._null(beg2, end2)
            self._null(end2, beg2)
            return beg1, end2
        beg = self._state()
        end = self._state()
        if node.kind == '.':
            for c in map(chr, range(0x20, 0x7f)):
                self._trans(beg, c, end)
        else:
            self._trans(beg, node.kind, end)
        return beg, end

    def print(self) -> None:
        for state, trans in enumerate(self.states):
            print(f'{state}:')
            for inp, tos in trans.items():
                print(f'  {inp!r} -> {tos}')


DfaState = NewType('DfaState', int)


class Dfa:
    def __init__(self, nfa: Nfa) -> None:
        self.accepts: Set[DfaState] = set()
        self.states: List[Dict[str, DfaState]] = []
        self.mapping: Dict[Tuple[NfaState, ...], DfaState] = {}

        self.beg = self._convert(nfa, {nfa.beg})

    def _trans(self, frm: DfaState, inp: str, to: DfaState) -> None:
        trans = self.states[frm]
        assert inp not in trans
        trans[inp] = to

    # states with null transition expanded
    def _expand(self, nfa: Nfa, states: Set[NfaState]) -> Set[NfaState]:
        res: Set[NfaState] = set()

        def rec(state: NfaState):
            if state in res:
                return
            res.add(state)
            for state in nfa.states[state].get('', set()):
                rec(state)
        for state in states:
            rec(state)
        return res

    def _convert(self, nfa: Nfa, states: Set[NfaState]) -> DfaState:
        states = self._expand(nfa, states)
        key = tuple(sorted(states))
        if key in self.mapping:
            return self.mapping[key]

        dfa_state = DfaState(len(self.states))
        self.states.append({})
        self.mapping[key] = dfa_state
        if nfa.end in states:
            self.accepts.add(dfa_state)

        trans: Dict[str, Set[NfaState]] = {}
        for state in states:
            for inp, outs in nfa.states[state].items():
                get_or(trans, inp, set).update(outs)
        for inp, outs in trans.items():
            if inp == '':
                continue
            next_state = self._convert(nfa, outs)
            self._trans(dfa_state, inp, next_state)
        return dfa_state

    def print(self) -> None:
        for state, trans in enumerate(self.states):
            print(f'{state}:')
            for inp, to in trans.items():
                print(f'  {inp!r} -> {to}')


Regex = Callable[[bytes], int]


def regex(dfa: Dfa) -> Regex:
    src_name = 'jit_regex.c'
    lib_name = './jit_regex.so'
    with open(src_name, 'w') as out:
        assert dfa.beg == 0
        out.write(f'''\
int search(const char *text) {{
    int position = 0;
''')
        for i, trans in enumerate(dfa.states):
            state = DfaState(i)
            accept = 1 if state in dfa.accepts else 0

            out.write(f'''\
state{state}:
    switch (text[position++]) {{
        case '\\0': return {accept};
''')
            for inp, to in sorted(trans.items()):
                if inp in "'\\":
                    inp = '\\' + inp
                out.write(f'''\
        case '{inp}': goto state{to};
''')
            out.write(f'''\
        default: return 0;
    }}
''')
        out.write(f'''\
}}
''')

    cmd = ['gcc', '-fpic', '-shared', src_name, '-o', lib_name]
    subprocess.run(cmd, check=True)
    lib = ctypes.CDLL(lib_name)
    return lib.search


def main() -> None:
    pat = 'a*(.c)+d?'
    node = Parser.parse(pat)
    nfa = Nfa(node)
    dfa = Dfa(nfa)
    reg = regex(dfa)
    print(reg('bcccdc'.encode()))
    print(reg('blah'.encode()))


if __name__ == '__main__':
    main()
