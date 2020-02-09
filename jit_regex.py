#!/usr/bin/env python3

from __future__ import annotations
from dataclasses import dataclass
from typing import (
    Callable,
    Dict,
    FrozenSet,
    Iterable,
    List,
    NewType,
    Optional,
    Set,
    Tuple,
    TypeVar,
)
import ctypes
import string
import subprocess

K = TypeVar('K')
V = TypeVar('V')


class Node:
    def build(self, nfa: Nfa) -> Tuple[Nfa.I, Nfa.I]:
        raise NotImplementedError()


@dataclass
class Single(Node):
    accept: Set[str]

    def build(self, nfa: Nfa) -> Tuple[Nfa.I, Nfa.I]:
        beg = nfa._state()
        end = nfa._state()
        for c in self.accept:
            nfa.add(beg, c, end)
        return beg, end

@dataclass
class Group(Node):
    children: List[Node]

    def build(self, nfa: Nfa) -> Tuple[Nfa.I, Nfa.I]:
        segs = [c.build(nfa) for c in self.children]
        return nfa.connect(segs)


@dataclass
class Repeat(Node):
    child: Node
    start: int
    stop: Optional[int]

    def build(self, nfa: Nfa) -> Tuple[Nfa.I, Nfa.I]:
        segs = [self.child.build(nfa) for _ in range(self.start)]
        if self.stop is None:
            beg, end = rep = self.child.build(nfa)
            nfa.add_null(beg, end)
            nfa.add_null(end, beg)
            segs.append(rep)
        else:
            extra = self.stop - self.start
            segs.extend(self.child.build(nfa) for _ in range(extra))
            assert len(segs) == self.stop
            end = segs[-1][1]
            for i in range(self.start, self.stop):
                nfa.add_null(segs[i][0], end)
        return nfa.connect(segs)


class Parser:
    ESCAPES = {
        'd': set(string.digits),
        's': set(string.whitespace),
        'w': set(string.ascii_letters + string.digits + '_'),
        '\\': {'\\'},
        '.': {'.'},
        '|': {'|'},
        '(': {'('},
        ')': {')'},
        '[': {'['},
        ']': {']'},
    }

    def __init__(self, src: str) -> None:
        self.source = src
        self.position = 0

    def _peek(self) -> str:
        return self.source[self.position]

    def _next(self) -> str:
        self.position += 1
        return self.source[self.position - 1]

    def _expect(self, exp: str) -> None:
        got = self._next()
        if got != exp:
            raise RuntimeError(f'expected {exp!r}, got {got!r}')

    def _eof(self) -> bool:
        return self.position == len(self.source)

    def _set(self) -> Set[str]:
        accept = set()
        while not self._eof() and self._peek() != ']':
            char = self._next()
            if char == '\\':
                accept.update(self.ESCAPES[self._next()])
            else:
                accept.add(char)
        return accept

    def _atom(self) -> Node:
        if self._peek() == '(':
            self._next()
            node = self._group()
            self._expect(')')
            return node
        char = self._next()
        if char == '[':
            accept = self._set()
            self._expect(']')
        elif char == '.':
            accept = set(string.printable) - {'\n'}
        elif char == '\\':
            accept = self.ESCAPES[self._next()]
        else:
            accept = {char}
        return Single(accept)

    def _repeat(self) -> Node:
        node = self._atom()
        if not self._eof() and self._peek() in '?*+':
            kind = self._next()
            start, stop = {
                '?': (0, 1),
                '*': (0, None),
                '+': (1, None),
            }[kind]
            return Repeat(node, start, stop)
        return node

    def _group(self) -> Node:
        items = []
        while not self._eof() and self._peek() != ')':
            items.append(self._repeat())
        return Group(items)

    @staticmethod
    def parse(src: str) -> Node:
        parser = Parser(src)
        node = parser._group()
        if not parser._eof():
            raise RuntimeError('expected end of input')
        return node


def get_or(d: Dict[K, V], k: K, v: Callable[[], V]) -> V:
    if k in d:
        return d[k]
    r = v()
    d[k] = r
    return r


class Nfa:
    I = NewType('I', int)

    class State:
        def __init__(self) -> None:
            self.trans: Dict[str, Set[Nfa.I]] = {}
            self.null: Set[Nfa.I] = set()

    def __init__(self) -> None:
        self.states: List[Nfa.State] = []

    def _state(self) -> Nfa.I:
        s = Nfa.I(len(self.states))
        self.states.append(Nfa.State())
        return s

    def add(self, frm: Nfa.I, inp: str, to: Nfa.I) -> None:
        trans = get_or(self.states[frm].trans, inp, set)
        trans.add(to)

    def add_null(self, frm: Nfa.I, to: Nfa.I) -> None:
        self.states[frm].null.add(to)

    def connect(self, pairs: List[Tuple[Nfa.I, Nfa.I]]) -> Tuple[Nfa.I, Nfa.I]:
        for (_, end), (beg, _) in zip(pairs, pairs[1:]):
            self.add_null(end, beg)
        return pairs[0][0], pairs[-1][1]

    def expand(self, states: Iterable[Nfa.I]) -> FrozenSet[Nfa.I]:
        """
        states with null transitions expanded
        """
        res: Set[Nfa.I] = set()

        def rec(state: Nfa.I) -> None:
            if state in res:
                return
            res.add(state)
            for state in self.states[state].null:
                rec(state)

        for state in states:
            rec(state)
        return frozenset(res)

    def non_null(self, states: Iterable[Nfa.I]) -> Dict[str, Set[Nfa.I]]:
        trans: Dict[str, Set[Nfa.I]] = {}
        for state in states:
            for inp, outs in self.states[state].trans.items():
                get_or(trans, inp, set).update(outs)
        return trans

    def print(self) -> None:
        for state, st in enumerate(self.states):
            print(f'{state}:')
            print(f'  null -> {st.null}')
            for inp, tos in sorted(st.trans.items()):
                print(f'  {inp!r} -> {tos}')


def ast_to_nfa(root: Node) -> Tuple[Nfa, Nfa.I, Nfa.I]:
    nfa = Nfa()
    beg, end = root.build(nfa)
    return nfa, beg, end


class Dfa:
    I = NewType('I', int)

    def __init__(self) -> None:
        self.accepts: Set[Dfa.I] = set()
        self.states: List[Dict[str, Dfa.I]] = []

    def _state(self) -> Dfa.I:
        dfa_state = Dfa.I(len(self.states))
        self.states.append({})
        return dfa_state

    def _trans(self, frm: Dfa.I, inp: str, to: Dfa.I) -> None:
        trans = self.states[frm]
        assert inp not in trans
        trans[inp] = to

    def print(self) -> None:
        for state, trans in enumerate(self.states):
            print(f'{state}:')
            for inp, to in trans.items():
                print(f'  {inp!r} -> {to}')


def nfa_to_dfa(
        nfa: Nfa,
        nfa_beg: Nfa.I,
        nfa_end: Nfa.I,
) -> Tuple[Dfa, Dfa.I]:
    mapping: Dict[FrozenSet[Nfa.I], Dfa.I] = {}
    dfa = Dfa()

    def convert(states: Set[Nfa.I]) -> Dfa.I:
        expanded = nfa.expand(states)
        dfa_state = mapping.get(expanded)
        if dfa_state is not None:
            return dfa_state

        dfa_state = dfa._state()
        mapping[expanded] = dfa_state
        if nfa_end in expanded:
            dfa.accepts.add(dfa_state)

        trans = nfa.non_null(expanded)
        for inp, outs in trans.items():
            next_state = convert(outs)
            dfa._trans(dfa_state, inp, next_state)
        return dfa_state

    dfa_beg = convert({nfa_beg})
    return dfa, dfa_beg


Regex = Callable[[bytes], int]


def codegen(dfa: Dfa, dfa_beg: Dfa.I) -> Regex:
    src_name = 'jit_regex.c'
    lib_name = './jit_regex.so'
    with open(src_name, 'w') as out:
        out.write(f'''\
int fullmatch(const char *text) {{
    int position = 0;
    goto state{dfa_beg};
''')
        for i, trans in enumerate(dfa.states):
            state = Dfa.I(i)

            out.write(f'''\
state{state}:
    switch (text[position++]) {{
''')
            if state in dfa.accepts:
                out.write(f'''\
        case '\\0': return 1;
''')
            for inp, to in sorted(trans.items()):
                lit = r"'\''" if inp == "'" else repr(inp)
                out.write(f'''\
        case {lit}: goto state{to};
''')
            out.write(f'''\
        default: return 0;
    }}
''')
        out.write(f'''\
}}
''')

    cmd = ['gcc', '-fpic', '-shared', '-O2', src_name, '-o', lib_name]
    subprocess.run(cmd, check=True)
    lib = ctypes.CDLL(lib_name)
    fun = lib.fullmatch
    fun.argtypes = [ctypes.c_char_p]
    fun.restype = ctypes.c_int
    return fun


def regex(patt: str) -> Regex:
    root = Parser.parse(patt)
    nfa, nfa_beg, nfa_end = ast_to_nfa(root)
    dfa, dfa_beg = nfa_to_dfa(nfa, nfa_beg, nfa_end)
    return codegen(dfa, dfa_beg)


def main() -> None:
    reg = regex('[ab]*(.c)+d?')
    print(reg(b'abaacd'))
    print(reg(b'bcccdc'))
    print(reg(b'blah'))


if __name__ == '__main__':
    main()
