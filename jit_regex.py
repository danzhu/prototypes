#!/usr/bin/env python3

from __future__ import annotations
from dataclasses import dataclass, field
from pathlib import Path
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

ANY_CHAR = frozenset(string.printable) - {'\n'}
UNESCAPE: Dict[str, FrozenSet[str]] = {
    'd': frozenset(string.digits),
    's': frozenset(string.whitespace),
    'w': frozenset(string.ascii_letters + string.digits + '_'),
    't': frozenset({'\t'}),
    'n': frozenset({'\n'}),
    'r': frozenset({'\r'}),
    'f': frozenset({'\f'}),
    'v': frozenset({'\v'}),
    **{c: frozenset({c}) for c in '()*+.?[\\]{|}'},
}
REP_RANGE = {
    '?': (0, 1),
    '*': (0, None),
    '+': (1, None),
}


def escape_set(accept: FrozenSet[str]) -> str:
    res = ''
    for esc, group in UNESCAPE.items():
        if group <= accept:
            accept -= group
            res += '\\' + esc
    res += ''.join(sorted(accept))
    return res


class Node:
    def build(self, nfa: Nfa) -> Nfa.Span:
        raise NotImplementedError()


@dataclass
class Single(Node):
    accept: FrozenSet[str]

    def __str__(self) -> str:
        if self.accept == ANY_CHAR:
            return '.'
        if len(self.accept) < len(ANY_CHAR) // 2:
            res = escape_set(self.accept)
        else:
            res = '^' + escape_set(ANY_CHAR - self.accept)
        if len(res) == 1 or (len(res) == 2 and res[0] == '\\'):
            return res
        return f'[{res}]'

    def build(self, nfa: Nfa) -> Nfa.Span:
        beg = nfa._state()
        end = nfa._state()
        for c in self.accept:
            nfa.add(beg, c, end)
        return beg, end

@dataclass
class Seq(Node):
    children: List[Node]

    def __str__(self) -> str:
        return ''.join(map(str, self.children))

    def build(self, nfa: Nfa) -> Nfa.Span:
        segs = [c.build(nfa) for c in self.children]
        return nfa.connect(segs)


@dataclass
class Choice(Node):
    children: List[Node]

    def __str__(self) -> str:
        res = '|'.join(map(str, self.children))
        return f'({res})'

    def build(self, nfa: Nfa) -> Nfa.Span:
        segs = [c.build(nfa) for c in self.children]
        beg = nfa._state()
        end = nfa._state()
        for b, e in segs:
            nfa.add_null(beg, b)
            nfa.add_null(e, end)
        return beg, end


@dataclass
class Repeat(Node):
    child: Node
    start: int
    stop: Optional[int]

    def __str__(self) -> str:
        for rep, span in REP_RANGE.items():
            if (self.start, self.stop) == span:
                break
        else:
            if self.start == self.stop:
                rep = f'{{{self.start}}}'
            else:
                start = '' if self.start == 0 else self.start
                stop = '' if self.stop is None else str(self.stop)
                rep = f'{{{start},{stop}}}'
        return f'{self.child}{rep}'

    def build(self, nfa: Nfa) -> Nfa.Span:
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

    def _num(self) -> int:
        num = []
        while self._peek().isdigit():
            num.append(self._next())
        return int(''.join(num))

    def _set(self) -> Set[str]:
        accept: Set[str] = set()
        while not self._eof() and self._peek() != ']':
            char = self._next()
            if char == '\\':
                accept.update(UNESCAPE[self._next()])
            elif self._peek() == '-':
                self._next()
                if self._eof() or self._peek() == ']':
                    accept.update([char, '-'])
                    break
                end = self._next()
                accept.update(map(chr, range(ord(char), ord(end) + 1)))
            else:
                accept.add(char)
        return accept

    def _atom(self) -> Node:
        char = self._next()
        if char == '(':
            node = self._choice()
            self._expect(')')
            return node

        if char == '[':
            if self._peek() == '^':
                self._next()
                accept = ANY_CHAR - self._set()
            else:
                accept = frozenset(self._set())
            self._expect(']')
        elif char == '.':
            accept = ANY_CHAR
        elif char == '\\':
            accept = UNESCAPE[self._next()]
        else:
            accept = frozenset({char})
        return Single(accept)

    def _repeat(self) -> Node:
        node = self._atom()
        if self._eof():
            return node

        if self._peek() in REP_RANGE.keys():
            kind = self._next()
            start, stop = REP_RANGE[kind]
        elif self._peek() == '{':
            self._next()
            start = self._num() if self._peek().isdigit() else 0
            if self._peek() == ',':
                self._next()
                stop = self._num() if self._peek().isdigit() else None
            else:
                stop = start
            self._expect('}')
        else:
            return node

        return Repeat(node, start, stop)

    def _seq(self) -> Node:
        items = []
        while not self._eof() and self._peek() not in '|)':
            items.append(self._repeat())
        return Seq(items)

    def _choice(self) -> Node:
        choices = [self._seq()]
        while not self._eof() and self._peek() == '|':
            self._next()
            choices.append(self._seq())
        return Choice(choices)

    @staticmethod
    def parse(src: str) -> Node:
        parser = Parser(src)
        node = parser._choice()
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
    Span = Tuple[I, I]

    @dataclass
    class State:
        trans: Dict[str, Set[Nfa.I]] = field(default_factory=dict)
        null: Set[Nfa.I] = field(default_factory=set)

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

    def connect(self, pairs: List[Nfa.Span]) -> Nfa.Span:
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


def ast_to_nfa(root: Node) -> Tuple[Nfa, Nfa.Span]:
    nfa = Nfa()
    span = root.build(nfa)
    return nfa, span


class Dfa:
    I = NewType('I', int)

    @dataclass
    class State:
        trans: Dict[str, Dfa.I] = field(default_factory=dict)
        accept = False

    def __init__(self) -> None:
        self.states: List[Dfa.State] = []

    def _state(self) -> Dfa.I:
        dfa_state = Dfa.I(len(self.states))
        self.states.append(Dfa.State())
        return dfa_state

    def _trans(self, frm: Dfa.I, inp: str, to: Dfa.I) -> None:
        state = self.states[frm]
        assert inp not in state.trans
        state.trans[inp] = to

    def print(self) -> None:
        for state, st in enumerate(self.states):
            print(f'{state}:')
            for inp, to in st.trans.items():
                print(f'  {inp!r} -> {to}')


def nfa_to_dfa(
        nfa: Nfa,
        nfa_span: Nfa.Span,
) -> Tuple[Dfa, Dfa.I]:
    nfa_beg, nfa_end = nfa_span
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
            dfa.states[dfa_state].accept = True

        trans = nfa.non_null(expanded)
        for inp, outs in trans.items():
            next_state = convert(outs)
            dfa._trans(dfa_state, inp, next_state)
        return dfa_state

    dfa_beg = convert({nfa_beg})
    return dfa, dfa_beg


Regex = Callable[[bytes], int]


def codegen(dfa: Dfa, beg: Dfa.I) -> Iterable[str]:
    yield f'''\
int fullmatch(const char *text) {{
    int position = 0;
    goto state{beg};
'''
    for i, st in enumerate(dfa.states):
        state = Dfa.I(i)

        yield f'''\
state{state}:
    switch (text[position++]) {{
'''
        if st.accept:
            yield f'''\
    case '\\0': return 1;
'''
        for inp, to in sorted(st.trans.items()):
            lit = r"'\''" if inp == "'" else repr(inp)
            yield f'''\
    case {lit}: goto state{to};
'''
        yield f'''\
    default: return 0;
    }}
'''
    yield f'''\
}}
'''


def regex(patt: str) -> Regex:
    root = Parser.parse(patt)
    print(root)
    nfa, nfa_span = ast_to_nfa(root)
    dfa, dfa_beg = nfa_to_dfa(nfa, nfa_span)
    code = ''.join(codegen(dfa, dfa_beg))

    src = Path('jit_regex.c')
    src.write_text(code)

    lib = src.with_suffix('.so')
    subprocess.run(['gcc', '-fpic', '-shared', '-O2', src, '-o', lib], check=True)
    dll = ctypes.CDLL(str(lib.absolute()))
    fun = dll.fullmatch
    fun.argtypes = [ctypes.c_char_p]
    fun.restype = ctypes.c_int
    return fun


def regex_test() -> None:
    reg = regex(r'[a-b]*(.c|x{3,})+[^\d]?')
    print(reg(b'abaacd'))
    print(reg(b'bcccxxxdc'))
    print(reg(b'blah'))


def speed_test() -> None:
    import re
    r = r'(foo(ba[rz]){2}\d)*'
    reg = regex(r)
    py = re.compile(r)
    s = 'foobarbaz1' * 10_000_000
    b = s.encode()

    print('=== C ===')
    print(reg(b))
    print('=== PYTHON ===')
    print(py.fullmatch(s))


def main():
    speed_test()


if __name__ == '__main__':
    main()
