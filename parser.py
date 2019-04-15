#!/usr/bin/env python3

from typing import Callable, Tuple, TypeVar, Generic, Optional

T = TypeVar('T')
U = TypeVar('U')


class Parser(Generic[T]):
    def __init__(self, f: Callable[[str], Optional[Tuple[T, str]]]) -> None:
        self.f = f

    def __call__(self, s: str) -> Optional[Tuple[T, str]]:
        return self.f(s)

    def __or__(self, p: 'Parser[T]') -> 'Parser[T]':
        def parse(s: str) -> Optional[Tuple[T, str]]:
            r1 = self(s)
            if r1 is not None:
                return r1
            return p(s)
        return Parser(parse)

    def __rshift__(self, p: Callable[[T], 'Parser[U]']) -> 'Parser[U]':
        def parse(s: str) -> Optional[Tuple[U, str]]:
            r1 = self(s)
            if r1 is None:
                return None
            a, s2 = r1
            return p(a)(s2)
        return Parser(parse)


single = Parser(lambda s: (s[0], s[1:]))


def char(c: str) -> Parser[str]:
    def parse(s: str) -> Optional[Tuple[str, str]]:
        if s[0] == c:
            return c, s[1:]
        return None
    return Parser(parse)


parse_item = single
