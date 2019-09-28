#!/usr/bin/env python3

import operator
import functools
from typing import Generic, Iterator, Sequence, TypeVar, Union, overload

T = TypeVar('T')


class SpanIter(Generic[T], Iterator[T]):
    def __init__(self, sequence: Sequence[T], indices: Iterator[int]) -> None:
        self.sequence = sequence
        self.indices = indices

    def __next__(self) -> T:
        return self.sequence[next(self.indices)]

    def __length_hint__(self) -> int:
        return operator.length_hint(self.indices, NotImplemented)


@functools.total_ordering
class Span(Generic[T], Sequence[T]):
    def __init__(
            self,
            sequence: Sequence[T],
            indices: Sequence[int] = None,
    ) -> None:
        self.sequence = sequence
        self.indices = range(len(sequence)) if indices is None else indices

    def __str__(self) -> str:
        return '<{}>'.format(', '.join(map(repr, self)))

    def __repr__(self) -> str:
        return '{}({!r}, indices={!r})'.format(
            self.__class__.__name__,
            self.sequence,
            self.indices,
        )

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Span):
            return False
        return len(self) == len(other) and all(map(operator.eq, self, other))

    def __lt__(self, other: object) -> bool:
        if not isinstance(other, Span):
            return False
        for a, b in zip(self, other):
            if a != b:
                return a < b
        return len(self) < len(other)

    @overload
    def __getitem__(self, index: int) -> T:
        ...

    @overload
    def __getitem__(self, index: slice) -> 'Span[T]':
        ...

    def __getitem__(self, index: Union[int, slice]) -> 'Union[T, Span[T]]':
        if isinstance(index, int):
            return self.sequence[self.indices[index]]
        if isinstance(index, slice):
            return Span(self.sequence, self.indices[index])
        raise TypeError()

    def __len__(self) -> int:
        return len(self.indices)

    def __iter__(self) -> SpanIter[T]:
        return SpanIter(self.sequence, iter(self.indices))

    def __reversed__(self) -> SpanIter[T]:
        return SpanIter(self.sequence, reversed(self.indices))


def speed_test():
    l = Span(list(range(100_000)))
    for _ in range(len(l) - 10):
        l = l[1:]
    print(l)


def main() -> None:
    l = Span(range(50))
    s = l[1:60:2]
    print(s)
    t = s[40:10:-3]
    print(t)
    print(t[-2])
    print(', '.join(map(str, reversed(t))))
    print(repr(t))
    print(t > t[:-1])

    speed_test()


if __name__ == '__main__':
    main()
