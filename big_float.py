#!/usr/bin/env python3

from __future__ import annotations
from collections import deque
from dataclasses import dataclass
from typing import Deque, Generic, List, Sequence, TypeVar, Union

T = TypeVar('T')


# class Arr(Generic[T]):
#     def __init__(self, default: T) -> None:
#         self.default = default
#         self.items = Deque[T]()
#         self.start = 0

#     @property
#     def stop(self) -> int:
#         return self.start + len(self.items)

#     def __getitem__(self, index: int) -> T:
#         i = index - self.start
#         if i not in range(len(self.items)):
#             return self.default
#         return self.items[i]

#     def __setitem__(self, index: int, item: T) -> None:
#         if index < self.start:
#             self.items.extendleft(self.default for _ in range(self.start - index))
#             self.start = index
#         elif index >= self.stop:
#             # + 1 since `self[index]` must be in range
#             self.items.extend(self.default for _ in range(index - self.stop + 1))

#         self.items[index - self.start] = item


# class BigFloat:
#     RADIX = 256

#     def __init__(self, value: Union[int] = None) -> None:
#         self.digits = Arr(default=0)

#         if value is None:
#             return
#         if isinstance(value, int):
#             self.digits[0] = value
#         self.normalize()

#     def __str__(self) -> str:
#         return '{} : {}'.format(
#             ' '.join(f'{d:02x}' for d in reversed(self.digits.items)),
#             self.digits.start,
#         )

#     def __add__(self, other: BigFloat) -> BigFloat:
#         r = BigFloat()
#         for f in (self, other):
#             d = f.digits
#             for i in range(d.start, d.stop):
#                 r.digits[i] += d[i]
#         r.normalize()
#         return r

#     def normalize(self) -> None:
#         d = self.digits
#         for i in range(d.start, d.stop):
#             c, d[i] = divmod(d[i], self.RADIX)
#             d[i + 1] += c


class BigFloat:
    def __init__(self, mant: int, prec: int = 0) -> None:
        if mant != 0:
            while True:
                d, r = divmod(mant, 2)
                if r != 0:
                    break
                mant = d
                prec -= 1

        self.mant = mant
        self.prec = prec

    @staticmethod
    def from_expo(mant: int, expo: int) -> BigFloat:
        return BigFloat(mant=mant, prec=mant.bit_length() - expo)

    def with_prec(self, prec: int) -> int:
        return self.mant << (prec - self.prec)

    @property
    def expo(self) -> int:
        """how many bits before the decimal point"""
        return self.mant.bit_length() - self.prec

    def __repr__(self) -> str:
        return f'{self.mant}:{self.prec}'

    def __str__(self) -> str:
        return f'0.{self.mant:x}^{self.expo:x}'

    def __add__(self, other: BigFloat) -> BigFloat:
        prec = max(self.prec, other.prec)
        mant = self.with_prec(prec) + other.with_prec(prec)
        return BigFloat(mant, prec)


def main() -> None:
    a = BigFloat(0x123456)
    b = BigFloat(0x789abc)
    c = a + b
    print(a)
    print(b)
    print(c)


if __name__ == '__main__':
    main()
