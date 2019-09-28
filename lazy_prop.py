#!/usr/bin/env python3

from typing import (
    Callable, Generic, Optional, Type, TypeVar, Union, cast, overload
)
import functools
import math


T = TypeVar('T')
U = TypeVar('U')


class lazy_property(Generic[T, U]):
    def __init__(self, get: Callable[[T], U]) -> None:
        self.__get = get
        functools.update_wrapper(cast(Callable[[T], U], self), get)

    @overload
    def __get__(self, obj: None, cls: Type[T]) -> 'lazy_property[T, U]': ...
    @overload
    def __get__(self, obj: T, cls: Type[T]) -> U: ...

    def __get__(
            self,
            obj: Optional[T],
            cls: Type[T] = None,
    ) -> 'Union[lazy_property[T, U], U]':
        if obj is None:
            return self
        val = self.__get(obj)
        setattr(obj, self.__get.__name__, val)
        return val


class Vec:
    def __init__(self, x: float, y: float) -> None:
        self.x = x
        self.y = y

    @lazy_property
    def length(self) -> float:
        print('length')
        return math.sqrt(self.x * self.x + self.y * self.y)


def main() -> None:
    v = Vec(3., 4.)
    print(v.__dict__)
    print(v.length + 0)
    print(v.__dict__)
    print(v.length)
    print(Vec.length)
    print(v.__dict__)


if __name__ == '__main__':
    main()
