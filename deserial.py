#!/usr/bin/env python3

from __future__ import annotations

from contextlib import contextmanager
from dataclasses import dataclass
from functools import lru_cache
from pathlib import Path
from typing import (Callable, Dict, Generic, Iterator, List, Optional, Set,
                    Type, TypeVar, Union, overload, Iterable, Tuple)

import yaml

T = TypeVar('T')
K = TypeVar('K')


class DataError(Exception):
    def __init__(self, msg: str, loc: Loc) -> None:
        super().__init__(msg)
        self.loc = loc
        self.ctx: List[str] = []

    @staticmethod
    @contextmanager
    def context(text: str) -> Iterator[None]:
        try:
            yield
        except DataError as e:
            e.ctx.append(text)
            raise

    def pretty(self) -> str:
        return '\n'.join([
            '-' * 80,
            'Context (most recent event last):',
            *(f'  {c}' for c in reversed(self.ctx)),
            f'{self.loc}: {self}',
        ])


@dataclass
class Loc:
    file: Path
    path: List[object]

    def __str__(self) -> str:
        path = '.'.join(map(str, self.path))
        return f'{self.file}:.{path}'

    def __truediv__(self, seg: object) -> Loc:
        return Loc(self.file, self.path + [seg])


@dataclass
class MissingType:
    pass


Missing = MissingType()


@dataclass(frozen=True)
class Raw:
    loc: Loc
    data: object

    @property
    def missing(self) -> bool:
        return self.data is Missing

    def _as(self, cls: Type[T], opt: bool = False) -> T:
        if not isinstance(self.data, cls):
            if opt:
                return cls()
            exp = cls.__name__
            found = 'nothing' if self.missing else type(self.data).__name__
            raise DataError(f'expected {exp}, found {found}', self.loc)
        return self.data

    def __bool__(self) -> bool:
        return self._as(bool)

    def __str__(self) -> str:
        return self._as(str)

    def __int__(self) -> int:
        return self._as(int)

    def __float__(self) -> float:
        return self._as(float)

    def list(self) -> Iterable[Raw]:
        data: List[object] = self._as(list, opt=True)
        return (Raw(self.loc / i, v) for i, v in enumerate(data))

    def dict(self) -> Iterable[Tuple[Raw, Raw]]:
        data: Dict[object, object] = self._as(dict, opt=True)
        return (
            (Raw(self.loc / k / '<key>', k), Raw(self.loc / k, v))
            for k, v in data.items()
        )


class RawDict(Dict[K, Raw]):
    def __init__(self, raw: Raw, mk_key: Maker[K]) -> None:
        super().__init__((mk_key(k), v) for k, v in raw.dict())
        self.loc = raw.loc

    def __getitem__(self, key: K) -> Raw:
        if key not in self:
            return Raw(self.loc / key, Missing)
        return super().__getitem__(key)


class LazyDict(Generic[K, T]):
    def __init__(self, data: Dict[K, Raw], mk: Maker[T]) -> None:
        self.data = data
        self.mk = mk

    @lru_cache(None)
    def __getitem__(self, key: K) -> T:
        return self.mk(self.data[key])

    def __contains__(self, key: K) -> bool:
        return key in self.data

    def __len__(self) -> int:
        return len(self.data)


class Struct:
    def __init__(self, raw: Raw) -> None:
        self._raw = RawDict(raw, str)
        self._init()

    def __repr__(self) -> str:
        cls = type(self)
        return f'<{cls.__module__}.{cls.__name__} struct at {self._raw.loc}>'

    def _init(self) -> None:
        pass


class LazyField(Generic[T]):
    def __init__(self, mk: Maker[T], key: str = None) -> None:
        self.mk = mk
        self.key = key

    def __set_name__(self, owner: Type[Struct], name: str) -> None:
        if self.key is None:
            self.key = name
        self.name = name

    @overload
    def __get__(self, obj: None, cls: Type[Struct]) -> LazyField[T]: ...
    @overload
    def __get__(self, obj: Struct, cls: Type[Struct]) -> T: ...

    def __get__(
            self,
            obj: Optional[Struct],
            cls: Type[Struct] = None,
    ) -> Union[LazyField[T], T]:
        if obj is None:
            return self
        assert self.key is not None
        val = self.mk(obj._raw[self.key])
        setattr(obj, self.name, val)
        return val


Maker = Callable[[Raw], T]


def list_of(mk: Maker[T]) -> Maker[List[T]]:
    return lambda raw: list(map(mk, raw.list()))


def set_of(mk: Maker[T]) -> Maker[Set[T]]:
    return lambda raw: set(map(mk, raw.list()))


def dict_of(mk_key: Maker[K], mk_val: Maker[T]) -> Maker[Dict[K, T]]:
    return lambda raw: {mk_key(k): mk_val(v) for k, v in raw.dict()}


def lazy_dict_of(mk_key: Maker[K], mk_val: Maker[T]) -> Maker[LazyDict[K, T]]:
    return lambda raw: LazyDict(RawDict(raw, mk_key), mk_val)


def load_yaml(mk: Maker[T], path: Path) -> T:
    with open(path) as f:
        data: object = yaml.safe_load(f)
    loc = Loc(path, [])
    return mk(Raw(loc, data))


class Executable(Struct):
    dependencies = LazyField(set_of(str))

    def _init(self) -> None:
        self.main = str(self._raw['main'])


class Package(Struct):
    name = LazyField(str)
    dependencies = LazyField(set_of(str))
    ghc_options = LazyField(str, key='ghc-options')
    executables = LazyField(lazy_dict_of(str, Executable))


pkg = load_yaml(Package, Path(__file__).parent / 'package.yaml')
print(f'{pkg=}')
print(f'{pkg.name=}')
print(f'{pkg.dependencies=}')
print(f'{pkg.ghc_options=}')
print(f'{pkg.executables["Action"].main=}')
print(f'{pkg.executables["Generator"].dependencies=}')
try:
    with DataError.context('outer'):
        with DataError.context('inner'):
            print(f'{pkg.executables["oops"]=}')
except DataError as e:
    print(e.pretty())
