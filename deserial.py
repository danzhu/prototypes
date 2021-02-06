#!/usr/bin/env python3

from __future__ import annotations

from dataclasses import dataclass
from functools import cached_property, partial
from os import PathLike
from pathlib import Path
from typing import (Callable, Dict, Generic, Iterable, Iterator, List, Mapping,
                    Optional, Tuple, Type, TypeVar, Union, overload)

import yaml

K = TypeVar('K')
T = TypeVar('T')
U = TypeVar('U')


class HydrationError(Exception):
    def __init__(self, msg: str, loc: Loc) -> None:
        super().__init__(msg)
        self.loc = loc

    def __str__(self) -> str:
        return f'{self.loc}: {super().__str__()}'


@dataclass(frozen=True)
class Loc:
    file: Path
    path: Tuple[object, ...]

    def __str__(self) -> str:
        path = '.'.join(map(str, self.path))
        return f'{self.file}:.{path}'

    def __truediv__(self, seg: object) -> Loc:
        return Loc(self.file, (*self.path, seg))


@dataclass
class MissingType:
    pass


Missing = MissingType()


@dataclass(frozen=True)
class Raw:
    data: object
    loc: Loc

    @property
    def missing(self) -> bool:
        return self.data is Missing

    def _as(self, cls: Type[T], opt: bool = False) -> T:
        if not isinstance(self.data, cls):
            if opt:
                return cls()
            exp = cls.__name__
            found = 'nothing' if self.missing else type(self.data).__name__
            raise HydrationError(f'expected {exp}, found {found}', self.loc)
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
        return (Raw(v, self.loc / i) for i, v in enumerate(data))

    def dict(self) -> Iterable[Tuple[Raw, Raw]]:
        data: Dict[object, object] = self._as(dict, opt=True)
        return (
            (Raw(k, self.loc / k / '<key>'), Raw(v, self.loc / k))
            for k, v in data.items()
        )


class RawDict(Dict[K, Raw]):
    def __init__(self, raw: Raw, mk_key: Maker[K]) -> None:
        super().__init__((mk_key(k), v) for k, v in raw.dict())
        self.loc = raw.loc

    def __getitem__(self, key: K) -> Raw:
        if key not in self:
            return Raw(Missing, self.loc / key)
        return super().__getitem__(key)


class Hydrated:
    _loc: Loc

    def __class_getitem__(cls, args):
        return partial(cls, args=args)


class Opt(Hydrated, Generic[T]):
    def __init__(self, raw: Raw, *, args: Maker[T] = None) -> None:
        assert args is not None
        self.mk = args
        self.raw = raw
        self._loc = raw.loc

    def __bool__(self) -> bool:
        return not self.raw.missing

    @cached_property
    def value(self) -> T:
        return self.mk(self.raw)


class Arr(Hydrated, List[T]):
    def __init__(self, raw: Raw, *, args: Maker[T] = None) -> None:
        assert args is not None
        super().__init__(args(a) for a in raw.list())
        self._loc = raw.loc


class Map(Hydrated, Dict[K, T]):
    def __init__(
            self,
            raw: Raw,
            *,
            args: Tuple[Maker[K], Maker[T]] = None,
    ) -> None:
        assert args is not None
        mk_key, mk_val = args
        super().__init__((mk_key(k), mk_val(v)) for k, v in raw.dict())
        self._loc = raw.loc


class LazyMap(Hydrated, Mapping[K, T]):
    def __init__(
            self,
            raw: Raw,
            *,
            args: Tuple[Maker[K], Maker[T]] = None,
    ) -> None:
        assert args is not None
        mk_key, mk_val = args
        self.src = RawDict(raw, mk_key)
        self.f = mk_val
        self.cache: Dict[K, T] = {}

    def __getitem__(self, key: K) -> T:
        if key not in self.cache:
            self.cache[key] = self.f(self.src[key])
        return self.cache[key]

    def __iter__(self) -> Iterator[K]:
        return iter(self.src)

    def __len__(self) -> int:
        return len(self.src)

    def __contains__(self, key: object) -> bool:
        return key in self.src


@dataclass
class CatMap(Mapping[K, T]):
    a: Mapping[K, T]
    b: Mapping[K, T]

    def __getitem__(self, key: K) -> T:
        return self.a[key] if key in self.a else self.b[key]

    def __iter__(self) -> Iterator[K]:
        return iter(self.a.keys() | self.b.keys())

    def __len__(self) -> int:
        return len(self.a.keys() | self.b.keys())


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


def load_yaml(mk: Maker[T], path: Union[str, PathLike[str]]) -> T:
    p = Path(path)
    with open(p) as f:
        data: object = yaml.safe_load(f)
    loc = Loc(p, ())
    return mk(Raw(data, loc))


class Executable(Struct):
    dependencies = LazyField(Arr[str])

    def _init(self) -> None:
        self.main = str(self._raw['main'])


class Package(Struct):
    name = LazyField(str)
    dependencies = LazyField(Arr[str])
    ghc_options = LazyField(str, key='ghc-options')
    executables = LazyField(LazyMap[str, Executable])


pkg = load_yaml(Package, Path(__file__).parent / 'package.yaml')
print(f'{pkg=}')
print(f'{pkg.name=}')
print(f'{pkg.dependencies=}')
print(f'{pkg.ghc_options=}')
print(f'{pkg.executables["Action"].main=}')
print(f'{pkg.executables["Generator"].dependencies=}')
try:
    print(f'{pkg.executables["oops"]=}')
except HydrationError as e:
    print(e)
