#!/usr/bin/env python3

import functools
import math


class lazy_property:
    def __init__(self, fget):
        self.fget = fget
        functools.update_wrapper(self, fget)

    def __get__(self, obj, cls):
        if obj is None:
            return self
        val = self.fget(obj)
        setattr(obj, self.fget.__name__, val)
        return val


class Vec:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    @lazy_property
    def length(self):
        print('length')
        return math.sqrt(self.x * self.x + self.y * self.y)


v = Vec(3, 4)
print(v.__dict__)
print(v.length)
print(v.__dict__)
print(v.length)
print(v.__dict__)
