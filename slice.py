#!/usr/bin/env python3

import operator


class Slice:
    def __init__(self, arr):
        if not isinstance(arr, list):
            raise TypeError()
        self._arr = arr
        self._start = 0
        self._stop = len(arr)

    def __contains__(self, key):
        for i in range(self._start, self._stop):
            if self._arr[i] == key:
                return True
        return False

    def __eq__(self, value):
        return all(map(operator.eq, self, value))
