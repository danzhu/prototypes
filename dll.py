#!/usr/bin/env python3

import ctypes
import subprocess


def main():
    with open('dyn.c', 'w') as f:
        print('int neg(int i) { return -i; }', file=f)
    subprocess.run(['gcc', '-fpic', '-c', 'dyn.c'], check=True)
    subprocess.run(['gcc', '-shared', 'dyn.o', '-o', 'dyn.so'], check=True)
    lib = ctypes.CDLL('./dyn.so')
    val = lib.neg(1)
    print(type(val), val)


if __name__ == '__main__':
    main()
