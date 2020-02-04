#!/usr/bin/env python3


def fac(n):
    if n < 2:
        return 1
    return n * (yield n - 1)


def fib(n):
    if n < 2:
        return n
    return (yield n - 1) + (yield n - 2)


def trampoline(func, arg):
    stack = [func(arg)]
    ret = None
    while len(stack) > 0:
        try:
            arg = stack[-1].send(ret)
        except StopIteration as e:
            ret = e.value
            stack.pop()
        else:
            ret = None
            stack.append(func(arg))
    return ret


def main():
    print(trampoline(fac, 10))
    print(trampoline(fib, 10))


if __name__ == '__main__':
    main()
