#!/usr/bin/env python3

# resumable exceptions proof-of-concept prototype with `with`, contextvars, and exceptions

from enum import Enum, auto
from dataclasses import dataclass
from contextlib import contextmanager
from contextvars import ContextVar


class Resume(Enum):
    RESUME = auto()
    RERAISE = auto()
    HANDLED = auto()


handlers_var = ContextVar("handlers", default={})


def throw(e):
    handlers = handlers_var.get()
    handler = handlers.get(type(e))
    if handler is None:
        raise e


@contextmanager
def catch(cls, f):
    handlers = handlers_var.get()
    token = handlers_var.set(handlers | {cls: f})
    try:
        yield
    except cls as e:
        if e.abort:
            raise
    finally:
        handlers_var.reset(token)


@dataclass()
class FooError(Exception):
    fatal: bool


def main():
    def handle(e):
        print("got exception:", e)
        return not e.fatal

    with catch(FooError, handle):
        print("non-fatal")
        throw(FooError(fatal=False))
        print("fatal")
        throw(FooError(fatal=True))
        print("unreachable")
