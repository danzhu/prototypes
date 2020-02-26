#!/usr/bin/env python3

from __future__ import annotations
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, Iterable, Iterator, List, Optional, Tuple, Union
import ast
import re
import shlex
import textwrap


@dataclass
class Record:
    INDENT = '  '

    subject: Optional[Conf] = None
    args: List[Conf] = field(default_factory=list)
    fields: Dict[str, List[Record]] = field(default_factory=dict)

    def _print_args(self, indent: int, line: bool) -> Iterator[str]:
        for i, arg in enumerate(self.args):
            if i > 0:
                yield ' '
            if isinstance(arg, str):
                yield format_str(arg)
            elif isinstance(arg, Record):
                yield '('
                yield from arg._print_args(indent, line=False)
                yield ')'
            else:
                assert False, type(arg)
        if self.subject is not None:
            assert len(self.fields) == 0, 'has subject and fields'
            yield ' | '
            if isinstance(self.subject, str):
                assert line, 'inline str subject'
                yield ' :\n'
                yield textwrap.indent(self.subject, self.INDENT)
            elif isinstance(self.subject, Record):
                yield from self.subject._print_args(indent, line)
            else:
                assert False, type(self.subject)
        elif line:
            yield '\n'
            yield from self._print(indent=indent + 1)
        else:
            assert len(self.fields) == 0, 'inline conf with fields'

    def _print(self, indent: int) -> Iterator[str]:
        ind = self.INDENT * indent
        for name, values in self.fields.items():
            for value in values:
                yield f'{ind}{format_str(name)} '
                yield from value._print_args(indent, line=True)

    def display(self) -> None:
        print(''.join(self._print(indent=0)), end='')


def format_str(s: str) -> str:
    # FIXME: this doesn't match parser syntax
    return shlex.quote(s)


Conf = Union[str, Record]

TOKEN_REGEX = re.compile(r"""'[^']*'|"(?:\\.|[^"])*"|[^\s()|'"]+|[()|]""")


class Parser:
    line: Optional[str]
    indent: int
    indents = [-1]

    tokens: Iterator[re.Match[str]]
    token: Optional[str]

    def __init__(self, lines: Iterable[str]) -> None:
        self.line_iter = iter(lines)

    def next_token(self) -> None:
        self.token = next(self.tokens, [None])[0]

    def parse_item(self) -> Record:
        args: List[Conf] = []
        while True:
            if self.token is None:
                fields = self.parse_block()
                return Record(args=args, fields=fields)
            elif self.token.startswith('"'):
                args.append(ast.literal_eval(self.token))
            elif self.token == '(':
                self.next_token()
                item = self.parse_item()
                args.append(item)
                if self.token != ')':
                    raise Exception('expect closing parenthesis')
            elif self.token == ')':
                return Record(args=args)
            elif self.token == '|':
                self.next_token()
                item = self.parse_item()
                return Record(args=args, subject=item)
            else:
                args.append(self.token)
            self.next_token()

    def parse_line(self, line: str) -> Tuple[str, Record]:
        self.tokens = TOKEN_REGEX.finditer(line)
        self.next_token()
        name = self.token
        assert name is not None, 'empty line'
        self.next_token()
        val = self.parse_item()
        return name, val

    def next_line(self) -> None:
        self.line = next(self.line_iter, None)
        if self.line is None:
            return
        stripped = self.line.lstrip()
        if stripped == '' or stripped.startswith('#'):
            return self.next_line()
        self.indent = len(self.line) - len(stripped)

    def parse_block(self) -> Dict[str, List[Record]]:
        fields: Dict[str, List[Record]] = {}
        while self.line is not None and self.indent > self.indents[-1]:
            self.indents.append(self.indent)
            line = self.line
            self.next_line()
            name, val = self.parse_line(line)
            self.indents.pop()
            fields.setdefault(name, []).append(val)
        return fields

    @staticmethod
    def parse(lines: Iterable[str]) -> Record:
        parser = Parser(lines)
        parser.next_line()
        fields = parser.parse_block()
        return Record(fields=fields)


def main():
    main_f = Path(__file__)

    conf_f = main_f.with_suffix('.conf')
    with conf_f.open() as f:
        conf = Parser.parse(f)
    conf.display()


if __name__ == '__main__':
    main()
