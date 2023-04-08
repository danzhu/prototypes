#!/usr/bin/env python3

from dataclasses import dataclass
from typing import Any
import jq
import json


@dataclass
class Concat:
    items: list[Any]

    def foo(bar):
        # comment
        bar.baz = bar.baz + 1.23 + __name__


@dataclass
class Quote:
    data: Any


@dataclass
class Unquote:
    code: Any


data = { 'items': [{'name': 'foo'}, {'name': 'bar', 'star': True}] }

'''
item:
{{ .items[] | }[
- {{ .name }}{{ if .star then }[ *]{ else "" end }}
]{ }}
'''
text_templ = Concat([
    'item:\n',
    Unquote(Concat([
        '.items[] | ',
        Quote(Concat([
            '- ',
            Unquote('.name'),
            Unquote(Concat(['if .star then ', Quote(' *'), ' else "" end'])),
            '\n',
        ])),
    ])),
])

'''
<div>
    <p>Items</p>
{{ .items[] | -}
    <input value={{ .name }}/>
{- }}
</div>
'''
xml_templ = {
    'tag': 'div',
    'children': Concat([
        [{ 'tag': 'p', 'children': ['Items'] }],
        Unquote(Concat([
            '.items[] | ',
            Quote([{
                'tag': 'input',
                'attrs': { 'value': Unquote('.name') },
            }]),
        ])),
    ]),
}


def gen_jq(code):
    match code:
        case str():
            return code
        case Concat(items):
            return ''.join(map(gen_jq, items))
        case Quote(data):
            return gen_data(data)
        case _:
            raise ValueError(code)


def gen_data(data):
    match data:
        case None | bool() | int() | float() | str():
            return json.dumps(data)
        case list():
            return '[{}]'.format(', '.join(map(gen_data, data)))
        case dict():
            return '{{{}}}'.format(', '.join(
                '{}: {}'.format(json.dumps(k), gen_data(v))
                for k, v in data.items()
            ))
        case Concat(items):
            return '({})'.format(' + '.join(map(gen_data, items)))
        case Unquote(code):
            return '([{}] | add)'.format(gen_jq(code))
        case _:
            raise ValueError(data)


if __name__ == '__main__':
    for templ in [text_templ, xml_templ]:
        jq_code = gen_data(templ)
        print(jq_code)
        print(jq.compile(jq_code).input(data).first())
