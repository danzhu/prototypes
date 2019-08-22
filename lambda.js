#!/usr/bin/env node

const eager = v => ({v})
const delay = f => ({f})
const force = c => 'v' in c ? c.v : (c.v = c.f())

const _trace = eager(_a => _b => {
    console.log(force(_a))
    return force(_b)
})

const _true = eager(_a => _b => force(_a))
const _false = eager(_a => _b => force(_b))
const _if = eager(_c => _t => _f => force(_c)(_t)(_f))

const _eq = eager(_a => _b => force(force(_a) === force(_b) ? _true : _false))
const _add = eager(_a => _b => force(_a) + force(_b))
const _mult = eager(_a => _b => force(_a) * force(_b))

const _fact = eager(_n => force(_if)(
    delay(() => force(_eq)(eager(0))(
        delay(() => force(_trace)(_n)(_n))
    ))
)(
    eager(1)
)(
    delay(() => force(_mult)(_n)(
        delay(() => force(_fact)(
            delay(() => force(_add)(_n)(delay(() => -1)))
        ))
    ))
))

console.log(force(_fact)(eager(10)))

const _cons = eager(_h => _t => [_h, _t])
const _head = eager(_l => force(force(_l)[0]))
const _tail = eager(_l => force(force(_l)[1]))
const _null = eager(null)

const _foldr = eager(_f => _i => _l => force(_if)(
    delay(() => force(_eq)(_null)(_l))
)(
    _i
)(
    delay(() => force(_f)(
        delay(() => force(_head)(_l))
    )(
        delay(() => force(_foldr)(_f)(_i)(
            delay(() => force(_tail)(_l))
        ))
    ))
))

const _range = eager(_a => _b => force(_if)(
    delay(() => force(_eq)(_a)(_b))
)(
    _null
)(
    delay(() => force(_cons)(_a)(
        delay(() => force(_range)(
            delay(() => force(_add)(_a)(eager(1)))
        )(
            _b
        ))
    ))
))

console.log(force(_foldr)(_add)(eager(0))(
    delay(() => force(_range)(eager(1))(eager(10)))
))
