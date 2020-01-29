#!/usr/bin/env node

const Functor = {
    class: Symbol('Functor'),
    map: (f, a) => a[Functor.class].map(f, a),
}
const Applicative = {
    class: Symbol('Applicative'),
    pure: (cls, a) => cls[Applicative.class].pure(a),
}
const Show = {
    class: Symbol('Show'),
    show: a => {
        const cls = a[Show.class]
        if (cls !== undefined)
            return cls.show(a)
        return a.toString()
    }
}

const List = {
    cons: (head, tail) => ({
        head,
        tail,
        [Functor.class]: {
            map: (f, a) => List.cons(f(a.head), Functor.map(f, a.tail)),
        },
        [Show.class]: {
            show: a => `${a.head}:${Show.show(a.tail)}`,
        },
    }),
    empty: {
        [Functor.class]: {
            map: (_f, _a) => List.empty,
        },
        [Show.class]: {
            show: _a => '[]',
        },
    },
    [Applicative.class]: {
        pure: a => List.cons(a, List.empty),
    },
}

{
    const { map } = Functor
    const { pure } = Applicative
    const { show } = Show
    const { cons } = List

    const l = cons(1, pure(List, 2))
    const m = map(i => i + 1, l)
    console.log(show(m))
}
