// TODO:
// helper for state across rebuilds
// inactive streams

const log = (msg, tar) => {
    console.log(`${msg}`, tar.build ? tar.build.val : '<handle>')
}

const State = () => ({ stack: [], written: new Set(), writing: false })

const Target = (state, build = null, write = null) => {
    const target = { build: null, write }
    if (build !== null) {
        target.build = { ...build, deps: new Set(), affs: new Set() }
        target.build.val = with_build(state, target, target.build.create)
        target.get = () => {
            when_build(state, active => {
                active.build.deps.add(target)
                target.build.affs.add(active)
            })
            return target.build.val
        }
    }
    if (target.write !== null) {
        target.set = val => {
            when_build(state, () => {
                throw new Error('cannot set when building')
            })
            log('writing', target)
            with_write(state, target, () => { target.write(val) })
        }
    }
    target.destroy = () => {
        if (target.build === null)
            return
        for (const dep of target.build.deps)
            dep.build.affs.delete(target)
        target.build.destroy()
    }
    return target
}

const when_build = (state, f) => {
    if (state.stack.length === 0)
        return
    f(state.stack[state.stack.length - 1])
}

const with_build = (state, target, f) => {
    state.stack.push(target)
    try {
        return f()
    } finally {
        const t = state.stack.pop()
        if (t !== target)
            throw new Error('mismatched stack')
    }
}

const with_write = (state, target, f) => {
    const clean = !state.writing
    state.writing = true
    f()
    if (target.build !== null)
        state.written.add(target)
    if (clean) {
        state.writing = false
        run_build(state)
    }
}

const run_build = state => {
    const changed = new Map()

    const update = target => {
        const ch = changed.get(target)
        if (ch !== undefined) {
            if (ch === 'changing')
                throw new Error('dependency cycle detected')
            return ch === 'changed'
        }
        log('building', target)
        changed.set(target, 'changing')
        let updated = false
        if (
            state.written.has(target)
                || Array.from(target.build.deps).some(dep => update(dep))
        ) {
            for (const dep of target.build.deps)
                dep.build.affs.delete(target)
            target.build.val = with_build(state, target, target.build.update)
            updated = true
        }
        if (updated) {
            log('updated', target)
            changed.set(target, 'changed')
            for (const aff of target.build.affs)
                update(aff)
        } else {
            log('up to date', target)
            changed.set(target, 'unchanged')
        }
        return updated
    }

    for (const target of state.written)
        update(target)
    state.written = new Set()
}


// === high-level api ===

const Arena = () => {
    const defs = []
    const defer = act => { defs.push(act) }
    const destroy = () => {
        for (const act of defs.reverse())
            act()
    }
    return { defer, destroy }
}

const Context = () => ({ arena: Arena(), state: State() })

const target = (context, build, write) => {
    const target =  Target(context.state, build, write)
    context.arena.defer(() => { target.destroy() })
    return target
}

const stream = (context, read, write = null) => {
    let arena = null
    const create = () => {
        arena = Arena()
        return read({ ...context, arena })
    }
    const destroy = () => { arena.destroy() }
    const update = () => {
        destroy()
        return create()
    }
    const build = { create, update, destroy }
    return target(context, build, write)
}

const handle = (context, write) => target(context, null, write)

const pure = (ctx, val) => stream(ctx, () => val)

const mut = (ctx, val) => stream(ctx, () => val, v => { val = v })


// === example ===

const submit = ({ id, code, lang }, cb) => {
    setTimeout(cb, 1000, `test results for ${id} ${lang}:\n${code}`)
}

const element = (ctx, tag, { value, children } = {}) => {
    const elem = document.createElement(tag)
    if (value !== undefined) {
        stream(ctx, ctx => { elem.value = value.get() })
        elem.addEventListener('change', () => { value.set(elem.value) })
    }
    if (children !== undefined) (
        stream(ctx, ctx => {
            for (const c of children.get())
                elem.appendChild(c)
            ctx.arena.defer(() => {
                while (elem.firstChild)
                    elem.removeChild(elem.lastChild)
            })
        })
    )
    return elem
}

const Text = (ctx, text) => {
    const elem = document.createTextNode('')
    stream(ctx, ctx => { elem.textContent = text.get() })
    return [elem]
}

const Container = (ctx, { tag = 'div' }, children) => {
    return [element(ctx, tag, { children })]
}

const Button = (ctx, { action }, children) => {
    const elem = element(ctx, 'button', { children })
    elem.addEventListener('click', () => { action.set(null) })
    return [elem]
}

const Editor = (ctx, { text }) => {
    return [element(ctx, 'textarea', { value: text })]
}

const Select = (ctx, { value, options }) => {
    const children = stream(ctx, ctx => options.get().map(({ id, name }) => {
        const o = document.createElement('option')
        o.value = id
        o.textContent = name
        return o
    }))
    return [element(ctx, 'select', { value, children })]
}

const Code = (ctx, { text }) => {
    return Container(ctx, { tag: 'pre' }, pure(ctx, Text(ctx, text)))
}

const Tabs = (ctx, { index }, tabs) => {
    return Container(ctx, {}, pure(ctx, [
        Container(ctx, {}, stream(ctx, ctx => tabs.get().flatMap(({ name }, i) => {
            const action = handle(ctx, () => index.set(i))
            return Button(ctx, { action }, pure(ctx, Text(ctx, name)))
        }))),
        Container(ctx, {}, stream(ctx, ctx => tabs.get()[index.get()].content)),
    ].flat()))
}

const Problem = (ctx, { id, code, solution, langs }) => {
    const lang = mut(ctx, langs.get()[0].id)
    const results = mut(ctx, 'run to see test results')
    const edit = stream(
        ctx,
        ctx => code.get()[lang.get()],
        v => { code.set({ ...code.get(), [lang.get()]: v }) },
    )
    return Container(ctx, {}, pure(ctx, [
        Tabs(ctx, { index: mut(ctx, 0) }, pure(ctx, [{
            name: pure(ctx, 'Editor'),
            content: [
                Editor(ctx, { text: edit }),
                Button(ctx, {
                    action: handle(ctx, () => {
                        results.set('running...')
                        const data = { id, code: edit.get(), lang: lang.get() }
                        submit(data, r => { results.set(r) })
                    }),
                }, pure(ctx, Text(ctx, pure(ctx, 'Run')))),
                Code(ctx, { text: results }),
            ].flat(),
        }, {
            name: pure(ctx, 'Solution'),
            content: Code(ctx, {
                text: stream(ctx, ctx => solution.get()[lang.get()]),
            }),
        }])),
        Select(ctx, { value: lang, options: langs }),
    ].flat()))
}

const Main = (ctx, { content, langs }) => {
    return Container(ctx, {}, stream(ctx, ctx => content.get().flatMap(c => {
        switch (c.kind) {
        case 'text':
            return Text(ctx, pure(ctx, c.text))
        case 'code':
            return Problem(ctx, {
                id: c.id,
                code: mut(ctx, c.starter),
                solution: pure(ctx, c.solution),
                langs,
            })
        default:
            throw new Error(`invalid kind ${c.kind}`)
        }
    })))
}

const DATA = {
    languages: [{
        id: 'python',
        name: 'Python',
    }, {
        id: 'javascript',
        name: 'JavaScript',
    }],
    content: [{
        kind: 'text',
        text: 'content',
    }, {
        kind: 'code',
        id: 'print',
        starter: {
            python: `\
print()
`,
            javascript: `\
console.log()
`,
        },
        solution: {
            python: `\
print('content')
`,
            javascript: `\
console.log('content')
`,
        },
    }],
}

const main = () => {
    const ctx = Context()
    const content = pure(ctx, DATA.content)
    const langs = pure(ctx, DATA.languages)

    for (const elem of Main(ctx, { content, langs }))
        document.body.appendChild(elem)
}
main()
