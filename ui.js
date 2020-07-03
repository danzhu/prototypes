const log = (msg, tar) => {
    console.log(`${msg}: ${tar.name} =`, tar.build ? tar.build.val : '<handle>')
}

const Context = (init = []) => {
    const stack = Array.from(init)

    const local = (target, f) => {
        stack.push(target)
        try {
            return f()
        } finally {
            const t = stack.pop()
            if (t !== target)
                throw new Error('mismatched stack')
        }
    }

    const top = () => {
        if (stack.length === 0)
            throw new Error('empty stack')
        return stack[stack.length - 1]
    }

    return { local, top }
}

const State = () => ({
    kind: 'idle',
    targets: Context(),
    pend_build: [],
    changed: null,
})

const Target = (state, name, { update, destroy, write }) => {
    const target = { name, build: null }

    if (update !== undefined) {
        target.build = { outdated: true, deps: new Set(), affs: new Set() }

        target.untracked_get = () => target.build.val

        target.get = () => {
            if (state.kind === 'build') {
                target.update()
                const active = state.targets.top()
                active.build.deps.add(target)
                target.build.affs.add(active)
            }
            return target.untracked_get()
        }

        target.update = () => {
            const ch = state.changed.get(target)
            if (ch !== undefined) {
                if (ch === 'changing')
                    throw new Error('dependency cycle detected')
                return ch === 'changed'
            }
            log('checking', target)
            state.changed.set(target, 'changing')
            if (
                target.build.outdated
                    || Array.from(target.build.deps).some(dep => dep.update())
            ) {
                for (const dep of target.build.deps)
                    dep.build.affs.delete(target)

                log('updating', target)
                target.build.val = state.targets.local(target, update)
                target.build.outdated = false

                log('updated', target)
                state.changed.set(target, 'changed')

                run_build(state)
                for (const aff of target.build.affs)
                    aff.update()
                return true
            } else {
                log('up to date', target)
                state.changed.set(target, 'unchanged')
                return false
            }
        }

        state.pend_build.push(target)
    }

    if (write !== undefined) {
        if (typeof write !== 'function')
            throw new Error('invalid write')

        target.set = val => {
            if (state.kind === 'build')
                throw new Error('cannot write when building')
            const clean = state.kind === 'idle'
            if (clean) {
                console.log('entering write')
                state.kind = 'write'
            }

            log('writing', target)
            write(val)
            if (target.build !== null) {
                target.build.outdated = true
                state.pend_build.push(target)
            }

            if (clean) {
                console.log('exiting write')
                state.kind = 'idle'
                run_build(state)
            }
        }
    }

    target.destroy = () => {
        log('destroying', target)
        if (target.build === null)
            return
        if (target.build.affs.size !== 0)
            throw new Error('destroying target in use')
        for (const dep of target.build.deps)
            dep.build.affs.delete(target)
        if (destroy !== undefined)
            destroy()
    }

    log('created', target)
    return target
}

const run_build = state => {
    if (state.kind === 'write')
        throw new Error('cannot build when writing')
    const clean = state.kind === 'idle'
    if (clean) {
        console.log('entering build')
        state.kind = 'build'
        state.changed = new Map()
    }

    const pend = state.pend_build
    state.pend_build = []
    for (const target of pend)
        target.update()

    if (state.pend_build.length !== 0)
        throw new Error('unfinished build')
    if (clean) {
        console.log('exiting build')
        state.kind = 'idle'
        state.changed = null
    }
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

const UiContext = () => ({ arenas: Context([Arena()]), state: State() })

const run = context => run_build(context.state)

const defer = (context, f) => context.arenas.top().defer(f)

const new_target = (context, name, opts) => {
    const target = Target(context.state, name, opts)
    defer(context, () => { target.destroy() })
    return target
}

const stream = (context, name, build, write) => {
    let arena = null
    const update = () => {
        if (arena !== null)
            arena.destroy()
        arena = Arena()
        return context.arenas.local(arena, build)
    }
    const destroy = () => { arena.destroy() }
    return new_target(context, name, { update, destroy, write })
}

const collection = (context, name, build) => {
    let arena = null
    let cache = new Map()
    const update = () => {
        const new_cache = new Map()
        const reuse = (key, val, f) => {
            if (new_cache.has(key))
                throw new Error('repeated keys')
            let entry = cache.get(key)
            if (entry === undefined) {
                const arena = Arena()
                const ret = context.arenas.local(arena, () => {
                    const tar = stream(context, `key/${key}`, () => {
                        // HACK: force update when target changed,
                        // required since we are accessing internal state `cache`
                        target.get()
                        return cache.get(key).val
                    })
                    return f(tar)
                })
                entry = { arena, ret }
            }
            new_cache.set(key, { ...entry, val })
            return entry.ret
        }

        if (arena !== null)
            arena.destroy()
        arena = Arena()
        const ret = context.arenas.local(arena, () => build(reuse))

        for (const [key, { arena }] of cache.entries()) {
            if (!new_cache.has(key))
                arena.destroy()
        }
        cache = new_cache
        return ret
    }
    const destroy = () => {
        for (const { arena } of cache.values())
            arena.destroy()
        arena.destroy()
    }
    const target = new_target(context, name, { update, destroy })
    return target
}

const handle = (context, name, write) => new_target(context, name, { write })

const pure = (context, val) => stream(context, 'pure', () => val)

const mut = (context, name, val) => stream(context, name, () => val, v => { val = v })


// === example ===

const submit = ({ id, code, lang }, cb) => {
    setTimeout(cb, 1000, `test results for ${id} ${lang}:\n${code}`)
}

const element = (ctx, tag, { value, children } = {}) => {
    const elem = document.createElement(tag)
    if (value !== undefined) {
        stream(ctx, 'value', () => elem.value = value.get())
        elem.addEventListener('change', () => { value.set(elem.value) })
    }
    if (children !== undefined) (
        stream(ctx, 'children', () => {
            for (const c of children.get())
                elem.appendChild(c)
            defer(ctx, () => {
                while (elem.firstChild)
                    elem.removeChild(elem.lastChild)
            })
        })
    )
    return elem
}

const Text = (ctx, text) => {
    const elem = document.createTextNode('')
    stream(ctx, 'textContent', () => elem.textContent = text.get())
    return [elem]
}

const Html = (ctx, html) => {
    const elem = element(ctx, 'section')
    stream(ctx, 'innerHTML', () => elem.innerHTML = html.get())
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
    const children = stream(ctx, 'options', () => options.get().map(({ id, name }) => {
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
        Container(ctx, {}, stream(ctx, 'tabs', () => tabs.get().flatMap(({ name }, i) => {
            const action = handle(ctx, 'select_tab', () => index.set(i))
            return Button(ctx, { action }, pure(ctx, Text(ctx, name)))
        }))),
        Container(ctx, {}, stream(ctx, 'active_tab', () => tabs.get()[index.get()].content)),
    ].flat()))
}

const Problem = (ctx, { id, code, solution, langs }) => {
    const lang = mut(ctx, 'lang', langs.untracked_get()[0].id)
    const results = mut(ctx, 'run_result', 'run to see test results')
    const edit = stream(
        ctx, 'code',
        () => code.get()[lang.get()],
        v => { code.set({ ...code.get(), [lang.get()]: v }) },
    )
    return Container(ctx, {}, pure(ctx, [
        Tabs(ctx, { index: mut(ctx, 'tab_index', 0) }, pure(ctx, [{
            name: pure(ctx, 'Editor'),
            content: [
                Editor(ctx, { text: edit }),
                Button(ctx, {
                    action: handle(ctx, 'run', () => {
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
                text: stream(ctx, 'solution', () => solution.get()[lang.get()]),
            }),
        }])),
        Select(ctx, { value: lang, options: langs }),
    ].flat()))
}

const Main = (ctx, { content, langs }) => {
    return Container(ctx, {}, collection(ctx, 'sections', reuse => content.get().flatMap(c => {
        switch (c.kind) {
        case 'html':
            return Html(ctx, pure(ctx, c.text))
        case 'problem':
            return reuse(c.id, c, p => Problem(ctx, {
                id: c.id,
                code: mut(ctx, 'codes', c.starter),
                solution: stream(ctx, 'solutions', () => p.get().solution),
                langs,
            }))
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
        kind: 'html',
        text: '<h1>Title</h1><p>content</p>',
    }, {
        kind: 'problem',
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
    const ctx = UiContext()
    const content = mut(ctx, 'content', DATA.content)
    const langs = mut(ctx, 'languages', DATA.languages)

    for (const elem of Main(ctx, { content, langs }))
        document.body.appendChild(elem)
    run(ctx)

    return { ctx, content, langs }
}
const m = main()

// TODO:
// inactive streams
