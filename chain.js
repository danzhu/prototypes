const funcs = {}
const p = new Proxy({}, {
    get(target, prop, receiver) {
        return function (...args) {
            return funcs[prop](this, ...args)
        }
    },
})
