export interface Resource<T> {
    acquire(): Promise<T>
    release(): void
}

export function promise<T>(): [
    Promise<T>,
    (value?: T | PromiseLike<T>) => void,
    (reason?: any) => void,
] {
    let resolve: ((value?: T | PromiseLike<T>) => void) | undefined
    let reject: ((reason?: any) => void) | undefined
    const prom = new Promise<T>((res, rej) => {
        resolve = res
        reject = rej
    })
    if (resolve === undefined || reject === undefined)
        throw new Error('broken promise implementation')
    return [prom, resolve, reject]
}

export function lock<T, R>(resource: Resource<T>, fn: (value: T) => Promise<R>): Promise<R> {
    return resource.acquire().then(fn).finally(() => resource.release())
}

export function all<T>(ps: Promise<T>[]): Promise<T[]> {
    return new Promise((res, rej) => {
        const r: T[] = Array(ps.length)
        let done = 0
        for (let i = 0; i < ps.length; ++i) {
            ps[i].then(v => {
                r[i] = v
                ++done
                if (done === ps.length)
                    res(r)
            }, rej)
        }
    })
}

export function race<T>(ps: Promise<T>[]): Promise<T> {
    return new Promise((res, rej) => {
        for (const p of ps) {
            p.then(res, rej)
        }
    })
}

export class EofError extends Error {
    constructor() {
        super('EOF reached')
    }
}

export class Sync<T> implements PromiseLike<T> {
    private _value: Promise<T>
    private _resolve: (value?: T | PromiseLike<T>) => void
    private _reject: (reason?: any) => void
    private _status: 'pending' | 'resolved' | 'rejected' = 'pending'

    constructor() {
        [this._value, this._resolve, this._reject] = promise()
    }

    async then<R1 = T, R2 = never>(
        onfulfilled?: ((value: T) => R1 | PromiseLike<R1>) | null | undefined,
        onrejected?: ((reason: any) => R2 | PromiseLike<R2>) | null | undefined,
    ): Promise<R1 | R2> {
        return this._value.then(onfulfilled, onrejected)
    }

    resolve(value?: T | PromiseLike<T>): void {
        if (this._status !== 'pending')
            return
        this._resolve(value)
        this._status = 'resolved'
    }

    reject(reason: any): void {
        if (this._status !== 'pending')
            return
        this._reject(reason)
        this._status = 'rejected'
    }

    get status(): 'pending' | 'resolved' | 'rejected' {
        return this._status
    }
}

export class Wait<T> {
    private wait: Sync<T>[] = []

    async block(): Promise<T> {
        const sync = new Sync<T>()
        this.wait.push(sync)
        return sync
    }

    // returns true if a promise is resolved
    unblock(value: T): boolean {
        const sync = this.wait.shift()
        if (sync === undefined)
            return false
        sync.resolve(value)
        return true
    }

    broadcast(value: T): void {
        for (const sync of this.wait)
            sync.resolve(value)
        this.wait = []
    }

    close(reason: any): void {
        for (const sync of this.wait)
            sync.reject(reason)
        this.wait = []
    }
}

export class Queue<T> implements AsyncIterable<T>, AsyncIterator<T, void> {
    private buffer: T[] = []
    private wait = new Wait<T>()
    private _closed = false
    private _end = new Sync<void>()

    push(...values: T[]): void {
        if (this._closed)
            throw new Error('queue closed')
        for (const value of values) {
            if (!this.wait.unblock(value))
                this.buffer.push(value)
        }
    }

    close(): void {
        if (this._closed)
            throw new Error('already closed')
        this._closed = true
        this.wait.close(new EofError())
        this._end.resolve()
    }

    async pop(): Promise<T> {
        if (this._closed)
            throw new EofError()
        const item = this.buffer.shift()
        if (item !== undefined)
            return item
        return this.wait.block()
    }

    async end(): Promise<void> {
        return this._end
    }

    async next(): Promise<IteratorResult<T, void>> {
        try {
            return { done: false, value: await this.pop() }
        } catch (e) {
            if (e instanceof EofError)
                return { done: true, value: undefined }
            throw e
        }
    }

    [Symbol.asyncIterator](): AsyncIterator<T> {
        return this
    }

    get closed(): boolean { return this._closed }

    get length(): number { return this.buffer.length }
}

export class Lock implements Resource<void> {
    private wait = new Wait<void>()
    private used = false

    async acquire(): Promise<void> {
        if (this.used)
            await this.wait.block()
        else
            this.used = true
    }

    release(): void {
        if (!this.used)
            throw new Error('releasing an unused lock')
        if (!this.wait.unblock())
            this.used = false
    }
}

export class Sem implements Resource<void> {
    private wait = new Wait<void>()

    constructor(private count: number) { }

    async acquire(): Promise<void> {
        if (this.count === 0)
            await this.wait.block()
        else
            --this.count
    }

    release(): void {
        if (!this.wait.unblock())
            ++this.count
    }

    P = this.acquire
    V = this.release
}

export function streamLines(stream: NodeJS.ReadableStream): Queue<string> {
    const queue = new Queue<string>()

    let buffer = ''
    stream.setEncoding('utf8')
    stream.on('data', (chunk: string) => {
        const lines = chunk.split('\n')
        if (lines.length === 0)
            throw 'broken String.prototype.split'
        lines[0] = buffer + lines[0]
        buffer = lines.pop()!
        queue.push(...lines)
    })
    stream.on('end', () => queue.close())

    return queue
}
