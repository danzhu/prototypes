export interface Obj<T> {
    [key: string]: T
}

export type Json = number | string | boolean | null | JsonArray | JsonObject
export interface JsonArray extends Array<Json> { }
export interface JsonObject extends Obj<Json> { }

export interface Schema<T> {
    serialize(value: T): Json
    deserialize(data: Json): T
}

export class FormatError extends Error {
    constructor(msg: string, got: Json) {
        super(`${msg}, got: ${JSON.stringify(got, null, 2)}`)
    }
}

export type PropSchema<T> = {
    [P in keyof T]: Schema<T[P]>
}

export type Tagged<T> = { [P in keyof T]: [P, T[P]] }[keyof T]

export type Visitor<T, R> = {
    [P in keyof T]: (val: T[P]) => R
}

// TODO: make implementation type check without type assertions
function mapObject<T, U>(obj: T, fn: <P extends (keyof T & keyof U)>(val: T[P], key: P) => U[P]): U {
    const res = {} as any
    for (const key in obj)
        res[key] = fn(obj[key], key as any)
    return res
}

function getOwnProperty<T, P extends keyof T>(obj: T, key: P): T[P] | undefined {
    if (!Object.prototype.hasOwnProperty.call(obj, key))
        return
    return obj[key]
}

// TODO: make implementation type check without type assertions
export class SumOf<T> implements Schema<Tagged<T>> {
    constructor(public variants: PropSchema<T>) { }

    serialize([tag, val]: Tagged<T>): JsonArray {
        return [tag as Json, this.variants[tag].serialize(val)]
    }

    deserialize(data: Json): Tagged<T> {
        if (!Array.isArray(data) || data.length !== 2)
            throw new FormatError('expect array of length 2', data)
        const [tag, val] = data as [keyof T & Json, Json]
        const variant = getOwnProperty(this.variants, tag)
        if (variant === undefined)
            throw new FormatError('unknown variant ${tag}', data)
        return [tag, variant.deserialize(val)]
    }

    // method instead of standalone function because typescript can't infer `T`
    // without annotations on the visitor
    visit<R = void>([tag, val]: Tagged<T>, visitor: Visitor<T, R>): R {
        return visitor[tag](val)
    }
}

export class ProdOf<T> implements Schema<T> {
    constructor(public properties: PropSchema<T>) { }

    serialize(value: T): JsonObject {
        return mapObject(this.properties, (prop, key) => prop.serialize(value[key]))
    }

    deserialize(data: Json): T {
        if (typeof data !== 'object' || data === null || Array.isArray(data))
            throw new FormatError('expect object', data)
        return mapObject(this.properties, (prop, key) => {
            const val = getOwnProperty(data, key as any)
            if (val === undefined)
                throw new FormatError(`missing key ${key}`, data)
            return prop.deserialize(val)
        })
    }
}

export class ObjectOf<T> implements Schema<Obj<T>> {
    constructor(public element: Schema<T>) { }

    serialize(value: Obj<T>): JsonObject {
        return mapObject(value, val => this.element.serialize(val))
    }

    deserialize(data: Json): Obj<T> {
        if (typeof data !== 'object' || data === null || Array.isArray(data))
            throw new FormatError('expect object', data)
        return mapObject(data, e => this.element.deserialize(e))
    }
}

export class ArrayOf<T> implements Schema<T[]> {
    constructor(public element: Schema<T>) { }

    serialize(value: T[]): JsonArray {
        return value.map(e => this.element.serialize(e))
    }

    deserialize(data: Json): T[] {
        if (!Array.isArray(data))
            throw new FormatError('expect array', data)
        return data.map(e => this.element.deserialize(e))
    }
}

export class PairOf<T, U> implements Schema<[T, U]> {
    constructor(public first: Schema<T>, public second: Schema<U>) { }

    serialize([a, b]: [T, U]): [Json, Json] {
        return [this.first.serialize(a), this.second.serialize(b)]
    }

    deserialize(data: Json): [T, U] {
        if (!Array.isArray(data) || data.length !== 2)
            throw new FormatError('expect array of length 2', data)
        const [a, b] = data
        return [this.first.deserialize(a), this.second.deserialize(b)]
    }
}

export class SetOf<T> implements Schema<Set<T>> {
    elements = new ArrayOf(this.element)

    constructor(public element: Schema<T>) { }

    serialize(value: Set<T>): JsonArray {
        return this.elements.serialize(Array.from(value))
    }

    deserialize(data: Json): Set<T> {
        return new Set(this.elements.deserialize(data))
    }
}

export class MapOf<K, V> implements Schema<Map<K, V>> {
    pair = new PairOf(this.key, this.value)
    elements = new ArrayOf(this.pair)

    constructor(public key: Schema<K>, public value: Schema<V>) { }

    serialize(value: Map<K, V>): JsonArray {
        return this.elements.serialize(Array.from(value))
    }

    deserialize(data: Json): Map<K, V> {
        return new Map(this.elements.deserialize(data))
    }
}

interface Prim {
    string: string
    number: number
    boolean: boolean
}

class Primitive<N extends keyof Prim> implements Schema<Prim[N]> {
    constructor(public name: N) { }

    serialize(value: Prim[N]): Prim[N] {
        return value
    }

    deserialize(data: Json): Prim[N] {
        if (typeof data !== this.name)
            throw new FormatError(`expect ${this.name}`, data)
        return data as Prim[N]
    }
}

export const Str = new Primitive('string')

export const Num = new Primitive('number')

export const Bool = new Primitive('boolean')

export const Sym = {
    serialize(value: symbol): string {
        const key = Symbol.keyFor(value)
        if (key === undefined)
            throw new FormatError('symbol has no key', String(value))
        return key
    },
    deserialize(data: Json): symbol {
        if (typeof data !== 'string')
            throw new FormatError('expect string', data)
        return Symbol.for(data)
    },
}

if (require.main === module) {
    const schema = new SumOf({ num: Num, sym: Sym })
    console.log(schema.serialize(['num', 1]))
    console.log(schema.deserialize(['sym', 't']))
}
