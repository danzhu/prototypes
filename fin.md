# Fin Prototype

## racket-style macro

``` fin
def &((match $cases) $cond) = &(match $cond $cases)

def &fun param... &= body... = &{
    def lambda $param... = $body...
    lambda
}

&(if $cond then $succ else $fail) = &(match $cond {
    T _ = $then
    F _ = $else
})

def &cond [&:block cases...] = match cases {
    [] = &(error "cond no match")
    [[cond then...] rest...] = &(if $cond $then (cond (:block $rest...)))
}

def &cond expr [&:block [pred... &= then...]...] = &{
    let val = $expr
    cond $[&:block [[pred... &val] then...]...]
}
```


## math context

changes parsing (like bash `$((1 + 2))`)

``` fin
def fib = of {
    0 = 0
    1 = 1
    n = :m fib (n - 1) + fib (n - 2)
}
```


## unified block for if and match

optional brace

``` fin
if b {
    then c
    else d
}

match n
1 = 1
_ = fact (pred n)
```


## auto-quoting for macro arguments

``` scheme
`(a = ,(if `b `c `d))
```
``` fin
a = {if b c d}
```

``` fin
{and a b = if a b &False}
c = {and a b}
```
expands to
``` fin
c = {if a b False}
```


## data type and pattern equivalence

equivalence (as in predicate in prolog) for data types and patterns

``` fin
struct Res T E {
    T { val = T }
    F { val = E }
}
match a { T val = T val, x = x } # = a, forall a
```


## pattern as a value

``` fin
[x, y] : Pat (List a) {x = a, y = a}
p -> e : v -> r
# where
p : Pat v b
e : r with b in scope
```

alternatively
``` fin
&[x, y] : List a -> Res {x = a, y = a} ()
syntax -> p e a = match p a {
    T b = with b e
    F v = F v
}
```
example
``` fin
match lst {
    &[x] -> x
}
```


## multiple patterns projecting to same value

``` fin
!ShowHex background = "232425" # background = Colour 0x232425
print (ShowHex background) # = "232425"
print (ShowRgb background) # = "35,36,37"
```


## lambda syntax

no precedence issues, multiple clauses
``` fin
{T x -> x}
```
better chaining, separate syntax from dict
``` fin
?T x -> x
```


## bi-directional patterns

``` fin
Json (Point x y) = JArr [JNum x, JNum y]
!Str data = "[1, 2]"
match data {
    Json (Point x y) = print x *> print y
}
```
or
``` fin
!Str (!Json (Point x y)) = "[1, 2]"
print x *> print y
```


## lua-style table-lists

function call
``` fin
map : {f : a -> b, list : List a} -> List b
```
positional and named arguments
``` fin
map str list=[1, 2]
# App (a -> b) c = (a - c) -> b
: {f : Int -> b} -> List b
map list=[1, 2]
```

this may break higher-order usage though:
``` fin
compose : {b -> c, a -> b, a} -> c
```
name of b? a?

so alternatively only for records (no partial app)
``` fin
map{f, list} = ...
map{str, list=[1, 2]}
```
this allows default values, which for functions would break currying

curry & default args are mutually exclusive
so ml/scala-style application might be good (you choose curry/tuple args):
``` fin
copy : Str -> Str -> {recursive : Bool, mkdir : Bool} -> IO ()
```
optional/named arguments are usually not suitable for currying anyways


## expression patterns

expressions can be used as predicates in pattern
``` fin
fact : Int -> Int
fact = {
    (< 2) -> 1
    'n -> n * fact (n - 1)
}
```
lens (getter) in pattern, and pattern groups (racket `and` pattern)
``` fin
x, y : Functor f => (Int -> f Int) -> Pos -> f Pos
dist : Pos -> Int
dist {x 'x, y 'y} = abs x + abs y
```
possible implementation with pattern of type `a -> Match a`:
``` fin
Match = ...
Accept f = {accept : a -> f a}
Reject f = {reject : a -> f a}
Functor Match = ...
Accept Match = ...
Reject Match = ...
(<) : Accept f => Reject f => Int -> Int -> f Int
'a : a -> Match a
```

alternatively, as an opaque contravariant type
``` fin
Just : Pat a -> Pat (Maybe a)
Nothing : Pat (Maybe a)
or : a -> Maybe a -> a
or d = {
    Just 'a -> a
    Nothing -> d
}
```

or, simply use regular functions for transformations
(like Haskell view patterns)
``` fin
(:) : Getter s a -> s -> a
x, y : Lens' Pos Int
dist : Pos -> Int
dist {:x 'x, :y 'y} = abs x + abs y
```
desugar:
``` fin
dist = \t0 -> (
    'x = t0 : x
    'y = t0 : y
    abs x + abs y
)
```


## pattern lift operator

`!` operator (like in Idris) but for patterns
theoretical (pattern) type:
``` fin
(!) : a -> m a
```
example:
``` fin
Left : Either a b -> Maybe a
left : Either a b -> Maybe a
left = \(Left (!x)) -> Just x
```
desugar:
``` fin
left = \t0 -> Left t0 >>= \x -> Just x
```


## lambda calculus extension: pattern syntax

expression:
abstraction
``` fin
\x -> a
(x : Pat a) -> (a : Expr b) -> Expr (a -> b)
```
application
``` fin
a b
(a : Expr (a -> b)) -> (b : Expr a) -> Expr b
```
let
``` fin
let x = a in b
(\x -> b) a
(a : Expr a) -> (x : Pat a) -> (b : Expr b) -> Expr b
```

pattern:
view pattern
``` fin
(a -> x)
v | x <- a v
(a : Expr (a -> b)) -> (x : Pat b) -> Pat a
```
pattern guard
``` fin
x | y <- a
((\x -> a) -> y)
(x : Pat a) -> (a : Expr b) -> (y : Pat b) -> Pat a
```


## CPS-based patterns

translates directly into lambda calculus,
since the exprs in match arms are the continuations,
and introduces variables easily
``` fin
Left : Either a b -> (a -> r) -> Maybe r
Pair : (a, b) -> (a -> b -> r) -> r
```
or flip around for lens-style:
``` fin
Left : (a -> r) -> Either a b -> Maybe r
Pair : (a -> b -> r) -> (a, b) -> r
```

alternatively, single pattern for whole type (decision tree):
``` fin
Either : Either a b -> (a -> r) -> (b -> r) -> r
List : List a -> (a -> List a -> r) -> r -> r
```
this removes the dependency on `Maybe`

``` fin
T = a * (b + c)
T : T -> (a -> ((b -> r) -> (c -> r) -> r) -> r) -> r
```


## do-block for dynamic language

similar to Racket `(require algebraic/class)`
can be used for do-syntax or operator
``` fin
main = IO {
    pure ()
}
```


## right-associative tag/function

``` fin
sort values map
# =>
sort (values map)
```

``` fin
ok: some: 1
# =>
Ok (Some 1)
```

can be syntactic sugar for pair with left operand quoted (like Perl)
``` fin
ok: some: 1
# =>
'(ok (some 1))
```

``` fin
Void = Sum {}
Unit = Prod {}
Opt a = Sum {
    some: a
    none: Unit
}
tagged {
    tag: 'Sum
    data: object {
        {some 'some, symbol 'a}
        {some 'none, symbol 'Unit}
    }
}

Data = Sum {
    tagged: Prod {tag: Sym, data: Opt Data}
    object: List Prod {key: Opt Sym, value: Data}
}
Json = Sum {
    null: Unit
    string: Str
    number: Float
    boolean: Bool
    array: List Json
    object: List Prod {name: Str, value: Json}
}

tagged {'Sum, object {
    {some 'tagged, tagged {'Prod, object {
        {some 'tag, symbol 'Sym}
        {some 'data, tagged {'Opt, symbol 'Data}}
    }}}
    {some 'object, tagged {'List, tagged {'Prod, object {
        {some 'key, tagged {'Opt, symbol 'Sym}}
        {some 'value, symbol 'Data}
    }}}}
}}

tagged "Sum" | object {
    some "tagged" = tagged tag:"Prod" data:| object {
        some "tag" = symbol "Sym"
        some "data" = tagged "Opt" | symbol "Data"
    }
    some "object" = tagged "List" | tagged "Prod" | object {
        some "key" = tagged "Opt" | symbol "Sym"
        some "value" = symbol "Data"
    }
}

Tagged("Sum", Object({
    Some("tagged"): Tagged("Prod", Object({
        Some("tag"): Symbol("Sym"),
        Some("data"): Tagged("Opt", Symbol("Data")),
    })),
}))
```

packages build
``` fin
Ast = Sum
    infix : Prod
        op : Str
        left : Ast
        right : Ast
    prefix : Prod
        op : Str
        val : Ast
    pair : Prod
        first : Ast
        second : Ast

packages = flat_map import < fs:find "*/.pkg"

config = import "config"

render = in out ->
    fs:write out <
    str:replace_all "\{\{(.*)\}\}" config <
    fs:read in

default = map packages < config "packages"
```


## indent syntax

``` fin
if cond then
    true
  else
    false
```


## newline lists

newline separates list elements, i.e. `\n` = `,`
pedantic: each non-empty line = one element

``` haskell
[a 1, b 2]
a 1 : b 2 : []
```
``` fin
[a 1, b 2]
[a 1
 b 2]
[
  a 1
  b 2
]
```

``` haskell
a 1 : b 2 : l 3
```
``` fin
[a 1, b 2 | l 3]
[a 1
 b 2
|l 3]
[
  a 1
  b 2
| l 3
]
```

maybe also for expressions
but this doesn't play well with currying
better suited for lisp

impure: `a 1; b 2`
pure: `a 1 >> b 2`
``` fin
(a 1, b 2)
```
and this doesn't really make sense / isn't really useful
``` haskell
# `a 1; b 2 (c 3)`
# `a 1 >> b 2 (c 3)`
```
``` fin
(a 1, b 2 | c 3)
(a 1
 b 2
|c 3)
(
  a 1
  b 2
| c 3
)
```


## generalized string interpolation

normal string interpolation
``` sh
"hello $user"
```
``` haskell
"hello " ++ show user
```

syntax can be generalized, e.g. as `Applicative`:
``` fin
"$[1, 2] + $[3, 4]"
```
``` haskell
(\a b -> show a ++ " + " ++ show b) <$> [1, 2] <*> [3, 4]
[show a ++ " + " ++ show b | a <- [1, 2], b <- [3, 4]]
```

and reversed, for pattern matching:
``` fin
"$k=$v" = "key=val"
```
``` haskell
(k, v) ~= match "(.*)=(.*)" "key=val"
```

for both cases, formatting (or some other extra info) may be needed:
format for expression
``` fin
"0x${addr:x}"
```
``` haskell
printf "0x%x" addr
```
regex for pattern
``` fin
"${key:[^=]}=$val"
```

alternatively, have some special type to wrap extra info:
``` fin
RegexPat : Regex -> Pat -> Pat
"${RegexPat '[^=]' key}=$val"
```
in which case, having some sort of quoting may be cleaner
``` fin
"${RegexPat [^=] key}=$val"
reg = "[^=]"
"${RegexPat $reg key}=$val"
```

example use case: routing
``` fin
route {
    Get "/" -> home
    Get "/user/$id" -> get_user id
}
```

we may even want a "quasiquote" mechanism for otther syntax, e.g. sql
``` fin
sql! select $name, $age from users where id = $id
sql! select ${count:count(*)} from users
sql! insert into users ($name, $age)
```
or simply reuse string interpolation syntax for everything:
``` fin
sql! "select ${count:count(*)} from users"
Macro "sql!" (Interp [Str "select ", Val "count" "count(*)", Str " from users"])
```
macros would be needed for this,
since inputs and outputs are mixed in the same interpolation
or, separate into input and output:
``` fin
select! "$name, $age" = from! "users where id = $id"
```


## macro as functions (first-class)

for some expression
``` fin
f (g a)
```
instead of evaluating as
``` fin
(eval ctx 'f) (eval ctx '(g a))
```
we can pass context and ast to the function
``` fin
(eval ctx 'f) ctx '(g a)
```
now, `f` can choose to behave like a normal function
``` fin
f ctx ast = ... (eval ctx ast) ...
```
or, it can do macro stuff
``` fin
quote _ ast = ast
scope ctx _ = ctx
assert ctx ast = when (not $ eval ctx ast) do
    print "assert failed:" ast
```
this has the benefits:
1. removes the need for separate mechanisms for functions/macros
2. macros can be passed around like values (first-class)
downside is of course:
1. less predictable evaluation
2. can't optimize
up/down-side (depends):
1. macros can be shadowed (also in lisp)


## unambiguous currying in ast

in ast
``` fin
a b c
```
can be represented as
``` fin
App (App a b) c
```
without ambiguity since
``` fin
{a b} c
```
is
``` fin
App (Block [App a b]) c
```
but in expr both
``` fin
(a, b), c
a, b, c
```
are
``` fin
Pair (Pair a b) c
```

note: above example uses `{}` (which is an extra node in ast);
`()` doesn't work (assuming it is not represented in ast)


## scope-as-an-object

two ways of sharing data between multiple functions:
``` javascript
class Count {
    constructor() {
        this.val = 0
    }
    add() { this.val += 1 }
    get() { return this.val }
}

function count() {
    let val = 0
    return {
        add() { val += 1 },
        get() { return val },
    }
}
```
i.e. OOP / functional

OOP style is
1. more inspectable:
   ``` javascript
   let c = new Count()
   console.log(c.val)
   ```
2. can call the other functions directly (e.g. `add` can call `get`);
   functional can do the same, but more verbosely:
   ``` javascript
   let val = 0
   let add = () => { val += 1 }
   let get = () => val
   return { add, get }
   ```

but functional style:
1. can be nested (arguably Java classes also can be nested, but complicated)
2. allow non-class return values (also possible in OOP w/o constructors)
3. can enforce encapsulation by not even exposing the private variables
   (though making it hard to inspect, can workaround with some `toString`)

can combine the two
``` fin
scope count() {
    let val = 0
    pub add() { val += 1 }
    pub get() { return val }
}
```

can also act as modules

alternative syntax
using `=` for variables and `:` for exports
this (kind of) mirrors variables/kv-pairs in e.g. js
``` fin
count () = {
    val = 0
    add () : val += 1
    get () : val
}
```

can have `:` without key as `return` / merging (js `...` / py `**`)
``` fin
parse = {
    expr = ...
    term = ...
    factor = ...
    : expr
}
```
alternative semantics would be extended pattern matching
i.e. `parse.expr` matches `expr = ...`, `parse.foo` matches `: expr`
complicated for curried nested patterns (including example above) though:
``` fin
val = {
    foo 1 = 1
    foo = 2
}
```
value of `val.foo`?
to make this work, type system may need to support arbitrary intersections
``` fin
val.foo == {1 = 1} & 2 == {1 = 1, = 2}
```
also pattern matching would make enumeration not plausible
(assuming that we allow variables, which is needed if we want functions)
and thus less of an object and more of just a function
(unless of course, we do it like Prolog)


## function Prolog search

``` fin
file ~/.vim/$f =
    guard (f != "init.vim")
    file vim/$f
```
Prolog
``` prolog
file(P, C) :-
    append("~/.vim/", F, P),
    F != "init.vim",
    append("vim/", F, P2),
    file(P2, C).
```

there's always a functional dependency for
``` fin
foo(A, B, ..., Z)
```
which is
``` fin
A, B, ... -> Z
```
since it simulates expression
``` fin
foo(A, B, ...) = Opt Z
```


## cons cell vs lambda

compare lisp lists:
``` scheme
(a b . a)
(a . (b . a))
```
with Haskell lambdas:
``` haskell
\a b -> a
\a -> \b -> a
```
both right-associative and "curried"

though there's no "empty function" so
``` fin
(a b)
(a b . ())
```
makes sense but
``` fin
\a b
\a b -> \
```
doesn't


## multi-match

normally pattern matching only takes a single value as input,
therefore map/dict needs special syntax:
``` fin
{ name: name } = { name: "Name", id: 11 }
```
if we use a list of pairs, this wouldn't always work:
``` fin
let ("name", name) : _ = [("id", 11), ("name", "Name")]
```
to make something like this work:
``` fin
[..., (2, x), ...] = [(0, 0), (1, 1), (2, 2)]
```
we could backtrack when matching (like parser combinators)
``` fin
match : Patt (List a) -> List a -> Match
match [..., patt, ...] list = choice $ map (match patt) list
```

this enables us to e.g. implement maps/dicts using sets:
``` fin
{ 'name -> name } = { 'id -> 11, 'name -> "Name" }
```
where `{}` are sets and `a -> b` is a key-value pair/tuple `(a, b)`
which compares using the key (like Haskell `Arg`)

now, naive implementation would have horrible performance,
so to make this useful outer pattern may need to inspect inner pattern,
and optimize based on that


## type-safe sql with arrays

type-safe: multiplicity
I: Identity
O: Optional
N: NonEmpty
L: List

join:
I x = x
O O = O
O N = L
O L = L
N N = N
N L = L
L L = L

foreign key:
``` fin
get_user : Id User -> Identity User
```

array
sql
``` sql
create table user
( id integer primary key
);
create table email
( user integer references user (id)
, email varchar
);
select user, email
from email
where email = "<email>";
```
array-sql
``` fin
User
    id Identity Int
    email NonEmpty Email
query
    user <- users
    user.email == "<email>"
    user
```


## world state with functional update

if values track their origin,
then we can model world state (e.g. filesystem) with functional updates
(i.e. producing a new value instead of mutation)
and still be efficient by not doing unnecessary work

e.g. create file:
``` fin
over (dir "some_path") $ insert "foo.txt" $ File 644 "content"
```
input -> output:
``` fin
<world_fs> -> { "foo.txt": File 644 "content", ...<world_fs> }
```
and thus we know original files in the dir need not to be updated;
only "foo.txt" needs to be created

this can extend to e.g. copying files:
``` fin
\fs -> insert "b" (fs ! "a") fs
```

moving (and swapping) is a lot trickier:
harder to detect and ordering is important.
one inefficient way is to just load into memory before performing operations,
(which is essentially copying even when moving is possible)

idea is similar to Haskell's IO monad:
instead of functions having side effects (IO) or working on read data (here),
they work on and produce *descriptions*,
which then is "executed" by a (stateful) interpreter

additionally, origin info can be used to track incremental build:
since all of the values used to produce a result are dependencies.
although, this needs to also include code used to produce the value,
so it's not entirely the same


## sync and async monad

Most languages with `async`/`await` suffers from having to choose:
1. sync code: cannot block on async, so more limited
2. async code: syntax heavy, so harder to write

Since promises/futures are monads, point 2 is not a problem in Haskell
as `IO` is a monad anyways.
So an alternative, promise/future-based Haskell can have async IO by default,
and a sync monad for synchronous operations (useful for e.g. critical section):

``` haskell
readLine :: Async String
writeLine :: String -> Async ()
readVar :: Var a -> Sync a
writeVar :: Var a -> a -> Sync ()
sync :: Sync a -> Async a
```

This is analogous to `IO` vs `STM` - former more general and often used,
latter for guarantee of atomicity / no context switch.


## database identity vs value

Identity as a special type/property like ML ref;
database is otherwise immutable.

``` fin
User
    name Ref String

Room
    users Ref (List User)

Message
    user User
    room Room
    text Ref String
```

This maps better to languages like ML and Haskell,
but worse for languages with re-assignable fields.

Alternatively, a coarse-grained database update implementation can support
full-row updates only,
which would correspond to an ref for the whole record.
This also simplifies ORM and avoids partial updates.


## dynamic (function) modules/objects

Normally invoking a (non-closure) function involves
a path (module/object + name) and argument(s).
This is (usually) the same as first getting/creating closure,
and then apply closure on arguments:

``` haskell
Foo.bar 1 == (Foo.bar) 1
```

``` python
foo.bar(1) == (foo.bar)(1)
```

What if we associate the other way? i.e. combine name and arguments first,
and then apply the module/object on the call?

``` fin
net = \
    .tcp -> \
        .listen{port} -> ...
        .accept{socket, timeout} -> ...

socket = net.tcp.listen{8000}
mod = net.tcp
op = .accept{socket, 1000}
mod op
```

In this way, a module/object is a function that accepts messages,
much like Erlang/Elixir processes.
The module/object can then decide to e.g. forward the message (re-export),
therefore making proxy/reflection trivial without language/library support.

One downside is that this requires compound values,
and modules/objects are also like compound values,
so either:
1. language supports compound values, making this kind of pointless, or
2. use atoms/symbols as keys first to build records,
   and then use the records for module/object function/methods;
   which seems quite unnecessary.
   
So alternatively one can forgo the idea of keeping name and arguments together,
and just do curried application for both function fetching and application:

``` fin
Tcp = \
    .listen -> Int -> Socket
    .accept -> Socket -> Int -> Socket
tcp : (F : .listen | .accept) -> Tcp F = \
    .listen port -> ...
    .accept socket timeout -> ...

socket = tcp.listen 8000
m = .accept
{conn = m socket 100} = tcp
```

This syntax removes the need to separate static/computed property access:
property access is just function application on atom/symbol.


## function call end marker

One downside of currying is that functions don't (natively) support variable
number of arguments.
One can use compound structure (list/map/record) for this purpose,
or a special marker value (possibly also syntax) to mark end of arguments:

``` fin
add 1 2 .
```

Nevertheless this makes currying lose most of its benefits:
`f a b . != (f a .) b .` unless `f` ignores `.` at 1 argument.


## overlay git

overlay fs - similar to git branch

``` haskell
base :: Map Path File
overlay :: Map Path (Maybe File)

overlay :: Path -> Maybe File -> Maybe File
```


## plain text layout engine

```
func
    margin-below 2
```

``` python
def foo():
    ...


def bar():
    ...
```


## first-class lens

``` fin
data sum Maybe a
| Just a
| Nothing ()

data product Pair a b
& first a
& second b

data alias List a
= Maybe (Pair a (List a))

data newtype Arg a b
= Pair a b

instance Eq (Arg a b) where
  x == y = x ^. from Arg . #first == y ^. from Arg . #first
```

sum, product, alias like `GHC.Generics`: transparent and auto instances

newtype has no auto instances

constrained types:

- `#first : Lens (Pair a c) (Pair b c) a b`
- `#Just : Prism (Maybe a) (Maybe b) a b`
- `Arg : Iso (Arg a b) (Arg c d) (Pair a b) (Pair c d)`

general types:

- `#first : HasLens "first" s t a b => Lens s t a b`
- `#Just : HasPrism "Just" s t a b => Prism s t a b`
- `Arg : Iso (Arg a b) (Arg c d) (Pair a b) (Pair c d)`

sum and product has `OverloadedLabels`-like optics, thus public

newtype just has an in-scope `Iso`, so private unless exported

pattern matching is like `^?`, i.e. traverse and (maybe) get first

``` fin
maybe a #Nothing  = a
maybe _ (#Just b) = b
```

``` haskell
maybe a m
  | Just _ <- m ^. _Nothing = a
  | Just b <- m ^. _Just    = b
```

problem is partial check: sum types work out nicely:
`HasLens "first" s => HasLens "second" s => ...`

but for product types, we want to have constraints that either
1. enforce no extra constructor
2. leave a `Partial`-like marker
