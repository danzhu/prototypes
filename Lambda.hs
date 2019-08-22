{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

import           Control.Arrow (second)
import           Control.Monad.Writer (Writer, execWriter, tell)
import           Data.Foldable (for_)
import           Data.List (intersperse)
import           Data.Proxy (Proxy (Proxy))
import           GHC.Exts (IsString, fromString)
import           GHC.OverloadedLabels (IsLabel, fromLabel)
import           GHC.TypeLits (KnownSymbol, symbolVal)


newtype Var = MkVar String

data Ast
  = Abs Var Ast
  | App Ast Ast
  | Var Var
  | Def [(Var, Ast)] Ast
  | Str String

data Lazy
  = LVar Var
  | LDelay Eager

data Eager
  = EAbs Var Eager
  | EApp Eager Lazy
  | EDef [(Var, Lazy)] Eager
  | EStr String
  | EForce Lazy

data Expr
  = Lazy Lazy
  | Eager Eager

-- class IsAst a where
--   fromAst :: Ast -> a

-- instance IsAst Ast where
--   fromAst = id

-- instance IsAst a => IsAst (Ast -> a) where
--   fromAst fun = fromAst . App fun

instance IsString Ast where
  fromString = Str

instance KnownSymbol l => IsLabel l Var where
  fromLabel = MkVar $ symbolVal $ Proxy @l

instance KnownSymbol l => IsLabel l Ast where
  fromLabel = Var $ fromLabel @l

-- instance (KnownSymbol l, IsAst a) => IsLabel l (Ast -> a) where
--   fromLabel = fromAst . App (fromLabel @ l)

convert :: Ast -> Eager
convert (Abs var body)  = EAbs var $ convert body
convert (App fun arg)   = EApp (convert fun) $ LDelay $ convert arg
convert (Var var)       = EForce $ LVar var
convert (Def vars body) = EDef (map (second $ LDelay . convert) vars) $ convert body
convert (Str str)       = EStr str


type Serializer = Writer String

paren :: Serializer a -> Serializer a
paren w = tell "(" *> w <* tell ")"

brace :: Serializer a -> Serializer a
brace w = tell "{" *> w <* tell "}"

arrow :: [Serializer ()] -> Serializer a -> Serializer a
arrow params body = paren $ paren (sepList params) *> tell "=>" *> body where
  sepList = sequence_ . intersperse (tell ",")

class Serializable a where
  serialize :: a -> Serializer ()

instance Serializable Var where
  serialize (MkVar var) = tell $ '_' : var

instance Serializable Lazy where
  serialize (LVar var) = serialize var
  serialize (LDelay expr) = do
    tell "delay"
    arrow [] $ serialize expr

instance Serializable Eager where
  serialize (EAbs var body) = arrow [serialize var] $ serialize body
  serialize (EApp fun arg) = do
    serialize fun
    paren $ serialize arg
  serialize (EDef vars expr) = do
    arrow [] $ brace $ do
      for_ vars $ \(var, val) -> do
        tell "const "
        serialize var
        tell "="
        serialize val
        tell ";"
      tell "return "
      serialize expr
    tell "()"
  serialize (EStr str) = do
    tell $ show str
  serialize (EForce expr) = do
    tell "force"
    paren $ serialize expr


entry :: String -> String
entry s = unlines
  [ "const strict = v => ({v})"
  , "const delay = f => ({f})"
  , "const force = c => 'v' in c ? c.v : (c.v = c.f())"
  , "const _True = strict(true)"
  , "const _False = strict(false)"
  , "const _if = strict(c => t => f => force(c) ? force(t) : force(f))"
  , "const _and = strict(a => b => a && b)"
  , "const _or = strict(a => b => a || b)"
  , "const _not = strict(a => !a)"
  , "const _eq = strict(a => b => force(a) === force(b))"
  , "const _ne = strict(a => b => force(a) !== force(b))"
  , "const _lt = strict(a => b => force(a) < force(b))"
  , "const _le = strict(a => b => force(a) <= force(b))"
  , "const _gt = strict(a => b => force(a) > force(b))"
  , "const _ge = strict(a => b => force(a) >= force(b))"
  , "const _int = strict(a => Number.parseInt(force(a)))"
  , "const _add = strict(a => b => force(a) + force(b))"
  , "const _sub = strict(a => b => force(a) - force(b))"
  , "const _mul = strict(a => b => force(a) * force(b))"
  , "const _div = strict(a => b => force(a) / force(b))"
  , "const _cons = strict(h => t => [h, t])"
  , "const _head = strict(l => force(force(l)[0]))"
  , "const _tail = strict(l => force(force(l)[1]))"
  , "const _null = strict(null)"
  , "const _trace = strict(a => {console.log(force(a)); return force(a)})"
  , "const _error = strict(a => {throw Error(force(a))})"
  , "const main = " ++ s
  , "console.log(main)"
  ]

infixr 0 ==>

(==>) :: [(Var, Ast)] -> Ast -> Ast
(==>) = Def

infixr 0 -->

(-->) :: Var -> Ast -> Ast
(-->) = Abs

infixl 1 $$

($$) :: Ast -> Ast -> Ast
($$) = App

prog :: Ast
prog =
  [ (#undef, #error $$ "undef")
  , (#zero, #int $$ "0")
  , (#one, #int $$ "1")
  , (#fact, #n -->
      #if $$ (#ge $$ #one $$ (#trace $$ #n)) $$
      #one $$
      (#mul $$ #n $$ (#fact $$ (#sub $$ #n $$ #one))))
  ] ==>
  #fact $$ (#int $$ "5")

main :: IO ()
main = do
  let p = convert prog
      s = execWriter $ serialize p
      r = entry s
  putStr r
