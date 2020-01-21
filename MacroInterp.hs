{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

type Var = String

data Val
  = VStr String
  | VFun (Ctx -> Ast -> Val)

instance Show Val where
  show = \case
    VStr s -> show s
    VFun f -> "<func>"

data Ast
  = AStr String
  | AVar Var
  | AApp Ast Ast

instance Show Ast where
  show = \case
    AStr s -> show s
    AVar v -> v
    AApp f a -> "(" ++ show f ++ " " ++ show a ++ ")"

type Ctx = [(Var, Val)]

interpret :: Ctx -> Ast -> Val
interpret ctx = \case
  AStr str -> VStr str
  AVar var -> case lookup var ctx of
    Just v  -> v
    Nothing -> error $ "interpret: no such variable: " ++ var
  AApp fun arg -> case interpret ctx fun of
    VFun f -> f ctx arg
    v      -> error $ "interpret: applying non-function: " ++ show v

macro = VFun . const
function f = VFun \ctx -> f . interpret ctx

lambda :: Ctx -> Ast -> Val
lambda ctx (AVar param) =
  macro \body ->
  function \arg ->
  interpret ((param, arg) : ctx) body

main :: IO ()
main = do
  let ident = AApp (AApp (AVar "fn") (AVar "a")) (AApp (AVar "id") (AVar "a"))
      ast = AApp ident $ AApp (AVar "show") $ AVar "var"
      ctx = [ ("show", macro $ VStr . show)
            , ("id", VFun interpret)
            , ("fn", VFun lambda)
            ]
  print $ interpret ctx ast
