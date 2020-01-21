data Expr
  = EVar String
  | EApp Expr Expr
  | EFun Pat Expr
  deriving (Show)

data Pat
  = PVar String
  | PApp Expr Pat
  deriving (Show)

-- test (i!) = {getLine! , putStrLn!}

test :: Maybe Int -> Maybe (IO ())
test m = m >>= \i -> pure $ getLine >>= \l -> putStrLn l
