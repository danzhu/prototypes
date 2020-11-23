{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import Control.Monad.Trans.RWS
import qualified Data.Map as M

newtype Memoize k v a = Memoize { unMemoize :: RWS (k -> Memoize k v v) () (M.Map k v) a }
  deriving (Functor, Applicative, Monad)

need :: Ord k => k -> Memoize k v v
need k = Memoize $ gets (M.lookup k) >>= \case
  Just a -> pure a
  Nothing -> do
    f <- ask
    v <- unMemoize $ f k
    modify $ M.insert k v
    pure v

runMemoize :: Ord k => (k -> Memoize k v v) -> k -> v
runMemoize f k = fst $ evalRWS (unMemoize $ f k) f mempty

fib :: Int -> Memoize Int Int Int
fib 0 = pure 0
fib 1 = pure 1
fib n = (+) <$> need (n - 1) <*> need (n - 2)

main = print $ runMemoize fib 10
