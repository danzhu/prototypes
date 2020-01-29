{-# LANGUAGE DeriveFunctor #-}

import           Control.Monad (ap, liftM)
import           Control.Monad.Trans.Writer (execWriter, tell)

data Task a r
  = Emit (a (Task a r))
  | Done r

instance Functor a => Functor (Task a) where
  fmap = liftM

instance Functor a => Applicative (Task a) where
  pure = Done
  (<*>) = ap

instance Functor a => Monad (Task a) where
  Emit a >>= f = Emit $ (f =<<) <$> a
  Done r >>= f = f r

emit :: ((r -> Task a r) -> a (Task a r)) -> Task a r
emit a = Emit (a Done)

runTask :: Monad m => (a (Task a r) -> m (Task a r)) -> Task a r -> m r
runTask run (Emit act) = runTask run =<< run act
runTask _ (Done res)   = pure res


data Gen o i t = Gen o (i -> t)
  deriving (Functor)

runGen :: Functor m => (o -> m i) -> Gen o i t -> m t
runGen f (Gen o c) = c <$> f o

type Generator o i = Task (Gen o i)

yield :: o -> Generator o i i
yield = emit . Gen

each :: Monad m => (o -> m i) -> Generator o i r -> m r
each f = runTask (runGen f)

list :: Generator o () r -> [o]
list = execWriter . each (tell . (:[]))

fib :: Int -> Int -> Generator Int i ()
fib a b = yield a *> fib b (a + b)


data IOAct t
  = GetLn (String -> t)
  | PutStrLn String t
  deriving (Functor)

runIO :: IOAct t -> IO t
runIO (GetLn t)      = t <$> getLine
runIO (PutStrLn s t) = t <$ putStrLn s

type IOTask = Task IOAct

ioGetLn :: IOTask String
ioGetLn = emit GetLn

ioPutStrLn :: String -> IOTask ()
ioPutStrLn ln = emit $ PutStrLn ln . ($ ())

echo :: IOTask ()
echo = ioPutStrLn =<< ioGetLn


main :: IO ()
main = do
  print . take 10 . list $ fib 0 1
  runTask runIO echo

-- objective: like Generator, but ensure correct output/input type pairs
