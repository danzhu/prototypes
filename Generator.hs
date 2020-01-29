import           Control.Monad (ap, liftM, (>=>))

data Generator o i r
  = Output o (i -> Generator o i r)
  | Result r

instance Functor (Generator o i) where
  fmap = liftM

instance Applicative (Generator o i) where
  pure = Result
  (<*>) = ap

instance Monad (Generator o i) where
  Output o c >>= f = Output o $ c >=> f
  Result r   >>= f = f r

yield :: o -> Generator o i i
yield o = Output o pure

list :: Generator o () r -> [o]
list (Output o c) = o : list (c ())
list (Result _)   = []

each :: Monad m => (o -> m i) -> Generator o i r -> m r
each f (Output o c) = each f . c =<< f o
each _ (Result r)   = pure r


fib :: Int -> Int -> Generator Int i ()
fib a b = do
  _ <- yield a
  fib b (a + b)

foo :: Generator Int Int String
foo = do
  i <- yield 1
  _ <- yield $ i + 1
  pure "3"

bar :: Generator Int Int ()
bar = do
  _ <- yield 0
  res <- foo
  _ <- yield $ read res
  pure ()

main :: IO ()
main = do
  print . take 10 . list $ fib 0 1
  print =<< each ((1 <$) . print) bar
