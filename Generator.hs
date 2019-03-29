data Generator i o r = Output o (i -> Generator i o r)
                     | Result r

instance Functor (Generator i o) where
  -- fmap f g = g >>= pure . f
  fmap f (Output o cont) = Output o $ \i -> fmap f $ cont i
  fmap f (Result r)      = Result $ f r

instance Applicative (Generator i o) where
  pure = Result
  -- g1 <*> g2 = g1 >>= \f -> fmap f g2
  Output o cont <*> g             = Output o $ \i -> cont i <*> g
  Result f      <*> Output o cont = Output o $ \i -> fmap f $ cont i
  Result f      <*> Result r      = Result $ f r

instance Monad (Generator i o) where
  Output o cont >>= f = Output o $ \i -> cont i >>= f
  Result r      >>= f = f r

yield :: o -> Generator i o i
yield o = Output o pure

list :: Generator () o r -> [o]
list (Output o f) = o : list (f ())
list (Result _)   = []


fib :: Int -> Int -> Generator i Int ()
fib a b = do
  yield a
  fib b (a + b)

foo :: Generator Int Int String
foo = do
  i <- yield 1
  yield $ i + 1
  pure "3"

bar :: Generator Int Int ()
bar = do
  yield 0
  res <- foo
  yield $ read res
  pure ()

main :: IO ()
main = do
  print $ take 10 $ list $ fib 0 1
  run bar
  where run (Output o f) = print o >> run (f 1)
        run (Result r)   = print r
