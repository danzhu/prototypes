{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Prelude hiding (curry, uncurry)

-- () -> a : a
-- (a, ()) -> b : a -> b
-- (a, (b, ())) -> c : a -> (b -> c)
-- (a, (b, (c, ()))) -> d : a -> (b -> (c -> d))

class Curry p r c | p r -> c where
  curry :: (p -> r) -> c
  uncurry :: c -> p -> r

instance Curry () r r where
  curry = ($ ())
  uncurry = const

instance Curry b r c => Curry (a, b) r (a -> c) where
  curry f a = curry $ \b -> f (a, b)
  uncurry f (a, b) = uncurry (f a) b

sum0 :: Num a => () -> a
sum0 () = 0

sum1 :: Num a => (a, ()) -> a
sum1 (a, ()) = a

sum2 :: Num a => (a, (a, ())) -> a
sum2 (a, (b, ())) = a + b

sum3 :: Num a => (a, (a, (a, ()))) -> a
sum3 (a, (b, (c, ()))) = a + b + c

class ToList a b where
  toList :: a -> [b]

instance ToList () a where
  toList = const []

instance ToList a b => ToList (b, a) b where
  toList (b, a) = b : toList a

main :: IO ()
main = do
  print $ (toList ((1, (2, ())) :: (Int, (Int, ()))) :: [Int])
  print $ curry sum0
  print $ curry sum1 1
  print $ curry sum2 1 2
  print $ curry sum3 1 2 3
  print $ uncurry (+) (1, (2, ()))
