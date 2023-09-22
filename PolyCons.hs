{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

class Cons a b c | c -> a b where
  cons :: a -> b -> c

class Nil n where
  nil :: n

instance Cons a [a] [a] where
  cons = (:)

instance Nil [a] where
  nil = []

instance Cons a b (a, b) where
  cons = (,)

instance Nil () where
  nil = ()

val :: (Nil a, Cons b a c, Cons d c e, Num b, Num d) => e
val = cons 1 $ cons 2 nil

list :: [Int]
list = val

tuple :: (Int, (Int, ()))
tuple = val

mixed :: (Int, [Int])
mixed = val

main :: IO ()
main = do
  print list
  print tuple
  print mixed
