{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoStarIsType  #-}
{-# LANGUAGE TypeOperators #-}

import Data.Functor.Identity (Identity (Identity), runIdentity)
import Control.Monad.Trans.Reader -- (ReaderT (ReaderT), runReaderT)
import Control.Monad.Trans.Identity

-- data E a b = L a | R b
type E a b r = a -> b -> r

-- data A a b = A (E a b)
type A a b r = (E a b r -> r) -> r

forward :: A a b r -> E a b r
forward t a b = t $ \f -> f a b

backward :: E a b r -> A a b r
backward t f = f t


type Wrap f a = (f a -> a) -> a

class ContWrap f where
  unwrap :: Wrap f a -> f a

instance ContWrap Identity where
  unwrap w = Identity $ w runIdentity

instance ContWrap m => ContWrap (IdentityT m) where
  unwrap w =
    IdentityT $ unwrap $ \p ->
    w $ \t ->
    p $ runIdentityT t

instance ContWrap (Reader r) where
  unwrap w =
    reader $ \r ->
    w $ \t ->
    runReader t r

instance ContWrap m => ContWrap (ReaderT r m) where
  unwrap w =
    ReaderT $ \r ->
    unwrap $ \p ->
    w $ \t ->
    p $ runReaderT t r

wrap :: f a -> Wrap f a
wrap = flip ($)

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap


data a + b = L a | R b
data a * b = P a b

l :: (a + b) -> Maybe a
l (L a) = Just a
l _     = Nothing

main :: IO ()
main = pure ()
