{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeApplications       #-}

module Main
  ( main
  ) where

import           Control.Monad.Cont (Cont, cont, runCont)
import           Data.Functor.Identity (Identity (Identity), runIdentity)
import           Data.List.NonEmpty (NonEmpty ((:|)))

-- type L a = forall r. (Zero r, Append r) => Cont r a
-- type N a = forall r. Append r => Cont r a
-- type O a = forall r. Zero r => Cont r a
-- type I a = forall r. Cont r a

class One l a | l -> a where
  one :: a -> l

class Zero l where
  zero :: l

class Append l where
  append :: l -> l -> l

instance One [a] a where
  one = (: [])

instance One (NonEmpty a) a where
  one = (:| [])

instance One (Maybe a) a where
  one = Just

instance One (Identity a) a where
  one = Identity

instance Zero [a] where
  zero = []

instance Zero (Maybe a) where
  zero = Nothing

instance Append [a] where
  append = (++)

instance Append (NonEmpty a) where
  append = (<>)

class From l r a | l -> a where
  from :: l -> Cont r a

instance (Zero r, Append r) => From [a] r a where
  from l = cont $ \f -> foldr (append . f) zero l

instance Append r => From (NonEmpty a) r a where
  from l = cont $ \f -> foldr1 append $ f <$> l

instance Zero r => From (Maybe a) r a where
  from l = cont $ \f -> maybe zero f l

instance From (Identity a) r a where
  from = pure . runIdentity

collect :: One l a => Cont l a -> l
collect = flip runCont one

main :: IO ()
main = do
  print . collect @[String] $ do
    a <- from $ Identity "1"
    b <- from $ "2" :| ["3"]
    c <- from $ Just "4"
    d <- from ["5", "6"]
    pure $ a <> b <> c <> d
  print . collect @(Maybe String) $ do
    a <- from $ Identity "1"
    c <- from $ Just "4"
    pure $ a <> c
  print . collect @(NonEmpty String) $ do
    a <- from $ Identity "1"
    b <- from $ "2" :| ["3"]
    pure $ a <> b
  print . collect @(Identity String) $ do
    a <- from $ Identity "1"
    pure $ a <> a
