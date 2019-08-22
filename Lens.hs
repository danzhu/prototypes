{-# LANGUAGE RankNTypes #-}

import           Control.Applicative (Const (Const), getConst)
import           Data.Functor.Identity (Identity (Identity), runIdentity)

class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
  lmap :: (a -> b) -> p b c -> p a c
  lmap = flip dimap id
  rmap :: (a -> b) -> p c a -> p c b
  rmap = dimap id

instance Profunctor (->) where
  dimap l r p = r . p . l
  lmap = flip (.)
  rmap = (.)

instance Profunctor (Exchange a b) where
  dimap l r (Exchange f g) = Exchange (f . l) (r . g)

type Iso       s t a b = forall p f. (Profunctor p, Functor f) => p a (f b)            -> p s (f t)
type Lens      s t a b = forall f. Functor f                   => (a -> f b)           -> s -> f t
type Getter    s   a   = forall r.                                (a -> Const r a)     -> s -> Const r s
type Traversal s t a b = forall f. Applicative f               => (a -> f b)           -> s -> f t
type Fold      s   a   = forall r. Monoid r                    => (a -> Const r a)     -> s -> Const r s
type Setter    s t a b =                                          (a -> Identity b)    -> s -> Identity t

data Exchange  a b s t = Exchange (s -> a) (b -> t)
type AnIso     s t a b = forall f. Functor f                   => Exchange a b a (f b) -> Exchange a b s (f t)

-- Lens < Getter
-- Lens < Traversal
-- Getter < Fold
-- Traversal < Fold
-- Traversal < Setter

type Lens'      s a = Lens      s s a a
type Traversal' s a = Traversal s s a a
type Setter'    s a = Setter    s s a a
type Iso'       s a = Iso       s s a a

data Point
  = Point Int Int
  deriving Show

data Rect
  = Rect Point Point
  deriving Show

data Posed a
  = Posed Point a
  deriving Show

pointX :: Lens' Point Int
pointX f (Point x y) = (\x' -> Point x' y) <$> f x

pointXY :: Traversal' Point Int
pointXY f (Point x y) = Point <$> f x <*> f y

pointIso :: Iso' Point (Int, Int)
pointIso = iso (\(Point x y) -> (x, y)) (uncurry Point)

rectTopLeft :: Lens' Rect Point
rectTopLeft f (Rect tl br) = (\tl' -> Rect tl' br) <$> f tl

posedVal :: Lens (Posed a) (Posed b) a b
posedVal f (Posed p a) = (\a' -> Posed p a') <$> f a

mapped :: Traversable t => Traversal (t a) (t b) a b
mapped = traverse

view :: Getter s a -> s -> a
view lens = getConst . lens Const

over :: Setter s t a b -> (a -> b) -> s -> t
over lens f = runIdentity . lens (Identity . f)

set :: Setter s t a b -> b -> s -> t
set lens = over lens . const

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso f g = dimap f $ fmap g

from :: AnIso s t a b -> Iso b a t s
from lens = iso (runIdentity . g) f where
  Exchange f g = lens $ Exchange id Identity

main :: IO ()
main = do
  let rect = Rect (Point 1 2) (Point 3 4)
      posed = Posed (Point 5 6) (7 :: Int)
      lst = [1, 2, 3] :: [Int]
  print $ view rectTopLeft rect
  print $ over (rectTopLeft . pointX) (+ 2) rect
  print $ set (rectTopLeft . pointXY) 0 rect
  print $ view (rectTopLeft . pointIso) rect
  print $ view (from pointIso) (3, 4)
  print $ over posedVal show posed
  print $ over mapped show lst
