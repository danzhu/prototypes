{-# LANGUAGE RankNTypes #-}

import Control.Applicative (Const (Const), getConst)
import Data.Functor.Identity (Identity (Identity), runIdentity)

type Lens      s t a b = forall f. Functor f     => (a -> f b)        -> s -> f t
type Getter    s   a   = forall r.                  (a -> Const r a)  -> s -> Const r s
type Traversal s t a b = forall f. Applicative f => (a -> f b)        -> s -> f t
type Fold      s   a   = forall r. Monoid r      => (a -> Const r a)  -> s -> Const r s
type Setter    s t a b =                            (a -> Identity b) -> s -> Identity t

-- Lens < Getter
-- Lens < Traversal
-- Getter < Fold
-- Traversal < Fold
-- Traversal < Setter

type Lens' s a      = Lens s s a a
type Traversal' s a = Traversal s s a a
type Setter' s a    = Setter s s a a

data Point = Point Int Int
           deriving Show

data Rect = Rect Point Point
          deriving Show

data Posed a = Posed Point a
             deriving Show

pointX :: Lens' Point Int
pointX f (Point x y) = (\x' -> Point x' y) <$> f x

pointXY :: Traversal' Point Int
pointXY f (Point x y) = Point <$> f x <*> f y

rectTopLeft :: Lens' Rect Point
rectTopLeft f (Rect tl br) = (\tl' -> Rect tl' br) <$> f tl

posedVal :: Lens (Posed a) (Posed b) a b
posedVal f (Posed p a) = (\a' -> Posed p a') <$> f a

mapped :: Traversable t => Traversal (t a) (t b) a b
mapped = traverse

view :: Getter s a -> s -> a
view lens s = getConst $ lens Const s

over :: Setter s t a b -> (a -> b) -> s -> t
over lens f s = runIdentity $ lens (Identity . f) s

set :: Setter s t a b -> b -> s -> t
set lens b = over lens (const b)

main :: IO ()
main = do
  let rect = Rect (Point 1 2) (Point 3 4)
      posed = Posed (Point 5 6) 7
      lst = [1, 2, 3]
  print $ view rectTopLeft rect
  print $ over (rectTopLeft . pointX) (+ 2) rect
  print $ set (rectTopLeft . pointXY) 0 rect
  print $ over posedVal show posed
  print $ over mapped show lst
