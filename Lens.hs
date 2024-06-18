{-# LANGUAGE RankNTypes #-}

import           Control.Applicative (Const (Const), getConst)
import           Data.Functor.Identity (Identity (Identity), runIdentity)
import Control.Arrow (ArrowChoice(left, right))
import Data.Foldable (traverse_)
import Data.Functor.Contravariant (Contravariant, phantom)
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Monoid (Endo (Endo, appEndo), First (First, getFirst))
import Data.Bifunctor (Bifunctor(bimap, first))

newtype Tagged s a = Tagged { unTagged :: a }

class Swap f where
  swap :: f a b -> f b a

instance Swap Either where
  swap (Left a) = Right a
  swap (Right a) = Left a

class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
  dimap f g = lmap f . rmap g
  lmap :: (a -> b) -> p b c -> p a c
  lmap = flip dimap id
  rmap :: (a -> b) -> p c a -> p c b
  rmap = dimap id
  {-# MINIMAL dimap | lmap, rmap #-}

instance Profunctor (->) where
  dimap l r p = r . p . l
  lmap = flip (.)
  rmap = (.)

class Profunctor p => Choice p where
  left' :: p a b -> p (Either a c) (Either b c)
  left' = dimap swap swap . right'
  right' :: p a b -> p (Either c a) (Either c b)
  right' = dimap swap swap . left'
  {-# MINIMAL left' | right' #-}

instance Choice (->) where
  left' = left
  right' = right

type Iso       s t a b = forall p f. (Profunctor p, Functor f)              => p a (f b)         -> p s (f t)
type Prism     s t a b = forall p f. (Choice p, Applicative f)              => p a (f b)         -> p s (f t)
type Review      t   b = forall p f. (Profunctor p, Bifunctor p, Functor f) => p b (f b)         -> p t (f t)
type Lens      s t a b = forall f. Functor f                                => (a -> f b)        -> s -> f t
type Getter    s   a   = forall f. (Functor f, Contravariant f)             => (a -> f a)        -> s -> f s
type Traversal s t a b = forall f. Applicative f                            => (a -> f b)        -> s -> f t
type Fold      s   a   = forall f. (Applicative f, Contravariant f)         => (a -> f a)        -> s -> f s
type Setter    s t a b =                                                       (a -> Identity b) -> s -> Identity t

data Exchange  a b s t = Exchange (s -> a) (b -> t)
data Market    a b s t = Market (b -> t) (s -> Either t a)

type Getting r s   a   = (a -> Const r a) -> s -> Const r s

type AnIso     s t a b = Exchange a b a (Identity b) -> Exchange a b s (Identity t)
type APrism    s t a b = Market   a b a (Identity b) -> Market   a b s (Identity t)
type AReview     t   b = Tagged       b (Identity b) -> Tagged       t (Identity t)
type ASetter   s t a b = Setter s t a b

type AGetter   s   a   = forall r. Getting r s a
type AFold     s   a   = forall r. Monoid r => Getting r s a

instance Profunctor (Exchange a b) where
  dimap l r (Exchange f g) = Exchange (f . l) (r . g)

instance Profunctor (Market a b) where
  dimap l r (Market g f) = Market (r . g) (left r . f . l)

instance Choice (Market a b) where
  right' (Market g f) = Market (Right . g) f'
    where
      f' = either (Left . Left) (left Right . f)

instance Functor (Tagged s) where
  fmap f (Tagged a) = Tagged (f a)

instance Profunctor Tagged where
  dimap _ r (Tagged a) = Tagged (r a)

instance Choice Tagged where
  right' (Tagged a) = Tagged (Right a)

instance Bifunctor Tagged where
  bimap _ r (Tagged a) = Tagged (r a)

-- Iso < Prism
-- Prism < Review
-- Lens < Getter
-- Lens < Traversal
-- Getter < Fold
-- Traversal < Fold
-- Traversal < Setter

type Iso'       s a = Iso       s s a a
type Prism'     s a = Prism     s s a a
type Lens'      s a = Lens      s s a a
type Traversal' s a = Traversal s s a a
type Setter'    s a = Setter    s s a a

type AnIso'     s a = AnIso     s s a a
type APrism'    s a = APrism    s s a a

biphantom :: (Profunctor p, Bifunctor p) => p a b -> p c b
biphantom = lmap (const ()) . first (const ())

-- # construction

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa (fmap bt)

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt sta = dimap sta (either pure $ fmap bt) . right'

unto :: (b -> t) -> Review t b
unto bt = biphantom . rmap (fmap bt)

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = sbt s <$> afb (sa s)

to :: (s -> a) -> Getter s a
to sa afa = phantom . afa . sa

sets :: ((a -> b) -> s -> t) -> Setter s t a b
sets abst aib = Identity . abst (runIdentity . aib)

traversed :: Traversable t => Traversal (t a) (t b) a b
traversed = traverse

folded :: Foldable f => Fold (f a) a
folded f = phantom . traverse_ f

both :: Bitraversable f => Traversal (f a a) (f b b) a b
both f = bitraverse f f

mapped :: Functor f => Setter (f a) (f b) a b
mapped = sets fmap

-- # elimination

views :: Getting r s a -> (a -> r) -> s -> r
views l f = getConst . l (Const . f)

foldMapOf :: Getting r s a -> (a -> r) -> s -> r
foldMapOf = views

view :: Getting a s a -> s -> a
view l = views l id

foldOf :: Getting a s a -> s -> a
foldOf = view

preview :: Getting (First a) s a -> s -> Maybe a
preview l = getFirst . foldMapOf l (First . Just)

toListOf :: Getting (Endo [a]) s a -> s -> [a]
toListOf l = flip appEndo [] . foldMapOf l (Endo . (:))

over :: ASetter s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)

set :: ASetter s t a b -> b -> s -> t
set l b = over l (const b)

review :: AReview t b -> b -> t
review l = runIdentity . unTagged . l . Tagged . Identity

-- # transformation

from :: AnIso s t a b -> Iso b a t s
from l = iso (runIdentity . g) f
  where
    Exchange f g = l $ Exchange id Identity

un :: Getting a s a -> Review a s
un l = unto $ view l

re :: AReview t b -> Getter b t
re l = to $ review l

-- # common lenses

_Identity :: Iso (Identity a) (Identity b) a b
_Identity = iso runIdentity Identity

_Just :: Prism (Maybe a) (Maybe b) a b
_Just = prism Just m
  where
    m (Just a) = Right a
    m Nothing = Left Nothing

_Nothing :: Prism' (Maybe a) ()
_Nothing = prism (const Nothing) m
  where
    m Nothing = Right ()
    m (Just a) = Left $ Just a

_Left :: Prism (Either a c) (Either b c) a b
_Left = prism Left m
  where
    m (Left a) = Right a
    m (Right b) = Left $ Right b

_Right :: Prism (Either c a) (Either c b) a b
_Right = prism Right m
  where
    m (Right a) = Right a
    m (Left b) = Left $ Left b

_1 :: Lens (a, c) (b, c) a b
_1 = lens fst (\(_, b) a -> (a, b))

_2 :: Lens (c, a) (c, b) a b
_2 = lens snd (\(a, _) b -> (a, b))

-- # example

main :: IO ()
main = do
  let lst = [1, 2, 3] :: [Int]
  let v = Right (1, 2) :: Either Int (Int, Int)
  print $ over traversed show lst
  print $ set (_Right . sets first . from _Identity) (Identity "foo") v
  print $ toListOf (_Right . both . to (+ 1) . re _Just) v
  print $ review (_Just . unto length . un _1) ("abc", "def")
  print $ preview (_Right . _1) v
