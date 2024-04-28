{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

import Control.Applicative (Const (Const, getConst))
import Control.Arrow (ArrowChoice (left, right))
import Data.Bifunctor (Bifunctor (bimap, first, second))
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Monoid
  ( All (All, getAll),
    Any (Any, getAny),
    Endo (Endo, appEndo),
    First (First, getFirst),
  )
import Data.Void (absurd)
import Data.Functor.Contravariant (Contravariant (contramap), phantom)

class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
  dimap f g = lmap f . rmap g
  lmap :: (a -> b) -> p b c -> p a c
  lmap = flip dimap id
  rmap :: (a -> b) -> p c a -> p c b
  rmap = dimap id
  {-# MINIMAL dimap | lmap, rmap #-}

class Bicontravariant p where
  contrabimap :: (b -> a) -> (d -> c) -> p a c -> p b d
  contrabimap l r = contrafirst l . contrasecond r
  contrafirst :: (b -> a) -> p a c -> p b c
  contrafirst l = contrabimap l id
  contrasecond :: (d -> c) -> p a c -> p a d
  contrasecond = contrabimap id
  {-# MINIMAL contrabimap | contrafirst, contrasecond #-}

class Profunctor p => Choice p where
  left' :: p a b -> p (Either a c) (Either b c)
  left' = diswap . right'
  right' :: p a b -> p (Either c a) (Either c b)
  right' = diswap . left'
  {-# MINIMAL left' | right' #-}

class Profunctor p => Cochoice p where
  unleft :: p (Either a c) (Either b c) -> p a b
  unleft = unright . diswap
  unright :: p (Either c a) (Either c b) -> p a b
  unright = unleft . diswap
  {-# MINIMAL unleft | unright #-}

class Profunctor p => Strong p where
  first' :: p a b -> p (a, c) (b, c)
  first' = diswap . second'
  second' :: p a b -> p (c, a) (c, b)
  second' = diswap . first'
  {-# MINIMAL first' | second' #-}

class Profunctor p => Costrong p where
  unfirst :: p (a, c) (b, c) -> p a b
  unfirst = unsecond . diswap
  unsecond :: p (c, a) (c, b) -> p a b
  unsecond = unfirst . diswap
  {-# MINIMAL unfirst | unsecond #-}

class Profunctor p => Closed p where
  closed :: p a b -> p (x -> a) (x -> b)

class (Choice p, Strong p) => Traversing p where
  wander :: (forall f. Applicative f => (a -> f b) -> s -> f t) -> p a b -> p s t
  traverse' :: Traversable f => p a b -> p (f a) (f b)
  traverse' = wander traverse

class (Traversing p, Closed p) => Mapping p where
  roam :: ((a -> b) -> s -> t) -> p a b -> p s t
  map' :: Functor f => p a b -> p (f a) (f b)
  map' = roam fmap

type Optic    p s t a b = p a b -> p s t
type Optic'   p s   a   = p a a -> p s s

type Iso        s t a b = forall p. (Profunctor p)                    => Optic  p s t a b
type Prism      s t a b = forall p. (Choice p)                        => Optic  p s t a b
type Lens       s t a b = forall p. (Strong p)                        => Optic  p s t a b
type Grate      s t a b = forall p. (Closed p)                        => Optic  p s t a b
type Traversal  s t a b = forall p. (Traversing p)                    => Optic  p s t a b
type Setter     s t a b = forall p. (Mapping p)                       => Optic  p s t a b
type Getter     s   a   = forall p. (Profunctor p, Bicontravariant p) => Optic' p s   a
type Fold       s   a   = forall p. (Traversing p, Bicontravariant p) => Optic' p s   a
type Review       t   b = forall p. (Profunctor p, Bifunctor p)       => Optic' p   t   b

data Exchange   a b s t = Exchange (s -> a) (b -> t)
data Market     a b s t = Market   (b -> t) (s -> Either t a)
data Store      a b s t = Store    (s -> a) (s -> b -> t)
newtype Well    a b s t = Well     { runWell   :: ((s -> a) -> b) -> t }
newtype Star      f a b = Star     { runStar   :: a -> f b }
newtype Forget    r a b = Forget   { runForget :: a -> r }
newtype Tagged      a b = Tagged   { unTagged  :: b }
newtype Re    p s t a b = Re       { unRe      :: p b a -> p t s }

type AnIso      s t a b = Optic  (Exchange a b) s t a b
type APrism     s t a b = Optic  (Market a b)   s t a b
type ALens      s t a b = Optic  (Store a b)    s t a b
type AGrate     s t a b = Optic  (Well a b)     s t a b
type ASetter    s t a b = Optic  (->)           s t a b
type LensLike f s t a b = Optic  (Star f)       s t a b
type Getting  r s   a   = Optic' (Forget r)     s   a
type AReview      t   b = Optic' Tagged           t   b

type Iso'        s a = Iso        s s a a
type Prism'      s a = Prism      s s a a
type Lens'       s a = Lens       s s a a
type Grate'      s a = Grate      s s a a
type Traversal'  s a = Traversal  s s a a
type Setter'     s a = Setter     s s a a
type AnIso'      s a = AnIso      s s a a
type APrism'     s a = APrism     s s a a
type ALens'      s a = ALens      s s a a
type AGrate'     s a = AGrate     s s a a
type ASetter'    s a = ASetter    s s a a

-- Exchange

instance Profunctor (Exchange a b) where
  dimap :: (u -> s) -> (t -> v) -> Exchange a b s t -> Exchange a b u v
  dimap us tv (Exchange sa bt) = Exchange (sa . us) (tv . bt)

-- Market

instance Profunctor (Market a b) where
  dimap :: (u -> s) -> (t -> v) -> Market a b s t -> Market a b u v
  dimap us tv (Market bt sta) = Market (tv . bt) (left tv . sta . us)

instance Choice (Market a b) where
  left' :: Market a b s t -> Market a b (Either s u) (Either t u)
  left' (Market bt sta) = Market (Left . bt) sta'
    where
      sta' = either (left Left . sta) (Left . Right)
  right' :: Market a b s t -> Market a b (Either u s) (Either u t)
  right' (Market bt sta) = Market (Right . bt) sta'
    where
      sta' = either (Left . Left) (left Right . sta)

-- Store

instance Profunctor (Store a b) where
  dimap :: (u -> s) -> (t -> v) -> Store a b s t -> Store a b u v
  dimap us tv (Store sa sbt) = Store (sa . us) (\u -> tv . sbt (us u))

instance Strong (Store a b) where
  first' :: Store a b s t -> Store a b (s, u) (t, u)
  first' (Store sa sbt) = Store (sa . fst) (\(s, u) b -> (sbt s b, u))
  second' :: Store a b s t -> Store a b (u, s) (u, t)
  second' (Store sa sbt) = Store (sa . snd) (\(u, s) b -> (u, sbt s b))

-- Well

instance Profunctor (Well a b) where
  dimap :: (u -> s) -> (t -> v) -> Well a b s t -> Well a b u v
  dimap us tv (Well sabt) = Well $ \uab -> tv $ sabt $ \sa -> uab (sa . us)

instance Closed (Well a b) where
  closed :: Well a b s t -> Well a b (x -> s) (x -> t)
  closed (Well sabt) = Well $ \xsab x -> sabt $ \sa -> xsab $ sa . ($ x)

-- Star

instance Functor f => Profunctor (Star f) where
  dimap l r (Star f) = Star $ fmap r . f . l

instance Contravariant f => Bicontravariant (Star f) where
  contrabimap ba dc (Star f) = Star $ contramap dc . f . ba

instance Applicative f => Choice (Star f) where
  left' (Star f) = Star $ either (fmap Left . f) (pure . Right)
  right' (Star f) = Star $ either (pure . Left) (fmap Right . f)

instance Traversable f => Cochoice (Star f) where
  unright (Star f) = Star $ go . Right
    where
      go = either (go . Left) id . sequence . f

instance Functor f => Strong (Star f) where
  first' (Star f) = Star $ \(a, c) -> (,c) <$> f a
  second' (Star f) = Star $ \(c, a) -> (c,) <$> f a

instance Applicative f => Traversing (Star f) where
  wander t (Star f) = Star $ t f

-- Forget

instance Profunctor (Forget r) where
  dimap l _ (Forget f) = Forget $ f . l

instance Bicontravariant (Forget r) where
  contrabimap ba _ (Forget f) = Forget $ f . ba

instance Monoid r => Choice (Forget r) where
  left' (Forget f) = Forget $ either f mempty
  right' (Forget f) = Forget $ either mempty f

instance Cochoice (Forget r) where
  unleft (Forget f) = Forget $ f . Left
  unright (Forget f) = Forget $ f . Right

instance Strong (Forget r) where
  first' (Forget f) = Forget $ f . fst
  second' (Forget f) = Forget $ f . snd

instance Monoid r => Traversing (Forget r) where
  wander t (Forget f) = Forget $ getConst . t (Const . f)
  traverse' (Forget f) = Forget $ foldMap f

-- Tagged

instance Profunctor Tagged where
  dimap _ r (Tagged b) = Tagged $ r b

instance Bifunctor Tagged where
  bimap _ r (Tagged b) = Tagged $ r b

instance Choice Tagged where
  left' (Tagged b) = Tagged $ Left b
  right' (Tagged b) = Tagged $ Right b

instance Costrong Tagged where
  unfirst (Tagged b) = Tagged $ fst b
  unsecond (Tagged b) = Tagged $ snd b

instance Closed Tagged where
  closed (Tagged b) = Tagged $ const b

-- Re

instance Profunctor p => Profunctor (Re p s t) where
  dimap l r (Re p) = Re $ p . dimap r l

instance Bicontravariant p => Bifunctor (Re p s t) where
  bimap l r (Re p) = Re $ p . contrabimap r l

instance Bifunctor p => Bicontravariant (Re p s t) where
  contrabimap l r (Re p) = Re $ p . bimap r l

instance Cochoice p => Choice (Re p s t) where
  left' (Re p) = Re $ p . unleft
  right' (Re p) = Re $ p . unright

instance Choice p => Cochoice (Re p s t) where
  unleft (Re p) = Re $ p . left'
  unright (Re p) = Re $ p . right'

instance Costrong p => Strong (Re p s t) where
  first' (Re p) = Re $ p . unfirst
  second' (Re p) = Re $ p . unsecond

instance Strong p => Costrong (Re p s t) where
  unfirst (Re p) = Re $ p . first'
  unsecond (Re p) = Re $ p . second'

instance (Profunctor p, Bicontravariant p) => Closed (Re p s t) where
  closed (Re p) = Re $ p . rphantom . lmap const

-- ->

instance Profunctor (->) where
  dimap l r f = r . f . l
  lmap = flip (.)
  rmap = (.)

instance Choice (->) where
  left' = left
  right' = right

instance Cochoice (->) where
  unleft f = go . Left
    where
      go = either id (go . Right) . f
  unright f = go . Right
    where
      go = either (go . Left) id . f

instance Strong (->) where
  first' = first
  second' = second

instance Costrong (->) where
  unfirst f a = b
    where
      (b, c) = f (a, c)
  unsecond f a = b
    where
      (c, b) = f (c, a)

instance Closed (->) where
  closed = (.)

instance Traversing (->) where
  wander t ab = runIdentity . t (Identity . ab)
  traverse' = fmap

instance Mapping (->) where
  roam = id
  map' = fmap

-- # helpers

class Swap f where
  swap :: f a b -> f b a

instance Swap Either where
  swap = either Right Left

instance Swap (,) where
  swap (a, b) = (b, a)

diswap :: (Profunctor p, Swap f) => p (f a b) (f s t) -> p (f b a) (f t s)
diswap = dimap swap swap

lphantom :: (Profunctor p, Bifunctor p) => p a b -> p c b
lphantom = first absurd . lmap absurd

rphantom :: (Profunctor p, Bicontravariant p) => p a b -> p a c
rphantom = rmap absurd . contrasecond absurd

-- # introduction

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa bt

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt sta = dimap sta (either id bt) . right'

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt = dimap (\s -> (s, sa s)) (uncurry sbt) . second'

grate :: (((s -> a) -> b) -> t) -> Grate s t a b
grate sabt = dimap (&) sabt . closed

traversal :: (forall f. Applicative f => (a -> f b) -> s -> f t) -> Traversal s t a b
traversal f = wander f

traversed :: Traversable t => Traversal (t a) (t b) a b
traversed = traverse'

sets :: ((a -> b) -> s -> t) -> Setter s t a b
sets abst = roam abst

mapped :: Functor f => Setter (f a) (f b) a b
mapped = map'

unto :: (b -> t) -> Review t b
unto bt = lphantom . rmap bt

to :: (s -> a) -> Getter s a
to sa = rphantom . lmap sa

folded :: Foldable f => Fold (f a) a
folded = rphantom . wander traverse_

-- # elimination

withIso :: AnIso s t a b -> ((s -> a) -> (b -> t) -> r) -> r
withIso o c = c sa bt
  where
    Exchange sa bt = o $ Exchange id id

withPrism :: APrism s t a b -> ((b -> t) -> (s -> Either t a) -> r) -> r
withPrism o c = c bt sta
  where
    Market bt sta = o $ Market id Right

withLens :: ALens s t a b -> ((s -> a) -> (s -> b -> t) -> r) -> r
withLens o c = c sa sbt
  where
    Store sa sbt = o $ Store id (const id)

degrate :: AGrate s t a b -> ((s -> a) -> b) -> t
degrate o = runWell . o $ Well ($ id)

traverseOf :: LensLike f s t a b -> (a -> f b) -> s -> f t
traverseOf o = runStar . o . Star

over :: ASetter s t a b -> (a -> b) -> s -> t
over = id

foldMapOf :: Getting r s a -> (a -> r) -> s -> r
foldMapOf o = runForget . o . Forget

review :: AReview t b -> b -> t
review o = unTagged . o . Tagged

re :: Optic (Re p a b) s t a b -> Optic p b a t s
re o = unRe . o $ Re id

-- # combinators

cloneIso :: AnIso s t a b -> Iso s t a b
cloneIso o = withIso o $ \sa bt -> iso sa bt

under :: AnIso s t a b -> (t -> s) -> b -> a
under o ts = withIso o $ \sa bt -> sa . ts . bt

clonePrism :: APrism s t a b -> Prism s t a b
clonePrism o = withPrism o $ \bt sta -> prism bt sta

cloneLens :: ALens s t a b -> Lens s t a b
cloneLens o = withLens o $ \sa sbt -> lens sa sbt

cloneGrate :: AGrate s t a b -> Grate s t a b
cloneGrate o = grate $ degrate o

cloneReview :: AReview t b -> Review t b
cloneReview o = unto $ review o

cloneGetter :: Getting a s a -> Getter s a
cloneGetter o = to $ view o

cloneSetter :: ASetter s t a b -> Setter s t a b
cloneSetter o = sets $ over o

forOf :: LensLike f s t a b -> s -> (a -> f b) -> f t
forOf = flip . traverseOf

sequenceOf :: LensLike f s t (f a) a -> s -> f t
sequenceOf o = traverseOf o id

set :: ASetter s t a b -> b -> s -> t
set o = over o . const

foldOf :: Getting a s a -> s -> a
foldOf o = foldMapOf o id

views :: Getting r s a -> (a -> r) -> s -> r
views = foldMapOf

view :: Getting a s a -> s -> a
view = foldOf

previews :: Getting (First r) s a -> (a -> r) -> s -> Maybe r
previews o f = getFirst . foldMapOf o (First . Just . f)

preview :: Getting (First a) s a -> s -> Maybe a
preview o = previews o id

has :: Getting Any s a -> s -> Bool
has o = getAny . foldMapOf o (const $ Any True)

hasn't :: Getting All s a -> s -> Bool
hasn't o = getAll . foldMapOf o (const $ All False)

foldrOf :: Getting (Endo r) s a -> (a -> r -> r) -> r -> s -> r
foldrOf o f r = flip appEndo r . foldMapOf o (Endo . f)

toListOf :: Getting (Endo [a]) s a -> s -> [a]
toListOf o = foldrOf o (:) []

traverseOf_ :: Applicative f => Getting (Endo (f ())) s a -> (a -> f b) -> s -> f ()
traverseOf_ o f = foldrOf o (\a -> (f a *>)) (pure ())

forOf_ :: Applicative f => Getting (Endo (f ())) s a -> s -> (a -> f b) -> f ()
forOf_ = flip . traverseOf_

sequenceOf_ :: Applicative f => Getting (Endo (f ())) s (f a) -> s -> f ()
sequenceOf_ o = traverseOf_ o id

-- # common lenses

_Identity :: Iso (Identity a) (Identity b) a b
_Identity = iso runIdentity Identity

swapped :: Swap f => Iso (f a b) (f c d) (f b a) (f d c)
swapped = iso swap swap

_Just :: Prism (Maybe a) (Maybe b) a b
_Just = prism Just $ maybe (Left Nothing) Right

_Nothing :: Prism' (Maybe a) ()
_Nothing = prism (const Nothing) $ maybe (Right ()) (Left . Just)

_Left :: Prism (Either a c) (Either b c) a b
_Left = prism Left $ either Right (Left . Right)

_Right :: Prism (Either c a) (Either c b) a b
_Right = prism Right $ either (Left . Left) Right

_1 :: Lens (a, c) (b, c) a b
_1 = lens fst (\(_, b) a -> (a, b))

_2 :: Lens (c, a) (c, b) a b
_2 = lens snd (\(a, _) b -> (a, b))

ret :: Grate (x -> a) (x -> b) a b
ret = grate $ \xaab x -> xaab ($ x)

both :: Bitraversable f => Traversal (f a a) (f b b) a b
both = traversal (\f -> bitraverse f f)

-- # example

main :: IO ()
main = do
  let lst = [1, 2, 3] :: [Int]
  let v = Right (1, 2) :: Either Int (Int, Int)
      plus1 x = x + 1 :: Int
      _identity :: Iso (Identity a) (Identity b) a b
      _identity = cloneIso _Identity
      _just :: Prism (Maybe a) (Maybe b) a b
      _just = clonePrism _Just
      _first :: Lens (a, c) (b, c) a b
      _first = cloneLens _1
      _ret :: Grate (x -> a) (x -> b) a b
      _ret = cloneGrate ret
  print $ over mapped show lst
  print $ under swapped (first length) ("foo", "bar")
  print $ set (_Right . sets first . re _Identity) (Identity "foo") v
  print $ toListOf (_Right . both . to (+ 1) . re _Just) v
  print $ review (_Just . unto length . re _1) ("abc", "def")
  print $ preview (_Right . _1) v
  print $ over ret (+ 9) plus1 2
  print $ over (_Right . _1 . re _Right) (either Right $ Left . (+ 20)) v
  v' <- forOf (traversed . both) v $ \a -> do
    print a
    pure $ -a
  print v'
  traverseOf_ (folded . both) print v
