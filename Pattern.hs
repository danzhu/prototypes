{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE PolyKinds              #-}

import           Control.Applicative (Alternative, empty)
import           Control.Arrow ((***), (+++))
import           Control.Category (Category, id, (.))
import           Control.Monad ((<=<))
import           Data.Functor.Contravariant (Contravariant, contramap)
import           Data.Functor.Identity (Identity, runIdentity)
import           Prelude hiding (id, (.))


infixr 3 |*|
infixr 2 |+|

class IsoArrow f where
  isoArr :: (r -> a) -> (a -> m r) -> f m r a
  (|*|) :: Applicative m => f m r a -> f m s b -> f m (r, s) (a, b)
  (|+|) :: Functor m => f m r a -> f m s b -> f m (Either r s) (Either a b)


newtype Fun m r a = Fun { new :: r -> a }

instance IsoArrow Fun where
  isoArr f _ = Fun f
  p |*| q = Fun $ new p *** new q
  p |+| q = Fun $ new p +++ new q


newtype Pat m r a = Pat { match :: a -> m r }

instance Contravariant (Pat m r) where
  contramap f p = Pat $ match p . f

instance Monad m => Category (Pat m) where
  id = Pat $ pure . id
  p . q = Pat $ match q <=< match p

instance IsoArrow Pat where
  isoArr _ f = Pat f
  p |*| q = Pat $ \(a, b) -> (,) <$> match p a <*> match q b
  p |+| q = Pat $ \case
    Left a -> Left <$> match p a
    Right b -> Right <$> match q b


ret :: Applicative m => Pat Identity r a -> Pat m r a
ret p = Pat $ pure . runIdentity . match p

lift :: Applicative m => Pat f r a -> Pat m (f r) a
lift p = Pat $ pure . match p

var :: (IsoArrow f, Applicative m) => f m a a
var = isoArr id pure

unit :: (IsoArrow f, Applicative m) => f m () ()
unit = isoArr id pure

eq :: (IsoArrow f, Eq a, Alternative m) => a -> f m () a
eq x = isoArr (const x) $ \a -> if x == a then pure () else empty

ign :: Applicative m => Pat m () a
ign = Pat $ const $ pure ()

test :: IsoArrow f => f Maybe (Int, Either () ()) (Int, Either Int ())
test = var |*| (eq 0 |+| unit)

main :: IO ()
main = do
  print $ match test (2, Right ())
  print $ new test (3, Left ())
