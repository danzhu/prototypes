import           Control.Applicative (Alternative, empty, liftA2, (<|>))
import           Control.Arrow (first, (***), (|||))
import           Control.Category (Category, id, (.))
import           Control.Monad (MonadPlus, mfilter, void)
import           Data.Char (isAlpha)
import           Data.List (uncons)
import           Data.List.NonEmpty (NonEmpty ((:|)))
import           Prelude hiding (id, print, (.))


-- Iso, Syntax

data Iso a b = Iso
  { apply  :: a -> b
  , revert :: b -> a
  }

instance Category Iso where
  id = Iso id id

  f . g = Iso (apply f . apply g) (revert g . revert f)

infixl 4 <??>
infixl 4 <$$>
infixr 3 <**>
infixr 2 <++>

class Syntax f where
  (<??>) :: (a -> Bool) -> f a -> f a
  (<$$>) :: Iso a b -> f a -> f b
  (<**>) :: f a -> f b -> f (a, b)
  (<++>) :: f a -> f b -> f (Either a b)
  expect :: Eq a => f a -> a -> f ()
  unit :: f ()
  token :: f Char


-- Parser

data Parser a = Parser
  { parse :: String -> Maybe (a, String)
  }

instance Functor Parser where
  fmap f p = Parser par where
    par s = first f <$> parse p s

instance Applicative Parser where
  pure a = Parser par where
    par s = pure (a, s)

  liftA2 f p q = Parser par where
    par s = do
      (a, s') <- parse p s
      (b, s'') <- parse q s'
      pure (f a b, s'')

instance Alternative Parser where
  empty = Parser $ const Nothing

  p <|> q = Parser par where
    par s = parse p s <|> parse q s

instance Monad Parser where
  p >>= f = Parser par where
    par s = do
      (a, s') <- parse p s
      parse (f a) s'

instance MonadPlus Parser

instance Syntax Parser where
  (<??>) = mfilter

  (<$$>) = fmap . apply

  (<**>) = liftA2 (,)

  p <++> q = Left <$> p <|> Right <$> q

  expect p a = void $ (== a) <??> p

  unit = pure ()

  token = Parser uncons


-- Printer

data Printer a = Printer
  { print :: a -> String
  }

instance Syntax Printer where
  (<??>) = const id

  f <$$> p = Printer $ print p . revert f

  p <**> q = Printer $ uncurry (++) . (print p *** print q)

  p <++> q = Printer $ print p ||| print q

  expect p a = Printer $ const $ print p a

  unit = Printer $ const []

  token = Printer (: [])


-- util, combinators

class Swappable s where
  swap :: s a b -> s b a

instance Swappable (,) where
  swap (a, b) = (b, a)

instance Swappable Either where
  swap (Left a)  = Right a
  swap (Right a) = Left a


isoSwap :: Swappable s => Iso (s a b) (s b a)
isoSwap = Iso swap swap

isoUnit :: Iso ((), a) a
isoUnit = Iso to from where
  to ((), a) = a
  from a = ((), a)

isoList :: Iso (Either () (NonEmpty a)) [a]
isoList = Iso to from where
  to (Left ())         = []
  to (Right (x :| xs)) = x : xs
  from []       = Left ()
  from (x : xs) = Right (x :| xs)

isoNonEmpty :: Iso (a, [a]) (NonEmpty a)
isoNonEmpty = Iso to from where
  to = uncurry (:|)
  from (x :| xs) = (x, xs)

single :: Syntax f => Char -> f ()
single = expect token

many :: Syntax f => f a -> f [a]
many syn = isoList . isoSwap <$$> (some syn <++> unit)

some :: Syntax f => f a -> f (NonEmpty a)
some syn = isoNonEmpty <$$> (syn <**> many syn)

(**>) :: Syntax f => f () -> f a -> f a
s **> t = isoUnit <$$> (s <**> t)

(<**) :: Syntax f => f a -> f () -> f a
s <** t = isoUnit . isoSwap <$$> (s <**> t)

between :: Syntax f => f () -> f () -> f a -> f a
between a b p = a **> p <** b


-- scheme

type Symbol = NonEmpty Char

data Exp
  = Symbol Symbol
  | Paren [Exp]
  deriving (Show, Eq)

paren :: Syntax f => f a -> f a
paren = between (single '(') (single ')')

synSymbol :: Syntax f => f Symbol
synSymbol = some $ isAlpha <??> token

isoExp :: Iso (Either Symbol [Exp]) Exp
isoExp = Iso to from where
  to (Left s)  = Symbol s
  to (Right l) = Paren l
  from (Symbol s) = Left s
  from (Paren l)  = Right l

synExp :: Syntax f => f Exp
synExp = isoExp <$$> (synSymbol <++> paren (many synExp)) where
