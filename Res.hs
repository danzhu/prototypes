import Prelude (IO, Int, const, foldr, id, map, print, ($), (+), (-), (.))

import qualified Prelude as P

data R t f
  = T t
  | F f
  deriving (P.Show)

class Log a where
  (&&) :: a -> a -> a
  (||) :: a -> a -> a

instance Log (R t f) where
  _ && F b = F b
  a && _   = a

  T a || _ = T a
  _   || b = b

instance (Log b) => Log (a -> b) where
  (&&) a b c = a c && b c
  (||) a b c = a c || b c

(>>=) :: R t1 f -> (t1 -> R t2 f) -> R t2 f
T t >>= f = f t
F f >>= _ = F f

(<) :: Int -> Int -> R Int ()
a < b
  | a P.< b     = T a
  | P.otherwise = F ()

(>) :: Int -> Int -> R Int ()
a > b
  | a P.> b     = T a
  | P.otherwise = F ()

and :: [R () f] -> R () f
and = foldr (&&) (T ())

or :: [R t ()] -> R t ()
or = foldr (||) (F ())

find :: (a -> R t ()) -> [a] -> R t ()
find f = or . map f

rtf :: (t -> a) -> (f -> a) -> R t f -> a
rtf a _ (T t) = a t
rtf _ b (F f) = b f

rts :: [R t f] -> [t]
rts = foldr (rtf (:) (const id)) []

rfs :: [R t f] -> [f]
rfs = foldr (rtf (const id) (:)) []

filter :: (a -> R t f) -> [a] -> [t]
filter f = rts . map f

main :: IO ()
main = do
  let l = [0..5]
  print $ find ((> 2) && (> 3)) l >>= (< 5)
  print $ filter ((< 2) || (> 3)) l
