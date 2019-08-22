import           Control.Applicative (liftA2)

newtype Pat a r = Pat { match :: a -> Maybe r }

pair :: Pat a r -> Pat b s -> Pat (a, b) (r, s)
pair p q = Pat $ \(a, b) -> liftA2 (,) (match p a) (match q b)

var :: Pat a a
var = Pat Just

eq :: Eq a => a -> Pat a ()
eq x = Pat $ \a -> if x == a then Just () else Nothing

ign :: Pat a ()
ign = Pat $ const $ Just ()

test :: Pat (Int, Int) (Int, ())
test = pair var (eq 1)

res :: Maybe (Int, ())
res = match test (2, 1)
