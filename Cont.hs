import           Control.Monad (when)
import           Data.Foldable (for_)
import           Data.Tree (Tree (Node))

newtype Cont r a = Cont { runCont :: (a -> r) -> r }

instance Functor (Cont r) where
  fmap f m = Cont $ \c -> runCont m $ c . f

instance Applicative (Cont r) where
  pure a = Cont ($ a)
  f <*> v = Cont $ \c -> runCont f $ \f' -> runCont v $ c . f'

instance Monad (Cont r) where
  m >>= f = Cont $ \c -> runCont m $ \a -> runCont (f a) c

evalCont :: Cont r r -> r
evalCont = flip runCont id

callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC f = Cont $ \c -> runCont (f $ \a -> Cont $ \_ -> c a) c

reset :: Cont r r -> Cont r' r
reset = pure . evalCont

shift :: ((a -> r) -> Cont r r) -> Cont r a
shift f = Cont $ evalCont . f

search :: (a -> Bool) -> Tree a -> Maybe a
search p t = evalCont $ callCC $ \done -> do
  let search' (Node a ts) = do
        when (p a) $ done $ Just a
        for_ ts search'
  search' t
  pure Nothing

main :: IO ()
main = do
  let t = Node 1 [Node 2 [t], Node 3 []] :: Tree Int
  print $ search (== 2) t
