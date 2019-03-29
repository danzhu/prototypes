{-# LANGUAGE RankNTypes #-}

import Control.Applicative ((<|>))
import Data.Functor.Identity (runIdentity)
import Debug.Trace

data StepT m a = StepT { unStepT :: a, runStepT :: m (Maybe a) }

instance Functor m => Functor (StepT m) where
  fmap f (StepT a b) = StepT (f a) $ fmap f <$> b

instance Monad m => Applicative (StepT m) where
  pure a = StepT a $ pure Nothing
  StepT f g <*> ~(StepT a b) = StepT (f a) $ do
    g' <- g
    case g' of
      Just f' -> pure $ Just $ f' a
      Nothing -> fmap f <$> b

instance Monad m => Monad (StepT m) where
  StepT a b >>= f =
    let ~(StepT c d) = f a
        d' = do
          b' <- b
          case b' of
            Just a' -> pure $ Just $ unStepT $ f a'
            Nothing -> d
    in StepT c d'

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type Traversal' s a = Traversal s s a a

step :: Monad f => Traversal' s a -> (a -> f (Maybe a)) -> s -> f (Maybe s)
step tra f s = runStepT $ tra f' s
  where f' a = StepT a $ f a

main :: IO ()
main = do
  let l = [1..]
      f n | traceShow n False = undefined
      f 4 = Just 2
      f 8 = Just 4
      f _ = Nothing
  print $ take 10 <$> runIdentity (step traverse (pure . f) l)
  let g n = do
        putStr $ show n ++ ": "
        line <- getLine
        case line of
          "" -> pure Nothing
          _  -> pure $ Just $ read line
  l' <- step traverse g l
  print $ take 10 <$> l'
