module Main
  ( main
  ) where

import           Control.Monad.Trans.Cont
import           Control.Monad.IO.Class         ( liftIO )

bracket :: IO a -> IO () -> ContT r IO a
bracket open close = do
  r <- liftIO open
  ContT $ \c -> c r <* close

resource :: Int -> ContT r IO Int
resource r = bracket alloc free where
  alloc = do
    putStrLn $ "alloc " ++ show r
    pure r
  free = putStrLn $ "free " ++ show r

sub :: ContT r IO Int
sub = do
  r <- resource 3
  liftIO $ putStrLn $ "sub " ++ show r
  resource 4

main :: IO ()
main = flip runContT pure $ do
  r <- (+) <$> resource 1 <*> resource 2
  liftIO $ putStrLn $ "r = " ++ show r
  s <- sub
  liftIO $ putStrLn $ "s = " ++ show s
  t <- resetT sub
  liftIO $ putStrLn $ "t = " ++ show t
