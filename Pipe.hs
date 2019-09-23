{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

class Pipe a b c d where
  lift :: (a -> b) -> c -> d

instance Pipe a b a b where
  lift = id

instance (Pipe a b c d, Functor f) => Pipe a b (f c) (f d) where
  lift = fmap . lift

test :: [[Int]] -> [[Int]]
test = lift add1 where
  add1 = (+ 1) :: Int -> Int

main :: IO ()
main = do
  print $ test [[1, 2], [3, 4]]
