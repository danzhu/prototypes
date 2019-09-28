{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

class Pipe a b c d | a b c -> d, a b d -> c, a c d -> b, b c d -> a where
  (<|) :: (a -> b) -> c -> d

instance Pipe a b a b where
  (<|) = id

instance (Pipe a b c d, Functor f) => Pipe a b (f c) (f d) where
  (<|) = fmap . (<|)

showInt :: Int -> String
showInt = show

testList :: [[Int]] -> [[String]]
testList = (showInt <|)

testFunc :: Int -> Int -> String
testFunc = showInt <| (+)

testApp :: String
testApp = showInt <| (1 :: Int)

main :: IO ()
main = do
  print $ testList [[1, 2], [3, 4]]
  print $ testFunc 1 2
  print $ testApp
