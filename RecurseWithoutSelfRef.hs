newtype Rec f = Rec (Rec f -> f)

recurse :: Rec f -> f
recurse r@(Rec f) = f r

recursive :: (Rec f -> f) -> f
recursive f = f (Rec f)

fac :: Int -> Int
fac = recursive $ \self n -> case n of
  1 -> 1
  _ -> n * recurse self (n - 1)

isEven, isOdd :: Int -> Bool
(isEven, isOdd) = recursive $ \self -> let (isEven', isOdd') = recurse self in
  ( \n -> n == 0 || isOdd' (n - 1)
  , \n -> n /= 0 && isEven' (n - 1)
  )

main :: IO ()
main = do
  print $ fac 4
  print $ isEven 5
  print $ isOdd 5
