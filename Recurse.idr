data Rec f = R (Rec f -> f)

recurse : Rec f -> f
recurse r@(R f) = f r

invoke : (Rec f -> f) -> f
invoke f = f (R f)

fac : Int -> Int
fac = invoke fac' where
  fac' : Rec (Int -> Int) -> Int -> Int
  fac' self 1 = 1
  fac' self n = n * recurse self (n - 1)

-- isEven, isOdd : Int -> Bool
-- (isEven, isOdd) = invoke $ \self -> let (isEven', isOdd') = recurse self in
--   ( \n -> n == 0 || isOdd' (n - 1)
--   , \n -> n /= 0 && isEven' (n - 1)
--   )

main : IO ()
main = do
  print $ fac 4
  -- print $ isEven 5
  -- print $ isOdd 5
