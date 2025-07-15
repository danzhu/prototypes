Rec f = Lazy f -> f

rec : Rec ff -> ff
rec r = f
  where
    f : ff
    f = r f

test : Rec (Nat -> Nat)
test self 0 = 0
test self (S n) = n + self n

main : IO ()
main = printLn $ rec test 10
