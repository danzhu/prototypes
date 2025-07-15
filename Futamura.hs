-- target language
data T a

-- source code
data S a

-- optimized code
data O a

run :: S a -> a
run = undefined

src :: T (Int -> Bool)
src = undefined

interp :: S ((T (a -> b), a) -> b)
interp = undefined

spec :: S ((S ((a, b) -> c), a) -> O (b -> c))
spec = undefined

proj1 :: O (Int -> Bool)
proj1 = run spec (interp, src)

proj2 :: O (T (a -> b) -> O (a -> b))
proj2 = run spec (spec, interp)

proj3 :: O (S ((a, b) -> c) -> O (a -> O (b -> c)))
proj3 = run spec (spec, spec)

main :: IO ()
main = pure ()
