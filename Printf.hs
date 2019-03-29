{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

type Accum = String -> String
type Format = String

class PrintfType r where
  format :: Accum -> Format -> r

instance PrintfType String where
  format = ($)

instance (a ~ ()) => PrintfType (IO a) where
  format acc fmt = putStrLn $ acc fmt

instance (Show a, PrintfType r) => PrintfType (a -> r) where
  format acc fmt a =
    let (txt, '%' : fmt') = span (/= '%') fmt
        acc' = acc . (txt ++) . (show a ++)
    in format acc' fmt'

printf :: (PrintfType r) => Format -> r
printf = format id

main :: IO ()
main = do
  printf "hello"
  putStrLn $ printf "hello %!" "world"
