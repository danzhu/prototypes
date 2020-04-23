module Main
  ( main
  ) where

import Control.Monad.Trans.Cont (Cont, runCont, cont)

readLine :: Cont (IO a) String
readLine = cont (getLine >>=)

writeLine :: String -> Cont (IO a) ()
writeLine s = cont (putStrLn s >>=)

run :: Cont (IO a) a -> IO a
run c = runCont c pure

main :: IO ()
main = run $ do
  line <- readLine
  writeLine line
