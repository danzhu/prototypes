data Async s o r = Emit s o
                 | Done r

newtype Task i o r = Task { exec :: i -> Async (Task i o r) o r }

data Event = Key Char

data Command = InsertChar Char
             | DeleteChar
             | Operation Operator Motion
             deriving Show

data Operator = Copy
              | Change
              | Delete
              deriving Show

data Motion = LineNext
            | LinePrev
            deriving Show

type Handle = Task Event
type Future = Handle ()
type Stream a = Handle a ()

normal :: Stream (Maybe Command)
normal = Task $ \e -> case e of
  Key 'i' -> Emit insert Nothing
  Key 'c' -> Emit (pending Change motion) Nothing
  Key 'q' -> Done ()

pending :: Operator -> Future (Maybe Motion) -> Stream (Maybe Command)
pending op t = Task $ \e -> case exec t e of
  Emit t' () -> Emit (pending op t') Nothing
  Done mot   -> Emit normal $ Operation op <$> mot

insert :: Stream (Maybe Command)
insert = Task $ \e -> case e of
  Key c -> Emit insert $ Just $ InsertChar c

motion :: Future (Maybe Motion)
motion = Task $ \e -> case e of
  Key 'j' -> Done $ Just LineNext
  Key 'k' -> Done $ Just LinePrev
  Key _   -> Done Nothing

runTask :: [i] -> Task i o r -> ([o], Either (Task i o r) r)
runTask [] task = ([], Left task)
runTask (i : is) t = case exec t i of
  Emit task' o -> let (os, r) = runTask is task'
                  in (o : os, r)
  Done r       -> ([], Right r)

runEvents :: (Show o) => Handle o r -> IO r
runEvents task = do
  line <- getLine
  let events = map Key line
      (os, r) = runTask events task
  print os
  case r of
    Left task' -> runEvents task'
    Right res -> pure res

main :: IO ()
main = runEvents normal
