data Rope = Str String
          | Cat Rope Rope
          deriving (Show, Eq)

instance Semigroup Rope where
  (<>) = Cat

instance Monoid Rope where
  mempty = Str ""

format :: Rope -> String -> String
format (Str s)   = (s ++)
format (Cat a b) = format a . format b

main :: IO ()
main = do
  let r = Str "abc" <> Str "def" <> Str "ghi"
  print r
  print $ format r ""
