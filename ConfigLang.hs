{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Applicative (empty, liftA2, optional, (<|>))
import           Control.Monad (void, when)
import           Control.Monad.Combinators (between, many, manyTill, option,
                                            skipMany)
import           Control.Monad.Reader (ReaderT, ask, asks, local, runReaderT)
import           Control.Monad.RWS.Lazy (RWS, evalRWS)
import           Control.Monad.State.Lazy (get, put)
import           Control.Monad.Writer.Lazy (tell)
import           Data.Char (isAlphaNum)
import           Data.Foldable (for_, toList)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Void (Void)
import           System.Environment (getArgs)
import           System.Exit (die, exitFailure)
import           System.IO (hPutStr, stderr)
import           Text.Megaparsec (ParseErrorBundle, Parsec, Pos, anySingle,
                                  chunk, eof, pos1, runParser, single,
                                  takeWhile1P)
import           Text.Megaparsec.Char (newline)
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Error (errorBundlePretty)

data Conf
  = CStr Text
  | CRec Record
  deriving (Show)

data Record = Record [Conf] Block
  deriving (Show)

newtype Block = Block [(Text, Record)]
  deriving (Show)

bare :: Char -> Bool
bare c = isAlphaNum c || Set.member c special where
  special = Set.fromList "$%*+,-./:=@_~"


-- R: indent, W: output, S: current line nonempty
type Disp = RWS Int Text Bool

class Display a where
  disp :: a -> Disp ()

dispLine :: Disp ()
dispLine = do
  nonempty <- get
  when nonempty $ tell "\n"
  put False

dispWord :: Text -> Disp ()
dispWord s = do
  nonempty <- get
  if nonempty
    then tell " "
    else tell . (`T.replicate` "  ") =<< ask
  tell s
  put True

indented :: Disp a -> Disp a
indented d = dispLine *> local (+ 1) d

display :: Display a => a -> Text
display a = snd $ evalRWS (disp a) 0 False


splitStrs :: [Conf] -> ([Text], [Conf])
splitStrs (CStr s : as) = do
  let (ss, rs) = splitStrs as
  (s : ss, rs)
splitStrs rs = ([], rs)

quoted :: Text -> Text
quoted s = if T.all bare s && not (T.null s)
  then s
  else T.pack $ show s

dispField :: Display a => Text -> a -> Disp ()
dispField n f = disp n *> disp f *> dispLine

instance Display Conf where
  disp (CStr s) = disp s
  disp (CRec r) = disp r

instance Display Text where
  disp = dispWord . quoted

instance Display Record where
  disp (Record args fields) = do
    let (strs, rems) = splitStrs args
    for_ strs disp
    indented $ do
      for_ rems $ dispField "-"
      disp fields

instance Display Block where
  disp (Block fields) = for_ fields $ uncurry dispField


type Parser = ReaderT (Maybe Pos) (Parsec Void Text)
type ParserError = ParseErrorBundle Text Void

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

sc :: Parser ()
sc = L.space (void space1) lineComment empty

space1 :: Parser Text
space1 = takeWhile1P Nothing (== ' ')

space :: Parser Text
space = option T.empty space1

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

line :: Parser Text
line = T.pack <$> (anySingle `manyTill` newline)

endline :: Parser Text
endline = symbol "\n"

indentBlock :: Parser a -> Parser a
indentBlock p = do
  (rel, ref) <- asks $ maybe (EQ, pos1) (GT,)
  act <- L.indentGuard (pure ()) rel ref
  local (const $ Just act) p

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

word :: Parser Text
word = lexeme $ ident <|> quote where
  ident = takeWhile1P (Just "bare character") bare
  quote = T.pack <$> (sing <|> doub)
  sing = single '\'' *> (anySingle `manyTill` single '\'')
  doub = single '"' *> (L.charLiteral `manyTill` single '"')

indentStr :: Parser Text
indentStr = do
  _ <- newline
  empties <- many emptyLn
  prefix <- space
  rest <- option [] . indentBlock $ do
    leader <- line
    follow <- many $ emptyLn <|> (chunk prefix *> line)
    _ <- space
    pure $ leader : follow
  pure . T.unlines $ empties ++ rest
  where
    emptyLn = "" <$ newline

heredocStr :: Parser Text
heredocStr = do
  end <- line
  lns <- line `manyTill` chunk (end <> "\n")
  _ <- space
  pure $ T.unlines lns

subject :: Parser Conf
subject = CRec <$> record <|> CStr <$> (indent <|> heredoc) where
  record = symbol "|" *> item
  indent = symbol ">" *> indentStr
  heredoc = symbol "<" *> heredocStr

item :: Parser Record
item = do
  args <- many $ CStr <$> word <|> CRec <$> parens item
  subj <- toList <$> optional subject
  Record (args ++ subj) <$> block

block :: Parser Block
block = skipMany endline *> (Block <$> many field) where
  field = indentBlock $ liftA2 (,) word item

parse :: Parser a -> String -> Text -> Either ParserError a
parse p = runParser $ runReaderT (sc *> p <* eof) Nothing

main :: IO ()
main = do
  (path, src) <- getArgs >>= \case
    []     -> ("<stdin>",) <$> T.getContents
    [path] -> (path,) <$> T.readFile path
    _      -> die "expect 0 or 1 args"
  ast <- case parse block path src of
    Left err -> do
      hPutStr stderr $ errorBundlePretty err
      exitFailure
    Right ast -> pure ast
  T.putStr $ display ast
