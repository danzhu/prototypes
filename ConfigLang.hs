{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Applicative (empty, liftA2, optional, (<|>))
import           Control.Monad (unless, void, when)
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
                                  takeWhile1P, takeWhileP)
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


data DispPos
  = Word Text
  | Line

-- R: indent, W: output, S: position
type Disp = RWS Int Text DispPos

dispLine :: Disp ()
dispLine = do
  get >>= \case
    Word _ -> tell "\n"
    Line -> pure ()
  put Line

dispText :: Bool -> Text -> Text -> Disp ()
dispText lead trail s = do
  get >>= \case
    Word sep -> when lead $ tell sep
    Line -> do
      ind <- ask
      tell $ T.replicate ind "  "
  tell s
  put $ Word trail

dispWord, dispPre, dispPost :: Text -> Disp ()
dispWord = dispText True " "
dispPre  = dispText True ""
dispPost = dispText False " "

dispBetween :: Text -> Text -> Disp a -> Disp a
dispBetween o e d = dispPre o *> d <* dispPost e

dispBlock :: Disp a -> Disp a
dispBlock = local (+ 1)

display :: Disp a -> Text
display d = snd $ evalRWS d 0 Line


quoted :: Text -> Text
quoted s = if T.all bare s && not (T.null s)
  then s
  else T.pack $ show s

class Display a where
  disp :: a -> Disp ()

instance Display Conf where
  disp (CStr s) = disp s
  disp (CRec r) = dispBetween "(" ")" $ disp r

instance Display Text where
  disp = dispWord . quoted

instance Display Record where
  disp (Record args fields) = do
    for_ args disp
    dispBlock $ disp fields

instance Display Block where
  disp (Block fields) = unless (null fields) $ do
    dispLine
    for_ fields $ \(n, f) -> do
      disp n
      disp f
      dispLine


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
line = takeWhileP (Just "character") (/= '\n') <* newline

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
  ident = takeWhile1P (Just "bare word") bare
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
    pure $ leader : follow
  sc
  pure . T.unlines $ empties ++ rest
  where
    emptyLn = T.empty <$ newline

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
  T.putStrLn . display $ disp ast
