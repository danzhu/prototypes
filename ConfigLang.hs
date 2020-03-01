{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Main
  ( main
  ) where

import           Control.Applicative (empty, liftA2, (<|>))
import           Control.Monad (unless, void, when)
import           Control.Monad.Combinators (choice, many, manyTill, option,
                                            skipMany, skipSome)
import           Control.Monad.Reader (ReaderT, ask, local, runReaderT)
import           Control.Monad.RWS.Lazy (RWS, evalRWS)
import           Control.Monad.State.Lazy (get, put)
import           Control.Monad.Writer.Lazy (tell)
import           Data.Char (isAlphaNum)
import           Data.Foldable (for_)
import           Data.List.NonEmpty (NonEmpty ((:|)))
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

data Record = Record [Text] Block
  deriving (Show)

data Block
  = Fields (NonEmpty (Text, Record))
  | Str Text
  | Rec Record
  | Empty
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
dispLine = tell "\n" *> put Line

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

instance Display Text where
  disp = dispWord . quoted

instance Display Record where
  disp (Record args blk) = do
    for_ args disp
    disp blk

instance Display Block where
  disp (Fields fs) = do
    dispWord "\\"
    dispLine
    dispBlock . for_ fs $ \(n, f) -> disp n *> disp f
  disp (Rec r) = do
    dispWord "|"
    disp r
  disp (Str s)
    | Just (c, _) <- T.uncons s, c /= ' ' = do
        dispWord ">"
        dispLine
        dispBlock . for_ (T.lines s) $ \l -> do
          unless (T.null l) $ dispWord l
          dispLine
    | otherwise = do
        dispWord $ "< EOF\n" <> s <> "EOF"
        dispLine
  disp Empty = dispLine


type Parser = ReaderT Pos (Parsec Void Text)
type ParserError = ParseErrorBundle Text Void

sc :: Parser ()
sc = L.space (void space1) lineComment empty where
  lineComment = L.skipLineComment "#"

space1 :: Parser Text
space1 = takeWhile1P Nothing (== ' ')

space :: Parser Text
space = option T.empty space1

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

endln :: Parser ()
endln = skipMany $ symbol "\n"

endln1 :: Parser ()
endln1 = skipSome $ symbol "\n"

indentGuard :: Ordering -> Pos -> Parser Pos
indentGuard = L.indentGuard $ pure ()

indented :: Parser a -> Parser a
indented p = do
  ref <- ask
  act <- L.indentLevel
  if act < ref
    then L.incorrectIndent GT ref act
    else local (const act) p

indentSome :: Parser a -> Parser (NonEmpty a)
indentSome p = do
  endln
  indented $ do
    ind <- ask
    leader <- p
    follow <- many $ indentGuard EQ ind *> p
    pure $ leader :| follow

line :: Parser Text
line = takeWhileP (Just "character") (/= '\n') <* newline

indentStr :: Parser Text
indentStr = do
  _ <- newline
  empties <- many emptyLn
  prefix <- space
  indented $ do
    leader <- line
    follow <- many $ emptyLn <|> (chunk prefix *> line)
    pure . T.unlines $ empties ++ (leader : follow)
  where emptyLn = T.empty <$ newline

heredocStr :: Parser Text
heredocStr = do
  end <- line
  T.unlines <$> line `manyTill` chunk (end <> "\n")

word :: Parser Text
word = lexeme $ ident <|> quote where
  ident = takeWhile1P (Just "bare word") bare
  quote = T.pack <$> (sing <|> doub)
  sing = single '\'' *> (anySingle `manyTill` single '\'')
  doub = single '"' *> (L.charLiteral `manyTill` single '"')

field :: Parser (Text, Record)
field = liftA2 (,) word record

block :: Parser Block
block = choice
  [ Fields <$> (symbol "\\" *> indentSome field)
  , Rec <$> (symbol "|" *> record)
  , Str <$> (str <* sc <* endln)
  , Empty <$ endln1
  ] where
  str = (symbol ">" *> indentStr) <|> (symbol "<" *> heredocStr)

record :: Parser Record
record = Record <$> many word <*> block

parse :: Parser a -> String -> Text -> Either ParserError a
parse p = runParser $ runReaderT (sc *> p <* eof) pos1

main :: IO ()
main = do
  (path, src) <- getArgs >>= \case
    []     -> ("<stdin>",) <$> T.getContents
    [path] -> (path,) <$> T.readFile path
    _      -> die "expect 0 or 1 args"
  ast <- case parse record path src of
    Left err -> do
      hPutStr stderr $ errorBundlePretty err
      exitFailure
    Right ast -> pure ast
  T.putStr . display $ disp ast
