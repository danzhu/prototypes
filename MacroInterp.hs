{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}

import           Control.Applicative (empty, liftA2, (<|>))
import           Control.Monad (void)
import           Control.Monad.Combinators (between, choice, many, sepEndBy,
                                            skipMany, skipSome, some)
import           Control.Monad.Combinators.Expr (Operator (InfixL),
                                                 makeExprParser)
import           Data.Functor (($>))
import           Data.List (intercalate)
import           Data.Void (Void)
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.IO (hPutStr, stderr)
import           Text.Megaparsec (ParseErrorBundle, Parsec, eof, noneOf,
                                  runParser, takeWhile1P)
import           Text.Megaparsec.Char (alphaNumChar, char, lowerChar)
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Error (errorBundlePretty)


newtype Var = Var String
  deriving (Eq, Ord)

instance Show Var where
  show (Var var) = var

data StrSeg
  = SLiteral String
  | SInterpo Ast

instance Show StrSeg where
  show = \case
    SLiteral s -> s
    SInterpo a -> "${" ++ show a ++ "}"

data Ast
  = AStr [StrSeg]
  | AVar Var
  | AApp Ast Ast
  | ABlk [Ast]

instance Show Ast where
  show = \case
    AStr s   -> show $ concatMap show s
    AVar v   -> show v
    AApp f a -> show f ++ " " ++ show a
    ABlk as  -> "(" ++ intercalate ", " (map show as) ++ ")"

type Parser = Parsec Void String
type ParserError = ParseErrorBundle String Void

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

sc :: Parser ()
sc = L.space spc lineComment empty where
  spc = void $ takeWhile1P Nothing (== ' ')

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

variable :: Parser Ast
variable = AVar . Var <$> liftA2 (:) idStart (many idCont) where
  idStart = lowerChar
  idCont = alphaNumChar <|> char '_'

string :: Parser Ast
string = AStr <$> between (char '"') (char '"') (many $ pLit <|> pInt) where
  pLit = SLiteral <$> some (noneOf "\\\"$" <|> pEsc)
  pInt = SInterpo <$> (char '$' *> atomInline)
  pEsc = char '\\' *> choice
    [ char 'n' $> '\n'
    , char '\\' $> '\\'
    , char '"' $> '"'
    , char '$' $> '$'
    ]

atomInline :: Parser Ast
atomInline = variable <|> string <|> parens block

atom :: Parser Ast
atom = lexeme atomInline

expression :: Parser Ast
expression = makeExprParser atom table where
  table = [ [ InfixL $ pure AApp ] ]

block :: Parser Ast
block = ABlk <$> (skipMany sep *> expression `sepEndBy` skipSome sep) where
  sep = symbol "\n" <|> symbol ","

parse :: String -> String -> Either ParserError Ast
parse = runParser $ sc *> block <* eof


data Val
  = VStr String
  | VFun (Ctx -> Ast -> Val)

instance Show Val where
  show = \case
    VStr s -> show s
    VFun _ -> "<func>"

type Ctx = [(Var, Val)]

interpret :: Ctx -> Ast -> Val
interpret ctx = \case
  AStr str -> VStr $ str >>= \case
    SLiteral s -> s
    SInterpo a -> case interpret ctx a of
      VStr s -> s
      v      -> error $ "interpret: string interpolation not string: " ++ show v
  AVar var -> case lookup var ctx of
    Just v  -> v
    Nothing -> error $ "interpret: no such variable: " ++ show var
  AApp fun arg -> case interpret ctx fun of
    VFun f -> f ctx arg
    v      -> error $ "interpret: applying non-function: " ++ show v
  ABlk lns -> interpret ctx $ last lns

function :: (Val -> Val) -> Ctx -> Ast -> Val
function f = (f .) . interpret

lambda :: Ctx -> Ast -> Val
lambda ctx = \case
  AVar param ->
    VFun $ \_ body ->
    VFun . function $ \arg ->
    interpret ((param, arg) : ctx) body
  arg -> error $ "lambda: incorrect argument: " ++ show arg

main :: IO ()
main = do
  args <- getArgs
  (path, src) <- case args of
    [] -> do
      src <- getContents
      pure ("<stdin>", src)
    path : _ -> do
      src <- readFile path
      pure (path, src)
  let ctx = [ (Var "show", VFun . const $ VStr . show)
            , (Var "id", VFun interpret)
            , (Var "fn", VFun lambda)
            ]
  case parse path src of
    Left err -> do
      hPutStr stderr $ errorBundlePretty err
      exitFailure
    Right ast -> do
      print ast
      print $ interpret ctx ast
