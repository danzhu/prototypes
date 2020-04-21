{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

import           Control.Applicative (empty, liftA2, (<|>))
import           Control.Monad (void)
import           Control.Monad.Combinators (between, choice, manyTill, sepEndBy,
                                            skipMany, skipSome)
import           Control.Monad.Combinators.Expr (Operator (InfixL),
                                                 makeExprParser)
import           Data.Char (isAlphaNum)
import           Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import           Data.String (IsString, fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Void (Void)
import           System.Environment (getArgs)
import           System.Exit (die, exitFailure)
import           System.IO (hPutStr, stderr)
import           Text.Megaparsec (ParseErrorBundle, Parsec, eof, runParser,
                                  single, takeWhile1P)
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Error (errorBundlePretty)


newtype Var = Var Text
  deriving (Eq, Ord)

newtype Sym = Sym Text
  deriving (Eq, Ord)

type Block a b = [(a, b)]

data Expr
  = EVar Var
  | ESym Sym
  | EStr Text
  | EApp Expr Expr
  | EAbs (Block Patt Expr)
  deriving (Show)

data Patt
  = PVar Var
  | PSym Sym
  | PStr Text
  | PAbs (Block Expr Patt)
  deriving (Show)

type Func = Block Patt Expr

instance Show Var where
  show (Var var) = T.unpack var

instance Show Sym where
  show (Sym sym) = '.' : T.unpack sym

instance IsString Sym where
  fromString = Sym . T.pack


type Parser = Parsec Void Text
type ParserError = ParseErrorBundle Text Void

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

sc :: Parser ()
sc = L.space spc lineComment empty where
  spc = void $ takeWhile1P Nothing (== ' ')

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

pId :: Parser Text
pId = lexeme $ takeWhile1P (Just "pId") bare where
  bare c = isAlphaNum c || Set.member c special
  special = Set.fromList "_"

pStr :: Parser Text
pStr = lexeme $ T.pack <$> doub where
  doub = single '"' *> (L.charLiteral `manyTill` single '"')

pVar :: Parser Var
pVar = Var <$> pId

pSym :: Parser Sym
pSym = Sym <$> (single '.' *> pId)

pFunc :: Parser Func
pFunc = pBlock pPatt pExpr

pAtom :: Parser Expr
pAtom = choice
  [ EVar <$> pVar
  , ESym <$> pSym
  , EStr <$> pStr
  , EAbs <$> braces pFunc
  ]

pExpr :: Parser Expr
pExpr = makeExprParser pAtom table where
  table = [ [ InfixL $ pure EApp ] ]

pPatt :: Parser Patt
pPatt = choice
  [ PVar <$> pVar
  , PSym <$> pSym
  , PStr <$> pStr
  , PAbs <$> braces (pBlock pAtom pPatt)
  ]

pBlock :: Parser a -> Parser b -> Parser (Block a b)
pBlock a b = skipMany sep *> entry `sepEndBy` skipSome sep where
  entry = liftA2 (,) a $ symbol "->" *> b
  sep = symbol "\n" <|> symbol ","

parse :: Parser a -> String -> Text -> Either ParserError a
parse p = runParser $ sc *> p <* eof


data Val
  = VSym Sym
  | VStr Text
  | VFun (Val -> Val)

instance Show Val where
  show = \case
    VSym s -> show s
    VStr s -> show s
    VFun _ -> "<func>"

type Ctx = [(Var, Val)]

abstract :: Ctx -> Func -> Val
abstract ctx arms = VFun $ \arg -> do
  let matchArm (p, e) = (, e) <$> match ctx p arg
  case mapMaybe matchArm arms of
    (ctx', expr) : _ -> interpret (ctx' <> ctx) expr
    [] -> error $ "interpret: no matching clause for " ++ show arg ++ " in function: " ++ show arms

interpret :: Ctx -> Expr -> Val
interpret ctx = \case
  ESym sym -> VSym sym
  EStr str -> VStr str
  EVar var -> case lookup var ctx of
    Just v  -> v
    Nothing -> error $ "interpret: no such variable: " ++ show var
  EApp fun arg -> case interpret ctx fun of
    VFun f -> f $ interpret ctx arg
    v      -> error $ "interpret: applying non-function: " ++ show v
  EAbs blk -> abstract ctx blk

match :: Ctx -> Patt -> Val -> Maybe Ctx
match _ (PVar var) val = Just [(var, val)]
match _ (PSym p) (VSym v)
  | p == v = Just []
match _ (PStr p) (VStr v)
  | p == v = Just []
match ctx (PAbs p) (VFun v) = concat <$> traverse assign p where
  assign (expr, patt) = match ctx patt $ v $ interpret ctx expr
match _ _ _ = Nothing

context :: Ctx
context = []

root :: Val
root = VFun $ \case
  VSym "str" -> VFun $ \case
        VSym "append" -> VFun $ \(VStr s) -> VFun $ \(VStr t) -> VStr $ s <> t

main :: IO ()
main = do
  (path, src) <- getArgs >>= \case
    []     -> ("<stdin>",) <$> T.getContents
    [path] -> (path,) <$> T.readFile path
    _      -> die "expect 0 or 1 args"
  ast <- case parse pFunc path src of
    Left err -> do
      hPutStr stderr $ errorBundlePretty err
      exitFailure
    Right ast -> pure ast
  print ast
  let VFun f = interpret context $ EAbs ast
  print $ f root
