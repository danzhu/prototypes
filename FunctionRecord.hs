{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Main
  ( main
  ) where

import           Control.Applicative (empty, liftA2, (<|>))
import           Control.Monad (void)
import           Control.Monad.Combinators (between, choice, manyTill, sepEndBy,
                                            skipMany, skipSome)
import           Control.Monad.Combinators.Expr (Operator (InfixL),
                                                 makeExprParser)
import           Control.Monad.Except (throwError)
import           Control.Monad.Reader (ReaderT, ask, asks, local, runReaderT)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import           Data.Char (isAlphaNum)
import           Data.Foldable (asum)
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

type Abs = Block Patt Expr

data Expr
  = EVar Var
  | ESym Sym
  | EStr Text
  | EApp Expr Expr
  | EAbs Abs
  deriving (Show)

data Patt
  = PVar Var
  | PSym Sym
  | PStr Text
  | PAbs (Block Expr Patt)
  deriving (Show)

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

pAbs :: Parser Abs
pAbs = pBlock pPatt pExpr

pAtom :: Parser Expr
pAtom = choice
  [ EVar <$> pVar
  , ESym <$> pSym
  , EStr <$> pStr
  , EAbs <$> braces pAbs
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
  | VFun Fun

data Fail
  = NoVar Var
  | NotFun Val
  | NoMatch Val

type Ctx = [(Var, Val)]

type Eval = ReaderT Ctx (Either Fail)

type Fun = Val -> MaybeT (Either Fail) Val

instance Show Val where
  show = \case
    VSym s -> show s
    VStr s -> show s
    VFun _ -> "<func>"

instance Show Fail where
  show = \case
    NoVar var -> "no such variable: " ++ show var
    NoMatch val -> "no matching clause for: " ++ show val
    NotFun val -> "applying non-function: " ++ show val

runEval :: Eval a -> Ctx -> Either Fail a
runEval = runReaderT

abstract :: Abs -> Eval Fun
abstract arms = asks $ \ctx arg -> do
  let assign (patt, expr) = do
        ctx' <- match patt arg
        lift $ local (ctx' <>) $ interpret expr
      run = asum $ map assign arms
  MaybeT $ runEval (runMaybeT run) ctx

interpret :: Expr -> Eval Val
interpret = \case
  ESym sym -> pure $ VSym sym
  EStr str -> pure $ VStr str
  EVar var -> asks (lookup var) >>= \case
    Just v  -> pure v
    Nothing -> throwError $ NoVar var
  EApp fun arg -> interpret fun >>= \case
    VFun f -> do
      a <- interpret arg
      lift (runMaybeT $ f a) >>= \case
        Just v -> pure v
        Nothing -> throwError $ NoMatch a
    v      -> throwError $ NotFun v
  EAbs blk -> VFun <$> abstract blk

match :: Patt -> Val -> MaybeT Eval Ctx
match (PVar var) val = pure [(var, val)]
match (PSym p) (VSym v)
  | p == v = pure []
match (PStr p) (VStr v)
  | p == v = pure []
match (PAbs arms) (VFun fun) = concat <$> traverse assign arms where
  assign :: (Expr, Patt) -> MaybeT Eval Ctx
  assign (expr, patt) = do
    arg <- lift $ interpret expr
    res <- MaybeT $ lift $ runMaybeT $ fun arg
    match patt res
match _ _ = empty

context :: Ctx
context = []

root :: Val
root = fn sym $ \case
  "str" -> pure . fn sym $ \case
    "append" -> pure . fn str $ \s -> pure . fn str $ \t -> pure . VStr $ s <> t
    _ -> empty
  _ -> empty
  where
    fn :: (Val -> Maybe a) -> (a -> MaybeT (Either Fail) Val) -> Val
    fn c f = VFun $ maybe empty f . c
    str (VStr s) = Just s
    str _        = Nothing
    sym (VSym s) = Just s
    sym _        = Nothing

main :: IO ()
main = do
  (path, src) <- getArgs >>= \case
    []     -> ("<stdin>",) <$> T.getContents
    [path] -> (path,) <$> T.readFile path
    _      -> die "expect 0 or 1 args"
  ast <- case parse pAbs path src of
    Left err -> do
      hPutStr stderr $ errorBundlePretty err
      exitFailure
    Right ast -> pure ast
  print ast
  let run = do
        f <- runEval (abstract ast) context
        runMaybeT $ f root
  case run of
    Right res -> print res
    Left err  -> print err
