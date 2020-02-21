{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

import           Control.Applicative (empty, liftA2, (<|>))
import           Control.Monad (void)
import           Control.Monad.Combinators (between, many, manyTill)
import           Control.Monad.Except (throwError)
import           Control.Monad.Reader (ReaderT, ask, asks, local, runReaderT)
import qualified Data.Map as Map
import           Data.Traversable (for)
import           Data.Void (Void)
import           System.Exit (die)
import           Text.Megaparsec (ParseErrorBundle, Parsec, anySingle, eof,
                                  runParser, takeWhile1P, optional, chunk)
import           Text.Megaparsec.Char (alphaNumChar, char, lowerChar)
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Error (errorBundlePretty)

type Parser = Parsec Void String
type ParserError = ParseErrorBundle String Void

newtype Var = Var String
  deriving (Show, Eq, Ord)

data Ast a
  = ALit a
  | AVar Var
  | AApp (Ast a) [Ast a]
  deriving (Show)

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
parens = between (symbol "(") (symbol ")")

variable :: Parser Var
variable = lexeme $ Var <$> liftA2 (:) idStart (many idCont) where
  idStart = lowerChar
  idCont = alphaNumChar <|> char '_'

template :: Parser a -> Parser (Ast a)
template lit = expr where
  atom = (AVar <$> variable) <|> (ALit <$> lit) <|> parens expr
  expr = AApp <$> atom <*> many atom

parse :: Parser a -> String -> String -> Either ParserError a
parse p = runParser $ sc *> p <* eof

-- block :: Parser (Ast a)
-- block = ABlk <$> (skipMany sep *> application `sepEndBy` skipSome sep) where
--   sep = symbol "\n" <|> symbol ","

type Interpreter v = ReaderT (Env v) (Either (InterpError v))
type Lazy v = Either (InterpError v) v
type Env v = Map.Map Var v

type Gen a r = Interpreter (Val a r) r
type Interp a r = Interpreter (Val a r) (Val a r)

data InterpError v
  = NameError Var
  | CallError v
  | ShowError v
  deriving (Show)

data Val a r
  = VLit r
  | VFun ([Ast a] -> Interp a r)

instance Show v => Show (Val a v) where
  show = \case
    VLit v -> show v
    VFun _ -> "<func>"

class Render a r where
  render :: a -> Gen a r

generate :: Render a r => Ast a -> Gen a r
generate a = interpret a >>= \case
  VLit v -> pure v
  val    -> throwError $ ShowError val

interpret :: Render a r => Ast a -> Interp a r
interpret = \case
  AVar var -> asks (Map.lookup var) >>= \case
    Just val -> pure val
    Nothing  -> throwError $ NameError var
  AApp fun args -> interpret fun >>= \case
    v@VLit{} -> if null args
      then pure v
      else throwError $ CallError v
    VFun fun' -> fun' args
  ALit lit -> VLit <$> render lit

runInterp :: Interpreter v r -> Env v -> Either (InterpError v) r
runInterp = runReaderT


newtype TemplStr = TS [StrSeg]
  deriving (Show)

data StrSeg
  = SOne Char
  | SInt (Ast TemplStr)
  deriving (Show)

parseTS :: Parser TemplStr
parseTS = TS <$> many seg where
  templ = template $ TS <$> (chunk "[" *> optional (chunk "\n") *> manyTill seg (chunk "]-"))
  seg = (SInt <$> (chunk "{-" *> templ <* chunk "}")) <|> (SOne <$> anySingle)

initEnv :: Env (Val TemplStr String)
initEnv = Map.fromList
  [ (Var "terminal", VLit "termite")
  , (Var "fn", VFun fn)
  ] where
  fn :: Render a r => [Ast a] -> Interp a r
  fn [AApp (AVar param) [], body] = do
    env <- ask
    pure . VFun $ \[arg] -> do
      arg' <- interpret arg
      local (const $ Map.insert param arg' env) $ interpret body

instance Render TemplStr String where
  render (TS segs) = fmap mconcat . for segs $ \case
    SOne chr -> pure [chr]
    SInt ast -> generate ast

main :: IO ()
main = do
  inp <- getContents
  ts <- case parse parseTS "<stdin>" inp of
    Left err -> die $ errorBundlePretty err
    Right ts -> pure ts
  s <- case runInterp (render ts) initEnv of
    Left err -> die $ show err
    Right s  -> pure s
  putStr s
