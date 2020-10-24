{-# LANGUAGE LambdaCase #-}

module Positive.Language where

import Control.Applicative hiding (many)
import qualified Data.Text as Text
import Positive.Prelude
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Text.Megaparsec.Debug

-- DEFINTIONS

data Expr
  = Pixel
  | Var
  | Num Double
  | BinOp Expr Op Expr
  | Fun Fun Expr
  deriving (Show)

data Op
  = Plu
  | Min
  | Mul
  | Exp
  deriving (Show)

data Fun
  = Sin
  | Cos
  | Neg
  | Sqrt
  deriving (Show)

-- EVAL

eval :: Double -> Double -> Expr -> Double
eval p v = \case
  Pixel -> p
  Var -> v
  Num n -> n
  BinOp a op b -> fromOp op (eval p v a) (eval p v b)
  Fun f a -> fromFun f (eval p v a)

fromOp :: Op -> Double -> Double -> Double
fromOp = \case
  Plu -> (+)
  Min -> (-)
  Mul -> (*)
  Exp -> (**)

fromFun :: Fun -> Double -> Double
fromFun = \case
  Sin -> sin
  Cos -> cos
  Neg -> negate
  Sqrt -> sqrt

-- RUN

type Parser a =
  Parsec Void Text a

parse :: Text -> Either Text Expr
parse =
  first (Text.pack . errorBundlePretty)
    . runParser (ws *> expr) ""

expr :: Parser Expr
expr =
  try binOp <|> try fun <|> var <|> pixel <|> num

-- PARSE

pixel :: Parser Expr
pixel =
  Pixel <$ lexe (char 'p')

var :: Parser Expr
var =
  Var <$ lexe (char 'n')

binOp :: Parser Expr
binOp = do
  a <- pixel <|> var <|> num <|> parens expr
  op <- operator
  b <- pixel <|> var <|> num <|> parens expr
  lexe . pure $ BinOp a op b

operator :: Parser Op
operator =
  lexe $
    Plu <$ char '+'
      <|> Min <$ char '-'
      <|> Exp <$ try (char '*' <* char '*')
      <|> Mul <$ char '*'

fun :: Parser Expr
fun = do
  name <- funName
  a <- pixel <|> var <|> num <|> parens expr
  lexe . pure $ Fun name a

funName :: Parser Fun
funName =
  lexe $
    Sin <$ chunk "sin"
      <|> Cos <$ chunk "cos"
      <|> Neg <$ chunk "neg"
      <|> Sqrt <$ chunk "sqrt"

num :: Parser Expr
num =
  Num <$> lexe (Lexer.signed ws (Lexer.decimal <|> Lexer.float))
    <?> "float"

parens :: Parser a -> Parser a
parens =
  lexe . between (lexe $ char '(') (char ')')

lexe :: Parser a -> Parser a
lexe =
  (<* ws)

ws :: Parser ()
ws =
  space <|> void eol <|> void newline
