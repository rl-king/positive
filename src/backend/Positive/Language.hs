{-# LANGUAGE LambdaCase #-}

module Positive.Language where

import Control.Applicative hiding (many)
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
  deriving (Show)

data Op
  = Plu
  | Min
  | Mul
  | Exp
  deriving (Show)

-- EVAL

eval :: Double -> Double -> Expr -> Double
eval p v = \case
  Pixel -> p
  Var -> v
  Num n -> n
  BinOp a op b -> fromOp op (eval p v a) (eval p v b)

fromOp :: Op -> Double -> Double -> Double
fromOp = \case
  Plu -> (+)
  Min -> (-)
  Mul -> (*)
  Exp -> (**)

-- RUN

type Parser a =
  Parsec Void Text a

parse :: Text -> Either String Expr
parse =
  first errorBundlePretty
    . runParser (ws *> expr) ""

expr :: Parser Expr
expr =
  try binOp <|> var <|> pixel <|> num

-- PARSE

pixel :: Parser Expr
pixel =
  lexe $ Pixel <$ char 'p'

var :: Parser Expr
var =
  lexe $ Var <$ char 'n'

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
      <|> Mul <$ char '*'
      <|> Exp <$ char '*' <* char '*'

num :: Parser Expr
num =
  Num <$> Lexer.signed ws (Lexer.decimal <|> Lexer.float)
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
