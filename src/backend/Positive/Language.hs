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
  | Fun Text [Expr]
  deriving (Show)

data Op
  = Plu
  | Min
  | Mul
  | Div
  | Exp
  deriving (Show)

-- CHECK

check :: Expr -> Either Text Expr
check e =
  case e of
    Fun f as -> e <$ (checkFun f =<< traverse check as)
    BinOp a _ b -> e <$ traverse check [a, b]
    _ -> Right e

checkFun :: Text -> [Expr] -> Either Text ()
checkFun f as =
  let countArgs name xs n
        | length xs == n = Right ()
        | otherwise =
          Left $
            "The function "
              <> tshow name
              <> " expects "
              <> tshow n
              <> " argument(s), but was given "
              <> tshow (length xs)
   in case f of
        "id" -> countArgs f as 1
        "sin" -> countArgs f as 1
        "cos" -> countArgs f as 1
        "neg" -> countArgs f as 1
        "abs" -> countArgs f as 1
        "sqrt" -> countArgs f as 1
        "min" -> countArgs f as 2
        "max" -> countArgs f as 2
        name -> Left $ "Unknown function: " <> tshow name -- adds quotes

-- EVAL

eval :: Double -> Double -> Expr -> Double
eval p v = \case
  Pixel -> p
  Var -> v
  Num n -> n
  BinOp a op b -> evalOp op (eval p v a) (eval p v b)
  Fun f as -> evalFun f (fmap (eval p v) as)

evalOp :: Op -> Double -> Double -> Double
evalOp = \case
  Plu -> (+)
  Min -> (-)
  Mul -> (*)
  Div -> (/)
  Exp -> (**)

evalFun :: Text -> [Double] -> Double
evalFun f as =
  case (f, as) of
    ("id", [x]) -> x
    ("sin", [x]) -> sin x
    ("cos", [x]) -> cos x
    ("neg", [x]) -> negate x
    ("sqrt", [x]) -> sqrt x
    ("abs", [x]) -> abs x
    ("min", [x, y]) -> min x y
    ("max", [x, y]) -> max x y
    (name, args) -> error $ Text.unpack name <> " " <> show args

-- RUN

type Parser a =
  Parsec Void Text a

parse :: Text -> Either Text Expr
parse =
  first (Text.pack . errorBundlePretty)
    . runParser (ws *> expr <* eof) ""

expr :: Parser Expr
expr =
  try binOp <|> try fun

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
      <|> Div <$ char '/'

fun :: Parser Expr
fun = do
  name <- lexe $ takeWhileP (Just "a function name") (/= ' ')
  a <- sepBy1 (pixel <|> var <|> num <|> parens expr) (lookAhead ws)
  lexe . pure $ Fun name a

num :: Parser Expr
num =
  Num <$> lexe (Lexer.signed ws (try Lexer.float <|> Lexer.decimal))
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
