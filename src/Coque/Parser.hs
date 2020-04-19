module Coque.Parser where

import Control.Applicative
import Text.Trifecta
import Data.Text (Text)
import Coque.Types


data Expr
  = Word Str
  | Opr  Str

exprToObject = \case
  Word x  -> OStr x
  Opr  x  -> OStr x


expr :: Parser Expr
expr = word <|> opr

word :: Parser Expr
word = do
  w <- some lower
  whiteSpace
  pure $ Word w

opr :: Parser Expr
opr = do
  -- w <- some $ oneOf ".,:;-=*+<>!@#$&\\/"
  w <- oneOf ".,:;-=*+<>!@#$&\\/"
  whiteSpace
  pure $ Opr [w]

parseCode :: Parser [Object]
parseCode = do
  whiteSpace
  es <- fmap exprToObject <$> many expr
  eof
  pure es

