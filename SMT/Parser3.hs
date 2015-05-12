module SMT.Parser3
( parseFormula
, ParseError
) where

import Text.Parsec
import Control.Applicative ((<**>))
import SMT.Types

parseFormula :: String -> Either ParseError Formula
parseFormula s = parse expr "" s

-- Simple string parser.
type SParser a = Parsec String () a

expr = spaces *> (term `chainl1` orOp)
term = factor `chainl1` andOp
factor = maybeNot <*> (parens expr <|> lit <|> var) <* spaces

orOp = string "|" *> (pure (Or)) <* spaces

andOp = string "&" *> (pure (And)) <* spaces

parens = between (string "(" <* spaces) (spaces *> string ")")

lit = string "T" *> spaces *> (pure T)
   <|> string "F" *> spaces *> (pure F)

var = (Var) <$> many1 lower

maybeNot = option id (string "~" *> spaces *> (pure (Not)))