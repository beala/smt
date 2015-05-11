module SMT.Parser2
( parseFormula
, ParseError
) where

import Text.Parsec
import Text.Parsec.Expr
import SMT.Types
import Control.Monad.Identity

parseFormula :: String -> Either ParseError Formula
parseFormula = parse expr ""

expr :: Parsec String () Formula
expr = buildExpressionParser table term
    <?> "expression"

parens :: Parsec String () a -> Parsec String () a
parens p = string "(" *> spaces *> p <* spaces <* string ")"

term = parens expr
    <|> string "T" *> spaces *> (pure (B True))
    <|> string "F" *> spaces *> (pure (B False))
    <|> (Var) <$> many1 letter <* spaces
    <?> "simple expression"

table = [ [prefix "~" (Not)]
        , [binary "|" (Or) AssocLeft, binary "&" (And) AssocLeft]
        ]

binary :: String -> (a -> a -> a) -> Assoc -> Operator String () Identity a
binary  name fun assoc = Infix (string name *> (pure fun) <* spaces) assoc

prefix :: String -> (a -> a) -> Operator String () Identity a
prefix  name fun       = Prefix (string name *> pure fun <* spaces)

postfix :: String -> (a -> a) -> Operator String () Identity a
postfix name fun       = Postfix (string name *> pure fun <* spaces)