module SMT.Parser
( parseFormula
, ParseError
) where

import Text.Parsec
import Control.Applicative ((<**>))
import SMT.Types

-- Expr = Lit PostExpr | Var PostExpr | Op Expr | Var | Lit
-- PostExpr = BinOp Lit PostExpr* | BinOp Var PostExpr*
-- BinOp = '&' | '|'
-- Op = '~'
-- Var = alpha+
-- Lit = 'T' | 'F'

parseFormula :: String -> Either ParseError Formula
parseFormula s = parse expr "" s

-- Simple string parser.
type SParser a = Parsec String () a

compose :: [a -> a] -> a -> a
compose fs = foldl (flip (.)) id fs

(<.>) :: Applicative f => f (a -> b) -> f (b -> c) -> f(a -> c)
(<.>) l r = (flip (.)) <$> l <*> r

-- Consume trailing whitespace and discard.
ws :: SParser a -> SParser a
ws parser = parser <* spaces

expr :: SParser Formula
expr = spaces *> (litPostExpr <|> varPostExpr <|> opExpr <|> var <|> lit) <* spaces <* eof

postExpr :: SParser (Formula -> Formula)
postExpr =  (try binOpLit <|> try binOpVar) <.> (compose <$> many postExpr)

binOpLit :: SParser (Formula -> Formula)
binOpLit = do
  op <- binOp
  l <- lit
  return (\l' -> l' `op` l)

binOpVar :: SParser (Formula -> Formula)
binOpVar = do
  op <- binOp 
  v <- var
  return (\l -> l `op` v)
  
litPostExpr = lit <**> postExpr

varPostExpr = var <**> postExpr

opExpr = op <*> expr

binOp :: SParser (Formula -> Formula -> Formula)
binOp =  ws $  char '&' *> (pure (And))
           <|> char '|' *> (pure (Or))

op :: SParser (Formula -> Formula)
op = ws $ char '~' *> (pure (Not))

var = ws $ Var <$> many1 letter

lit =  ws $ char 'T' *> (pure (B True))
         <|> char 'F' *> (pure (B False))
