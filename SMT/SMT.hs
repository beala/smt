module SMT.SMT
( satParse,
  Env
) where

import Control.Applicative
import qualified Data.Map.Strict as Map
import SMT.Types
import SMT.Parser2
import Debug.Trace

decide :: Env -> Formula -> Maybe (Env, Env)
decide _ (B _) = Nothing
decide (env) (And f1 f2) = (decide env f1) <|> (decide env f2)
decide (env) (Or f1 f2) = (decide env f1) <|> (decide env f2)
decide (env) (Not f) = decide env f
decide (env) (Var s) = Just (Map.insert s True env, Map.insert s False env)

substitute :: Env -> Formula -> Formula
substitute _ (B b) = B b
substitute (env) (Var name) = maybe (Var name) (B) (Map.lookup name env)
substitute (env) (Or f1 f2) = Or (substitute env f1) (substitute env f2)
substitute (env) (And f1 f2) = And (substitute env f1) (substitute env f2)
substitute (env) (Not f) = Not (substitute env f)

simplify :: Formula -> Formula
simplify (B b) = B b
simplify (Var name) = Var name
simplify (Or (B True) _) = B True
simplify (Or _ (B True)) = B True
simplify (Or (B False) (B False)) = B False
simplify (Or f1 f2) = Or (simplify f1) (simplify f2)
simplify (And (B False) _) = B False
simplify (And _ (B False)) = B False
simplify (And (B True) (B True)) = B True
simplify (And f1 f2) = And (simplify f1) (simplify f2)
simplify (Not (B True)) = B False
simplify (Not (B False)) = B True
simplify (Not (Or f1 f2)) = simplify (And (simplify (Not f1)) (simplify (Not f2)))
simplify (Not (And f1 f2)) = simplify (Or (simplify (Not f1)) (simplify (Not f2)))
simplify (Not f) = Not (simplify f)

eval :: Formula -> Bool
eval (B b) = b
eval (Or f1 f2) = (eval f1) || (eval f2)
eval (And f1 f2) = (eval f1) && (eval f2)
eval (Not f) = not $ eval f

freeVars :: Formula -> [String]
freeVars (B _) = []
freeVars (Var n) = [n]
freeVars (And f1 f2) = freeVars f1 ++ freeVars f2
freeVars (Or f1 f2) = freeVars f1 ++ freeVars f2
freeVars (Not f) = freeVars f

sat :: Env -> Formula -> Maybe Env
sat env f = case decide env f of
  Just (tEnv, fEnv) -> subEvalSat tEnv f <|> subEvalSat fEnv f
  Nothing -> if eval f then (Just env) else Nothing
  where
    subEvalSat env = (sat env) . simplify . (substitute env)

satParse :: String -> Either ParseError (Maybe Env)
satParse s = (sat Map.empty) <$> (parseFormula s)