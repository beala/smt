{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module SMT.Types
( Formula(T, F, And, Or, Not, Var)
, Env
) where

import qualified Data.Map.Strict as Map
import Data.Typeable
import GHC.Generics

data Formula = T
             | F
             | And Formula Formula
             | Or Formula Formula
             | Not Formula
             | Var String
             deriving (Show, Eq, Generic, Typeable)

type Env = Map.Map String Formula