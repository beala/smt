import SMT.SMT

import System.IO
import Control.Monad
import qualified Data.Map as M
import Text.Parsec

main :: IO ()
main = forever repl

repl :: IO ()
repl = do
  putStr "> "
  hFlush stdout
  input <- getLine
  putStrLn $ prettyPrintResult $ satParse input

prettyPrintResult :: Either ParseError (Maybe Env) -> String
prettyPrintResult result = case result of
    Left e -> show e ++ "\n"
    Right (Just env) -> prettyPrintEnv env
    Right (Nothing) -> "Not satisfiable\n"

prettyPrintEnv :: (Show k, Show v) => M.Map k v -> String
prettyPrintEnv = M.foldMapWithKey (\k v -> show k ++ ": " ++ show v ++ "\n")
