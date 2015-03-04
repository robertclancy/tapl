module Main (main) where

import Data.Bifunctor
import Language.SimplyTyped.Parser
import Language.SimplyTyped.Pretty
import Language.SimplyTyped.Semantics
import Language.SimplyTyped.Interpreter
import System.Console.Haskeline

evalString :: String -> String
evalString input = either id id $ evalString' input where
    evalString' :: String -> Either String String
    evalString' input = do
        term <- first show $ readTerm input
        unnamedTerm <- first show $ unname term
        first show $ typeof unnamedTerm
        value <- first show $ name $ eval unnamedTerm
        return $ showTerm value

main :: IO ()
main = runInputT defaultSettings loop
       where
             loop :: InputT IO ()
             loop = do
                 minput <- getInputLine "> "
                 case minput of
                     Nothing -> return ()
                     Just input -> do outputStrLn $ evalString input
                                      loop
