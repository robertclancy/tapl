module Main (main) where

import Language.Inference.Parser
import Language.Inference.Pretty
import Language.Inference.Semantics
import System.Console.Haskeline
import Data.Bifunctor

evalString :: String -> String
evalString input = either id id $ do
    term <- first show $ readTerm input
    ty   <- first show $ infer term
    return $ showTermType term ty

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
