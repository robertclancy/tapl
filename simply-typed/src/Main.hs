module Main (main) where

import Language.SimplyTyped.Parser
import Language.SimplyTyped.Pretty
import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings loop
       where
             loop :: InputT IO ()
             loop = do
                 minput <- getInputLine "> "
                 case minput of
                     Nothing -> return ()
                     Just input -> do outputStrLn $ either show showTerm . readTerm $ input
                                      loop
