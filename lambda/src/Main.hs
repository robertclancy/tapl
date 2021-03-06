module Main (main) where

import LambdaParser
import Lambda
import LambdaPrinter
import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings loop
       where
             loop :: InputT IO ()
             loop = do
                 minput <- getInputLine "> "
                 case minput of
                     Nothing -> return ()
                     Just input -> do outputStrLn $ either show (render_term . evalNamedTerm) . parse_term $ input
                                      loop
