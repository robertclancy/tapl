module SimpleRepl (repl) where

repl :: (String -> String) -> IO ()
repl f = interact (eachLine f)

eachLine :: (String -> String) -> (String -> String)
eachLine f = unlines . map f . lines
