-- Haskell REPL

module Repl where
    import System.IO (Handle, isEOF, hIsTerminalDevice, stdin, stderr, hFlush,
                     hPutStr, hPutChar)

    repl :: (String -> String) -> IO ()
    repl = replWithPrompt "> "

    replWithPrompt :: String -> (String -> String) -> IO ()
    replWithPrompt prompt eval = do
                                 term <- hIsTerminalDevice stdin
                                 loop term
                                 where
                                     loop term = do
                                         if term then (flushStr stderr prompt) else return ()
                                         end <- isEOF
                                         if end then (exitFromRepl term) else nextLine
                                                where
                                                    nextLine = do
                                                        input <- getLine
                                                        let result = eval input
                                                        putStrLn result
                                                        loop term
                                     flushStr :: Handle -> String -> IO ()
                                     flushStr h str = (hPutStr h str) >> hFlush h
                                     exitFromRepl :: Bool -> IO ()
                                     exitFromRepl term = do if term then (hPutChar stderr '\n') else return ()
