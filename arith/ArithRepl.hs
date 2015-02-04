module Main (main) where

import Arith
import ArithParser
import ArithPrettyPrint
import SimpleRepl

main :: IO ()
main = repl $ either show (render_term . eval) . parse_term
