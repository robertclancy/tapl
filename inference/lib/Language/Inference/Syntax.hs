module Language.Inference.Syntax where

data Term = TmVar String | TmAbs String Term | TmApp Term Term |
          TmTrue | TmFalse | TmIf Term Term Term |
          TmZero | TmSucc Term | TmRec Term Term
          deriving (Show, Eq)
