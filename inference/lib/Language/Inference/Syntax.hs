module Language.Inference.Syntax where

data TermG bind var = TmVar var | TmAbs bind (TermG bind var) | TmApp (TermG bind var) (TermG bind var) |
                    TmTrue | TmFalse | TmIf (TermG bind var) (TermG bind var) (TermG bind var) |
                    TmZero | TmSucc (TermG bind var) | TmRec (TermG bind var) (TermG bind var)
                    deriving (Show, Eq)

type Term = TermG String String
