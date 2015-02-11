module Lambda where

data NamedTerm = NVar String | NAbs String NamedTerm | NApp NamedTerm NamedTerm
               deriving Show

data Term = Var Integer | Abs Term | App Term Term
