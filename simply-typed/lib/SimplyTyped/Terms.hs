module SimplyTyped.Terms where

data Type = TyBool | TyArr Type Type
          deriving (Show, Eq)
data Term a = TmVar a | TmAbs a Type (Term a) | TmApp (Term a) (Term a) |
              TmTrue | TmFalse | TmIf (Term a) (Term a) (Term a)
              deriving (Show)
