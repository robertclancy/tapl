{-# LANGUAGE DeriveFunctor #-}
module Language.SimplyTyped.Syntax where

data Type = TyBool | TyArr Type Type
          deriving (Show, Eq)
data Term v s = TmVar v | TmAbs s Type (Term v s) | TmApp (Term v s) (Term v s) |
              TmTrue | TmFalse | TmIf (Term v s) (Term v s) (Term v s) |
              TmLet s (Term v s) (Term v s)
              deriving (Show, Eq, Functor)
