module Language.SimplyTyped.Interpreter (eval) where

import Language.SimplyTyped.Syntax

eval :: Term Integer s -> Term Integer s
eval TmTrue                   = TmTrue
eval TmFalse                  = TmFalse
eval (TmIf TmTrue t f)        = eval t
eval (TmIf TmFalse t f)       = eval f
eval (TmIf b t f)             = eval $ TmIf (eval b) t f
eval (TmVar x)                = TmVar x
eval (TmAbs a ty b)           = TmAbs a ty b
eval (TmApp (TmAbs a ty b) s) = eval $ shift (-1) $ substitute (shift 1 (eval s)) b
eval (TmApp f s)              = eval $ TmApp (eval f) s

substitute :: Term Integer s -> Term Integer s -> Term Integer s
substitute v x = substitute' 0 v x where
    substitute' :: Integer -> Term Integer s -> Term Integer s -> Term Integer s
    substitute' k v TmTrue    = TmTrue
    substitute' k v TmFalse   = TmFalse
    substitute' k v (TmIf b t f) = TmIf (substitute' k v b) (substitute' k v t) (substitute' k v f)
    substitute' k v (TmVar x) = if k == x then v else TmVar x
    substitute' k v (TmAbs t ty b) = TmAbs t ty $ substitute' (k + 1) (shift 1 v) b
    substitute' k v (TmApp a b) = TmApp (substitute' k v a) (substitute' k v b)

shift :: Integer -> Term Integer s -> Term Integer s
shift m t = shift' m 0 t where
    shift' :: Integer -> Integer -> Term Integer s -> Term Integer s
    shift' n c TmTrue = TmTrue
    shift' n c TmFalse = TmFalse
    shift' n c (TmIf b t f) = TmIf (shift' n c b) (shift' n c t) (shift' n c f)
    shift' n c (TmVar x) = if x < c then TmVar x else TmVar $ x + n
    shift' n c (TmApp s u) = TmApp (shift' n c s) (shift' n c u)
    shift' n c (TmAbs v ty b) = TmAbs v ty (shift' n (succ c) b)
