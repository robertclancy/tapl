module Language.Inference.Interpreter (evaluate) where

import Data.List
import Control.Monad
import Language.Inference.Syntax

data RuntimeError = UndefinedVariable String

instance Show RuntimeError where
        show (UndefinedVariable x) = "undefined variable: " ++ x

evaluate :: Term -> Either RuntimeError Term
evaluate term = do
        uterm <- unname term
        name $ eval uterm

type UTerm = TermG String Integer

-- convert to unnamed form
scoped :: (Monad m) => (v -> [s] -> m a) -> TermG s v -> m (TermG s a)
scoped changeVariable term = scoped' term [] where
    scoped' (TmVar x) ctx      = fmap TmVar $ changeVariable x ctx
    scoped' (TmAbs s b) ctx = do
        b' <- scoped' b $ s : ctx
        return $ TmAbs s b'
    -- scoped' (TmLet s v b) ctx = do
    --    v' <- scoped' v ctx
    --    b' <- scoped' b $ s : ctx
    --    return $ TmLet s v' b'
    scoped' (TmApp f a) ctx    = do
        f' <- scoped' f ctx
        a' <- scoped' a ctx
        return $ TmApp f' a'
    scoped' TmTrue _           = return TmTrue
    scoped' TmFalse _          = return TmFalse
    scoped' (TmIf b t f) ctx   = do
        b' <- scoped' b ctx
        t' <- scoped' t ctx
        f' <- scoped' f ctx
        return $ TmIf b' t' f'
    scoped' TmZero _           = return TmZero
    scoped' (TmSucc n) ctx     = do
        n' <- scoped' n ctx
        return $ TmSucc n'
    scoped' (TmRec z f) ctx    = do
        z' <- scoped' z ctx
        f' <- scoped' f ctx
        return $ TmRec z' f'

unname :: Term -> Either RuntimeError UTerm
unname = scoped indexOfVar where
    indexOfVar x ctx = case elemIndex x ctx of
                           Just index -> Right $ toInteger index
                           Nothing -> Left (UndefinedVariable x)

name :: UTerm -> Either RuntimeError Term
name = scoped nameVar where
    nameVar x ctx = Right $ genericIndex ctx x

-- evaluation
eval :: UTerm -> UTerm
eval TmTrue                         = TmTrue
eval TmFalse                        = TmFalse
eval (TmIf TmTrue t f)              = eval t
eval (TmIf TmFalse t f)             = eval f
eval (TmIf b t f)                   = eval $ TmIf (eval b) t f
eval TmZero                         = TmZero
eval (TmSucc n)                     = TmSucc $ eval n
-- eval (TmLet s v b)            = eval $ shift (-1) $ substitute (eval v) b
eval (TmVar x)                      = TmVar x
eval (TmAbs a b)                    = TmAbs a b
eval (TmApp (TmAbs a b) s)          = eval $ shift (-1) $ substitute (shift 1 (eval s)) b
eval (TmRec z f)                    = TmRec z f
eval (TmApp (TmRec z f) TmZero)     = eval z
eval (TmApp (TmRec z f) (TmSucc n)) = eval $ TmApp (TmApp f n) (TmApp (TmRec z f) n)
eval (TmApp f s)                    = eval $ TmApp (eval f) s

substitute :: UTerm -> UTerm -> UTerm
substitute v x = substitute' 0 v x where
    substitute' :: Integer -> UTerm -> UTerm -> UTerm
    substitute' k v TmTrue    = TmTrue
    substitute' k v TmFalse   = TmFalse
    substitute' k v (TmIf b t f) = TmIf (substitute' k v b) (substitute' k v t) (substitute' k v f)
    substitute' k v TmZero    = TmZero
    substitute' k v (TmSucc n) = TmSucc $ substitute' k v n
    substitute' k v (TmRec z f) = TmRec (substitute' k v z) (substitute' k v f)
    substitute' k v (TmVar x) = if k == x then v else TmVar x
    substitute' k v (TmAbs t b) = TmAbs t $ substitute' (k + 1) (shift 1 v) b
    substitute' k v (TmApp a b) = TmApp (substitute' k v a) (substitute' k v b)
    -- substitute' k v (TmLet s a b) = TmLet s (substitute' k v a) (substitute' (k + 1) (shift 1 v) b)

shift :: Integer -> UTerm -> UTerm
shift m t = shift' m 0 t where
    shift' :: Integer -> Integer -> UTerm -> UTerm
    shift' n c TmTrue = TmTrue
    shift' n c TmFalse = TmFalse
    shift' n c (TmIf b t f) = TmIf (shift' n c b) (shift' n c t) (shift' n c f)
    shift' n c TmZero  = TmZero
    shift' n c (TmSucc x) = TmSucc $ shift' n c x
    shift' n c (TmRec z f) = TmRec (shift' n c z) (shift' n c f)
    shift' n c (TmVar x) = if x < c then TmVar x else TmVar $ x + n
    shift' n c (TmApp s u) = TmApp (shift' n c s) (shift' n c u)
    shift' n c (TmAbs v b) = TmAbs v (shift' n (succ c) b)
    -- shift' n c (TmLet s a b) = TmLet s (shift' n c a) (shift' n (succ c) b)
