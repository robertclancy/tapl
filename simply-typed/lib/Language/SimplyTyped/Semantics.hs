module Language.SimplyTyped.Semantics (unname, name, alphaEquiv, typeof) where

import Language.SimplyTyped.Syntax
import Data.List (elemIndex, genericIndex)

data SemanticError = SemanticError String

scoped :: (v -> [s] -> Either SemanticError a) -> Term v s -> Either SemanticError (Term a s)
scoped changeVariable term = scoped' term [] where
    scoped' (TmVar x) ctx      = fmap TmVar $ changeVariable x ctx
    scoped' (TmAbs s ty b) ctx = do
        b' <- scoped' b $ s : ctx
        return $ TmAbs s ty b'
    scoped' (TmApp f a) ctx    = do
        f' <- scoped' f ctx
        a' <- scoped' a ctx
        return $ TmApp f' a'
    scoped' TmTrue _           = Right TmTrue
    scoped' TmFalse _          = Right TmFalse
    scoped' (TmIf b t f) ctx   = do
        b' <- scoped' b ctx
        t' <- scoped' t ctx
        f' <- scoped' f ctx
        return $ TmIf b' t' f'

unname :: (Eq s) => Term s s -> Either SemanticError (Term Integer s)
unname = scoped indexOfVar where
    indexOfVar x ctx = case elemIndex x ctx of
                           Just index -> Right $ toInteger index
                           Nothing -> Left (SemanticError $ "undefined variable")

name :: (Eq s) => Term Integer s -> Either SemanticError (Term s s)
name = scoped nameVar where
    nameVar x ctx = Right $ genericIndex ctx x

eraseName :: Term v s -> Term v ()
eraseName = fmap (\_ -> ())

alphaEquiv :: (Eq s) => Term s s -> Term s s -> Either SemanticError Bool
alphaEquiv x y = do
        x' <- unname x
        y' <- unname y
        return $ (eraseName x') == (eraseName y')

typeof :: Term Integer s -> Either SemanticError Type
typeof = typeof' [] where
    typeof' :: [s] -> Term Integer s -> Either SemanticError Type
    typeof' _ TmTrue = Right TyBool
    typeof' _ TmFalse = Right TyBool
    typeof' s (TmIf b t f) = do
        tb <- typeof' s b
        tt <- typeof' s t
        tf <- typeof' s f
        checkType tb TyBool
        checkType tt tf
        return tt
    typeof' s (TmAbs a ty b) = do
        tb <- typeof' (ty : s) b
        return $ TyArr ty tb
    typeof' s (TmApp f a) = do
        tf <- typeof' s f
        ta <- typeof' s a
        case tf of
            TyArr tfa tb -> checkType tfa ta >> return tb
    typeof' s (TmVar v) = Right $ genericIndex s v

    checkType :: Type -> Type -> Either SemanticError ()
    checkType x y = if x == y then Right () else Left $ SemanticError $ "expected type " ++ show x ++ " to equal " ++ show y
