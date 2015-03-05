module Language.SimplyTyped.Semantics (unname, name, alphaEquiv, typeof) where

import Language.SimplyTyped.Syntax
import Data.List (elemIndex, genericIndex)

data SemanticError = SemanticError String

instance Show SemanticError where
        show (SemanticError x) = x

scoped :: (v -> [s] -> Either SemanticError a) -> Term v s -> Either SemanticError (Term a s)
scoped changeVariable term = scoped' term [] where
    scoped' (TmVar x) ctx      = fmap TmVar $ changeVariable x ctx
    scoped' (TmAbs s ty b) ctx = do
        b' <- scoped' b $ s : ctx
        return $ TmAbs s ty b'
    scoped' (TmLet s v b) ctx = do
        v' <- scoped' v ctx
        b' <- scoped' b $ s : ctx
        return $ TmLet s v' b'
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

unname :: Term String String -> Either SemanticError (Term Integer String)
unname = scoped indexOfVar where
    indexOfVar x ctx = case elemIndex x ctx of
                           Just index -> Right $ toInteger index
                           Nothing -> Left (SemanticError $ "undefined variable " ++ x)

-- this works, I think for closed terms, which is all we allow since
-- capture occurs when a term with a free variable is substituted into
-- another. Since we use call by value, we never evaluate under an
-- abstraction. This means that all terms that are substituted as arguments
-- are closed.
name :: (Eq s) => Term Integer s -> Either SemanticError (Term s s)
name = scoped nameVar where
    nameVar x ctx = Right $ genericIndex ctx x

eraseName :: Term v s -> Term v ()
eraseName = fmap (\_ -> ())

alphaEquiv :: Term String String -> Term String String -> Either SemanticError Bool
alphaEquiv x y = do
        x' <- unname x
        y' <- unname y
        return $ (eraseName x') == (eraseName y')

typeof :: Term Integer s -> Either SemanticError Type
typeof = typeof' [] where
    typeof' :: [Type] -> Term Integer s -> Either SemanticError Type
    typeof' _ TmTrue = Right TyBool
    typeof' _ TmFalse = Right TyBool
    typeof' s (TmIf b t f) = do
        tb <- typeof' s b
        tt <- typeof' s t
        tf <- typeof' s f
        checkType tb TyBool
        checkType tt tf
        return tt
    typeof' s (TmLet a v b) = do
        vt <- typeof' s v
        typeof' (vt : s) b
    typeof' s (TmAbs a ty b) = do
        tb <- typeof' (ty : s) b
        return $ TyArr ty tb
    typeof' s (TmApp f a) = do
        tf <- typeof' s f
        ta <- typeof' s a
        case tf of
            TyArr tfa tb -> checkType tfa ta >> return tb
            _            -> Left $ SemanticError "applied non-function type"
    typeof' s (TmVar v) = Right $ genericIndex s v

    checkType :: Type -> Type -> Either SemanticError ()
    checkType x y = if x == y then Right () else Left $ SemanticError $ "expected type " ++ show x ++ " to equal " ++ show y
