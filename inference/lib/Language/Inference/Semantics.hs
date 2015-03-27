{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Inference.Semantics where
-- Reference: Generalizing Hindley-Milner Type Inference Algorithms (2002)

import Language.Inference.Syntax
import Data.Foldable (foldrM)
import Data.Maybe
import Data.Monoid
import Control.Monad.State
import qualified Data.Set as Set
import qualified Data.Map as Map

data TyMono = TyBool |
            TyNat |
            TyArr TyMono TyMono |
            TyVar Integer
            deriving (Eq, Show)

data TyPoly = TyPoly (Set.Set Integer) TyMono

type Context = Map.Map String TyPoly

data SemanticError = UndefinedVariable String |
                   UnificationFailure TyMono TyMono
                   deriving (Eq)

instance Show SemanticError where
        show (UndefinedVariable var) = "undefined variable: " ++ var
        show (UnificationFailure x y) = "cannot unify: " ++ (show x) ++ " and " ++ (show y)


fresh :: (Monad m) => StateT Integer m Integer
fresh = modify (+1) >> get

freshMono :: (Monad m) => StateT Integer m TyMono
freshMono = fresh >>= (\x -> return $ TyVar x)

-- Substitution
newtype Substitution = Substitution (Map.Map Integer TyMono) deriving Monoid

singleton :: Integer -> TyMono -> Substitution
singleton key val = Substitution $ Map.singleton key val

remove :: Set.Set Integer -> Substitution -> Substitution
remove s (Substitution substitution) = Substitution $ foldr Map.delete substitution s

apply :: Substitution -> Integer -> Maybe TyMono
apply (Substitution substitution) i = Map.lookup i substitution

substitute :: Substitution -> TyMono -> TyMono
substitute _ TyBool = TyBool
substitute _ TyNat  = TyNat
substitute s (TyArr a b) = TyArr (substitute s a) (substitute s b)
substitute s (TyVar i) = fromMaybe (TyVar i) (apply s i)

substitutePoly :: Substitution -> TyPoly -> TyPoly
substitutePoly substitution (TyPoly bindings monotype) = TyPoly bindings monoSub where
    monoSub = substitute (remove bindings substitution) monotype

substituteCtx :: Substitution -> Context -> Context
substituteCtx x = Map.map $ substitutePoly x

-- Generalization and instantiation
generalize :: Context -> TyMono -> TyPoly
generalize context monotype = TyPoly free monotype where
    free = Set.difference (freeVarsMono monotype) (freeVarsCtx context)

instantiate :: (Monad m) => TyPoly -> StateT Integer m TyMono
instantiate (TyPoly bindings monotype) = do
        freshSubstitution <- freshVars bindings
        return $ substitute freshSubstitution monotype
        where
            freshVars = foldrM build mempty where
                build key substitution = do
                    new <- fresh
                    return $ singleton key (TyVar new) <> substitution

freeVarsMono :: TyMono -> Set.Set Integer
freeVarsMono TyBool = Set.empty
freeVarsMono TyNat  = Set.empty
freeVarsMono (TyVar i) = Set.singleton i
freeVarsMono (TyArr x y) = Set.union (freeVarsMono x) (freeVarsMono y)

freeVars :: TyPoly -> Set.Set Integer
freeVars (TyPoly bindings mt) = Set.difference (freeVarsMono mt) bindings

freeVarsCtx :: Context -> Set.Set Integer
freeVarsCtx ctx = Set.unions . map freeVars . Map.elems $ ctx

-- Unification
unify :: TyMono -> TyMono -> Either SemanticError Substitution
unify TyBool TyBool = Right mempty
unify TyNat  TyNat  = Right mempty
unify (TyVar x) (TyVar y) | x == y = Right mempty
unify (TyVar var) x | Set.notMember var (freeVarsMono x) = Right $ singleton var x
unify x (TyVar var) | Set.notMember var (freeVarsMono x) = Right $ singleton var x
unify (TyArr a b) (TyArr x y) = do
        s <- unify a x
        t <- unify (substitute s b) (substitute s y)
        return $ t <> s
unify x@_ y@_ = Left $ UnificationFailure x y

-- Algorithm W
infer :: Term -> Either SemanticError TyMono
infer term = (evalStateT (inferWithState Map.empty term) 0) >>= return . snd

inferWithState :: Context -> Term -> StateT Integer (Either SemanticError) (Substitution, TyMono)
inferWithState _ TmTrue  = return $ (mempty, TyBool)
inferWithState _ TmFalse = return $ (mempty, TyBool)
inferWithState _ TmZero  = return $ (mempty, TyNat)
inferWithState context (TmVar i) = do
        typoly <- lift typescheme
        tymono <- instantiate typoly
        return $ (mempty, tymono)
        where
            typescheme = case Map.lookup i context of
                             Just polytype -> Right polytype
                             Nothing -> Left $ UndefinedVariable i
inferWithState context (TmAbs var body) = do
        new <- freshMono
        let newPoly = TyPoly Set.empty new
        (sub, ty) <- inferWithState (Map.insert var newPoly context) body
        return $ (sub, TyArr (substitute sub new) ty)
inferWithState context (TmApp f x) = do
        (s1, t1) <- inferWithState context f
        (s2, t2) <- inferWithState (substituteCtx s1 context) x
        new <- freshMono
        s3 <- lift $ unify (substitute s2 t1) (TyArr t2 new)
        return (s3 <> s2 <> s1, substitute s3 new)
inferWithState context (TmIf b t f) = do
        (s1, t1) <- inferWithState context b
        s2 <- lift $ unify t1 TyBool
        (s3, t3) <- inferWithState (substituteCtx (s2 <> s1) context) t
        (s4, t4) <- inferWithState (substituteCtx (s3 <> s2 <> s1) context) f
        s5 <- lift $ unify (substitute s4 t3) t4
        return (s5 <> s4 <> s3 <> s2 <> s1, (substitute s5 t4))
inferWithState context (TmSucc n) = do
        (s1, t1) <- inferWithState context n
        s2 <- lift $ unify t1 TyNat
        return (s2 <> s1, substitute s2 t1)
inferWithState context (TmRec base ind) = do
        (s1, t1) <- inferWithState context base
        (s2, t2) <- inferWithState (substituteCtx s1 context) ind
        s3 <- lift $ unify t2 (TyArr TyNat (TyArr (substitute s2 t1) (substitute s2 t1)))
        return (s3 <> s2 <> s1, TyArr TyNat (substitute (s3 <> s2) t1))
