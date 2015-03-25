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

data TyPoly = TyPoly (Set.Set Integer) TyMono

type Context = Map.Map String TyPoly

data SemanticError = UndefinedVariable String |
                   UnificationFailure TyMono TyMono

fresh :: State Integer Integer
fresh = modify (+1) >> get

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

instantiate :: TyPoly -> State Integer TyMono
instantiate (TyPoly bindings monotype) = do
        freshSubstitution <- freshVars bindings
        return $ substitute freshSubstitution monotype
        where
            freshVars :: Set.Set Integer -> State Integer Substitution
            freshVars = foldrM build mempty where
                build :: Integer -> Substitution -> State Integer Substitution
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
        t <- unify b y
        return $ s <> t
unify x@_ y@_ = Left $ UnificationFailure x y

-- Algorithm W
infer :: Term -> Either SemanticError TyMono
infer _ = Right TyBool
