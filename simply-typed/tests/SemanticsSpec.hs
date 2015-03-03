{-# LANGUAGE FlexibleInstances #-}
module SemanticsSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Control.Monad
import Language.SimplyTyped.Semantics
import Language.SimplyTyped.Syntax

instance Arbitrary Type where
        arbitrary = sized ty where
            ty 0 = return TyBool
            ty n = do s <- choose (0, pred n)
                      (liftM2 TyArr) (ty s) (ty $ n - s - 1)
        shrink TyBool = []
        shrink (TyArr x y) = [x, y] ++ [TyArr x' y' | (x', y') <- shrink (x, y)]

instance (Arbitrary a) => Arbitrary (Term a a) where
        arbitrary = sized (aTerm []) where
            aTerm :: (Arbitrary a) => [a] -> Int -> Gen (Term a a)
            aTerm ids 0 = oneof [
                                liftM TmVar (elements ids),
                                return TmTrue,
                                return TmFalse
                                ]
            aTerm ids n = oneof [
                                do
                                    id <- arbitrary
                                    ty <- arbitrary
                                    bd <- aTerm (id : ids) (pred n)
                                    return $ TmAbs id ty bd,
                                ifTerm,
                                appTerm
                                ] where
                                    ifTerm = do a <- choose (0, pred n)
                                                b <- choose (0, pred n)
                                                c <- choose (0, pred n)
                                                (liftM3 TmIf) (aTerm ids a) (aTerm ids b) (aTerm ids c)
                                    appTerm = do s <- choose (0, pred n)
                                                 (liftM2 TmApp) (aTerm ids s) (aTerm ids $ n - s - 1)
        shrink TmTrue = []
        shrink TmFalse = []
        shrink (TmVar _) = []
        shrink (TmAbs _ _ x) = [x] ++ shrink x
        shrink (TmApp x y) = [x, y] ++ [TmApp x' y' | (x', y') <- shrink (x, y)]
        shrink (TmIf x y z) = [x, y, z] ++ [TmIf x' y' z' | (x', y', z') <- shrink (x, y, z)]

spec :: Spec
spec = do
        describe "unname" $ do
            prop "should be inverse to name for correctly scoped terms" $
                \x -> either (\_ -> True) (\y -> y == x) $ (name <=< unname) (x :: Term String String)
