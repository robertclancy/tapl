{-# LANGUAGE FlexibleInstances #-}
module PrettySpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Control.Monad
import Language.SimplyTyped.Syntax
import Language.SimplyTyped.Parser
import Language.SimplyTyped.Pretty
import Text.PrettyPrint (render)

instance Arbitrary Type where
        arbitrary = sized ty where
            ty 0 = return TyBool
            ty n = do s <- choose (0, pred n)
                      (liftM2 TyArr) (ty s) (ty $ n - s - 1)
        shrink TyBool = []
        shrink (TyArr x y) = [x, y] ++ [TyArr x' y' | (x', y') <- shrink (x, y)]

instance Arbitrary (Term String) where
        arbitrary = sized aTerm where
            aTerm 0 = oneof [
                            liftM TmVar arbitraryIdentifier,
                            return TmTrue,
                            return TmFalse
                            ]
            aTerm n = oneof [
                            (liftM3 TmAbs) arbitraryIdentifier arbitrary (aTerm $ pred n),
                            ifTerm,
                            appTerm
                            ] where
                                ifTerm = do a <- choose (0, pred n)
                                            b <- choose (0, pred n)
                                            c <- choose (0, pred n)
                                            (liftM3 TmIf) (aTerm a) (aTerm b) (aTerm c)
                                appTerm = do s <- choose (0, pred n)
                                             (liftM2 TmApp) (aTerm s) (aTerm $ n - s - 1)
        shrink TmTrue = []
        shrink TmFalse = []
        shrink (TmVar _) = []
        shrink (TmAbs _ _ x) = [x] ++ shrink x
        shrink (TmApp x y) = [x, y] ++ [TmApp x' y' | (x', y') <- shrink (x, y)]
        shrink (TmIf x y z) = [x, y, z] ++ [TmIf x' y' z' | (x', y', z') <- shrink (x, y, z)]

arbitraryIdentifier :: Gen String
arbitraryIdentifier = suchThat (listOf1 alpha) isNotReserved where
    alpha = choose ('a', 'z')
    isNotReserved x = notElem x ["if", "then", "else", "true", "false"]

spec :: Spec
spec = do
        describe "printing" $ do
            prop "should be a right inverse of parsing" $
                \x -> either (\x -> False) (\y -> x == y) $ (readTerm . showTerm) x
