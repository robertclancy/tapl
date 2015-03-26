module Arbitrary where

import Test.QuickCheck
import Language.Inference.Syntax
import Control.Monad

instance Arbitrary Term where
        arbitrary = sized aTerm where
            aTerm :: (Int -> Gen Term)
            aTerm 0 = oneof [
                            liftM TmVar arbitraryIdentifier,
                            return TmTrue,
                            return TmFalse,
                            return TmZero
                            ]
            aTerm n = oneof [
                            (liftM2 TmAbs) arbitraryIdentifier (aTerm $ pred n),
                            (liftM TmSucc) (aTerm $ pred n),
                            recTerm,
                            ifTerm,
                            appTerm
                            ] where
                                recTerm = do s <- choose (0, pred n)
                                             (liftM2 TmRec) (aTerm s) (aTerm $ n - s - 1)
                                ifTerm = do a <- choose (0, pred n)
                                            b <- choose (0, pred n)
                                            c <- choose (0, pred n)
                                            (liftM3 TmIf) (aTerm a) (aTerm b) (aTerm c)
                                appTerm = do s <- choose (0, pred n)
                                             (liftM2 TmApp) (aTerm s) (aTerm $ n - s - 1)
        shrink TmTrue = []
        shrink TmFalse = []
        shrink TmZero = []
        shrink (TmVar _) = []
        shrink (TmSucc x) = [x] ++ [TmSucc x' | x' <- shrink x]
        shrink (TmRec x y) = [x, y] ++ [TmRec x' y' | (x', y') <- shrink (x, y)]
        shrink (TmAbs ident x) = [x] ++ [TmAbs ident x' | x' <- shrink x]
        shrink (TmApp x y) = [x, y] ++ [TmApp x' y' | (x', y') <- shrink (x, y)]
        shrink (TmIf x y z) = [x, y, z] ++ [TmIf x' y' z' | (x', y', z') <- shrink (x, y, z)]

arbitraryIdentifier :: Gen String
arbitraryIdentifier = suchThat (listOf1 alpha) isNotReserved where
    alpha = choose ('a', 'z')
    isNotReserved x = notElem x ["if", "then", "else", "true", "false", "let", "in", "0", "succ", "rec"]

