module InferenceSpec (spec) where

import Test.Hspec

import Language.Inference.Syntax
import Language.Inference.Semantics

import Data.Either

spec :: Spec
spec = do
        describe "infering types" $ do
            -- TODO: these tests are brittle due to the explicit numbering
            it "should infer the identity" $ do
                infer (TmAbs "x" (TmVar "x")) `shouldBe` Right (TyArr (TyVar 1) (TyVar 1))
            it "should infer the church booleans" $ do
                infer (TmAbs "x" (TmAbs "y" (TmVar "x"))) `shouldBe` Right (TyArr (TyVar 1) (TyArr (TyVar 2) (TyVar 1)))
            it "should infer true" $ do
                infer TmTrue `shouldBe` Right TyBool
            it "should infer false" $ do
                infer TmFalse `shouldBe` Right TyBool
            it "should infer zero" $ do
                infer TmZero `shouldBe` Right TyNat
            it "should infer one" $ do
                infer (TmSucc TmZero) `shouldBe` Right TyNat
            it "should infer recursive function definitions" $ do
                infer (TmRec TmTrue (TmAbs "y" TmFalse)) `shouldBe` Right (TyArr TyNat TyBool)
            it "should infer the argument of an if statement" $ do
                infer (TmAbs "x" (TmIf (TmVar "x") TmZero (TmSucc (TmZero)))) `shouldBe` Right (TyArr TyBool TyNat)
            it "should infer the result of an if statement" $ do
                infer (TmAbs "x" (TmIf TmTrue (TmVar "x") TmZero)) `shouldBe` Right (TyArr TyNat TyNat)
            it "should correctly determine result of application" $ do
                infer (TmApp (TmAbs "x" (TmVar "x")) TmZero) `shouldBe` Right TyNat
            it "should correctly determine result of application of plus" $ do
                infer (TmApp (TmRec (TmAbs "x" (TmVar "x")) (TmAbs "f" (TmAbs "x" (TmSucc (TmApp (TmVar "f") (TmVar "x")))))) (TmSucc TmZero)) `shouldBe` Right (TyArr TyNat TyNat)
        describe "let" $ do
            it "should be polymorphic" $ do
                infer (TmLet "x" (TmAbs "x" (TmVar "x")) (TmIf (TmApp (TmVar "x") TmTrue) (TmApp (TmVar "x") TmZero) (TmApp (TmVar "x") (TmZero)))) `shouldBe` Right TyNat
        describe "invalid types" $ do
            it "should error if undefined variable" $ do
                infer (TmVar "x") `shouldSatisfy` isLeft



