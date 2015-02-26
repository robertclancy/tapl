module ParserSpec (spec) where

import Test.Hspec
import Language.SimplyTyped.Syntax
import Language.SimplyTyped.Parser
import Text.Parsec
import Text.Parsec.Error

parseTerm :: String -> Either ParseError (Term String)
parseTerm = parse term ""

-- equality instance needed for equality checking on right argument
instance Eq ParseError where
        x == y = errorMessages x == errorMessages y

isLeft :: Either a b -> Bool
isLeft (Left _)  = True
isLeft (Right _) = False

spec :: Spec
spec = do
        describe "atoms" $ do
            it "should parse true" $ do
                parseTerm "true" `shouldBe` Right TmTrue
            it "should parse false" $ do
                parseTerm "false" `shouldBe` Right TmFalse
            it "should parse variables with underscored" $ do
                parseTerm "some_variable3" `shouldBe` Right (TmVar "some_variable3")
            it "should not parse variables with operator characters" $ do
                parseTerm "$" `shouldSatisfy` isLeft
            it "should not parse identifiers that start with numbers" $ do
                parseTerm "3some" `shouldSatisfy` isLeft
        describe "if statements" $ do
            it "should parse basic if" $ do
                parseTerm "if true true false" `shouldBe` Right (TmIf TmTrue TmTrue TmFalse)
            it "should parse nested ifs" $ do
                parseTerm "if if if false true false true false true false" `shouldBe` Right (TmIf (TmIf (TmIf TmFalse TmTrue TmFalse) TmTrue TmFalse) TmTrue TmFalse)
            it "should fail with incomplete if statement" $ do
                parseTerm "if true false" `shouldSatisfy` isLeft
            it "should handle abstractions in branches" $ do
                parseTerm "if (\\x : Bool. y) x y" `shouldBe` Right (TmIf (TmAbs "x" TyBool (TmVar "y")) (TmVar "x") (TmVar "y"))
        describe "abstractions" $ do
            it "should parse abstraction" $ do
                parseTerm "\\x : Bool. x" `shouldBe` Right (TmAbs "x" TyBool (TmVar "x"))
            it "should ignore whitespace" $ do
                parseTerm "\\x: Bool. x" `shouldBe` Right (TmAbs "x" TyBool (TmVar "x"))
                parseTerm "\\x :Bool. x" `shouldBe` Right (TmAbs "x" TyBool (TmVar "x"))
                parseTerm "\\x : Bool . x" `shouldBe` Right (TmAbs "x" TyBool (TmVar "x"))
                parseTerm "\\x : Bool .x" `shouldBe` Right (TmAbs "x" TyBool (TmVar "x"))
            it "should extend as far to the right as possible" $ do
                parseTerm "\\x : (Bool -> (Bool -> Bool)).  (x true) true" `shouldBe` Right (TmAbs "x" (TyArr TyBool (TyArr TyBool TyBool)) (TmApp (TmApp (TmVar "x") TmTrue) TmTrue))
            it "should handle nested abstractions" $ do
                parseTerm "\\x: Bool -> Bool. \\y:Bool. x y" `shouldBe` Right (TmAbs "x" (TyArr TyBool TyBool) (TmAbs "y" TyBool (TmApp (TmVar "x") (TmVar "y"))))
            it "should parse an expression with undefined variables" $ do
                parseTerm "\\z: Bool. x" `shouldBe` Right (TmAbs "z" TyBool (TmVar "x"))
            it "should error if abstraction has no type" $ do
                parseTerm "\\x. x" `shouldSatisfy` isLeft
        describe "type declarations" $ do
            it "should parse type declarations right associatively" $ do
                parseTerm "\\x : (Bool -> Bool -> Bool). x" `shouldBe` Right (TmAbs "x" (TyArr TyBool (TyArr TyBool TyBool)) (TmVar "x"))
            it "should handle parens in types" $ do
                parseTerm "\\x : (Bool -> Bool) -> Bool. x" `shouldBe` Right (TmAbs "x" (TyArr (TyArr TyBool TyBool) TyBool) (TmVar "x"))
            it "should not require parens" $ do
                parseTerm "\\z : Bool -> Bool.z" `shouldBe` Right (TmAbs "z" (TyArr TyBool TyBool) (TmVar "z"))
        describe "applications" $ do
            it "should parse applications" $ do
                parseTerm "(\\x: Bool. true) false" `shouldBe` Right (TmApp (TmAbs "x" TyBool TmTrue) TmFalse)
            it "should parse incorrect applications" $ do
                parseTerm "true false" `shouldBe` Right (TmApp TmTrue TmFalse)
            it "should associate to the left" $ do
                parseTerm "(\\x: Bool. true) true (\\y: Bool. false)" `shouldBe` Right (TmApp (TmApp (TmAbs "x" TyBool TmTrue) TmTrue) (TmAbs "y" TyBool TmFalse))
            it "should allow parens to associate to the right" $ do
                parseTerm "true (true (true false))" `shouldBe` Right (TmApp TmTrue (TmApp TmTrue (TmApp TmTrue TmFalse)))
            it "should parse applications of if statements" $ do
                parseTerm "if x y z if a b c" `shouldBe` Right (TmApp (TmIf (TmVar "x") (TmVar "y") (TmVar "z")) (TmIf (TmVar "a") (TmVar "b") (TmVar "c")))
