module ParserSpec (spec) where

import Test.Hspec
import Language.SimplyTyped.Syntax
import Language.SimplyTyped.Parser
import Text.Parsec
import Text.Parsec.Error

type PTerm = Term String String

parseTerm :: String -> Either ParseError PTerm
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
            it "should parse identifiers that start with reserved words" $ do
                parseTerm "truely" `shouldBe` Right (TmVar "truely")
                parseTerm "if_statement" `shouldBe` Right (TmVar "if_statement")
        describe "if statements" $ do
            it "should parse basic if" $ do
                parseTerm "if true then true else false" `shouldBe` Right (TmIf TmTrue TmTrue TmFalse)
            it "should parse nested ifs" $ do
                parseTerm "if if if false then true else false then true else false then true else false" `shouldBe` Right (TmIf (TmIf (TmIf TmFalse TmTrue TmFalse) TmTrue TmFalse) TmTrue TmFalse)
            it "should fail with incomplete if statement" $ do
                parseTerm "if true then false" `shouldSatisfy` isLeft
            it "should handle abstractions in branches" $ do
                parseTerm "if (\\x : Bool. y) then x else y" `shouldBe` Right (TmIf (TmAbs "x" TyBool (TmVar "y")) (TmVar "x") (TmVar "y"))
                parseTerm "if true then \\x : Bool. if x then true else false else \\y:Bool. y" `shouldBe` Right (TmIf TmTrue (TmAbs "x" TyBool (TmIf (TmVar "x") TmTrue TmFalse)) (TmAbs "y" TyBool (TmVar "y")))
            it "should extend as far to the right as possible" $ do
                parseTerm "if true then true else f x" `shouldBe` Right (TmIf TmTrue TmTrue (TmApp (TmVar "f") (TmVar "x")))
            it "should handle applications in branches" $ do
                parseTerm "if x y then f g else f x" `shouldBe` Right (TmIf (TmApp (TmVar "x") (TmVar "y")) (TmApp (TmVar "f") (TmVar "g")) (TmApp (TmVar "f") (TmVar "x")))
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
                parseTerm "\\x: Bool. x true" `shouldBe` Right (TmAbs "x" TyBool (TmApp (TmVar "x") TmTrue))
            it "should handle nested abstractions" $ do
                parseTerm "\\x: Bool -> Bool. \\y:Bool. x y" `shouldBe` Right (TmAbs "x" (TyArr TyBool TyBool) (TmAbs "y" TyBool (TmApp (TmVar "x") (TmVar "y"))))
            it "should parse an expression with undefined variables" $ do
                parseTerm "\\z: Bool. x" `shouldBe` Right (TmAbs "z" TyBool (TmVar "x"))
            it "should error if abstraction has no type" $ do
                parseTerm "\\x. x" `shouldSatisfy` isLeft
        describe "let" $ do
            it "should parse let expression" $ do
                parseTerm "let x = true in x" `shouldBe` Right (TmLet "x" TmTrue (TmVar "x"))
            it "should parse as far to the right as possible" $ do
                parseTerm "let x = true in x y" `shouldBe` Right (TmLet "x" TmTrue (TmApp (TmVar "x") (TmVar "y")))
            it "should handle nested lets" $ do
                parseTerm "let x = let y = true in y z in x" `shouldBe` Right (TmLet "x" (TmLet "y" TmTrue (TmApp (TmVar "y") (TmVar "z"))) (TmVar "x"))
        describe "type declarations" $ do
            it "should parse type declarations right associatively" $ do
                parseTerm "\\x : (Bool -> Bool -> Bool). x" `shouldBe` Right (TmAbs "x" (TyArr TyBool (TyArr TyBool TyBool)) (TmVar "x"))
            it "should handle parens in types" $ do
                parseTerm "\\x : (Bool -> Bool) -> Bool. x" `shouldBe` Right (TmAbs "x" (TyArr (TyArr TyBool TyBool) TyBool) (TmVar "x"))
            it "should not require parens" $ do
                parseTerm "\\z : Bool -> Bool.z" `shouldBe` Right (TmAbs "z" (TyArr TyBool TyBool) (TmVar "z"))
            it "should error on incomplete declarations" $ do
                parseTerm "\\x: Bool -> . z" `shouldSatisfy` isLeft
        describe "applications" $ do
            it "should parse applications" $ do
                parseTerm "(\\x: Bool. true) false" `shouldBe` Right (TmApp (TmAbs "x" TyBool TmTrue) TmFalse)
            it "should parse incorrect applications" $ do
                parseTerm "true false" `shouldBe` Right (TmApp TmTrue TmFalse)
            it "should parse an application with a top level paresn" $ do
                parseTerm "(f x)" `shouldBe` Right (TmApp (TmVar "f") (TmVar "x"))
            it "should associate to the left" $ do
                parseTerm "(\\x: Bool. true) true (\\y: Bool. false)" `shouldBe` Right (TmApp (TmApp (TmAbs "x" TyBool TmTrue) TmTrue) (TmAbs "y" TyBool TmFalse))
            it "should allow parens to associate to the right" $ do
                parseTerm "true (true (true false))" `shouldBe` Right (TmApp TmTrue (TmApp TmTrue (TmApp TmTrue TmFalse)))
            it "should parse applications of if statements" $ do
                parseTerm "(if x then y else z) (if a then b else c)" `shouldBe` Right (TmApp (TmIf (TmVar "x") (TmVar "y") (TmVar "z")) (TmIf (TmVar "a") (TmVar "b") (TmVar "c")))
            it "should parse applications of if statements" $ do
                parseTerm "if x then y else z (if a then b else c)" `shouldBe` Right (TmIf (TmVar "x") (TmVar "y") (TmApp (TmVar "z") (TmIf (TmVar "a") (TmVar "b") (TmVar "c"))))
            it "should error if applied to invalid argument" $ do
                parseTerm "x if a then b else c" `shouldSatisfy` isLeft
                parseTerm "x \\x : Bool. x" `shouldSatisfy` isLeft
