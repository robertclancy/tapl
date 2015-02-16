module LambdaSpec (spec) where

import Test.Hspec
import LambdaParser
import LambdaPrinter
import Lambda

evalStr :: String -> String
evalStr = either show (render_term . evalNamedTerm) . parse_term

spec :: Spec
spec = do
        describe "eval" $ do
            it "should evaluate" $ do
                evalStr "((lambda x x) y)" `shouldBe` "y"

        describe "Church booleans" $ do
            it "should treat true correctly" $ do
                evalStr "(((lambda x (lambda y x)) t) f)" `shouldBe` "t"

            it "should treat false correctly" $ do
                evalStr "(((lambda x (lambda y y)) t) f)" `shouldBe` "f"

            describe "AND" $ do
                it "true and true = true" $ do
                    evalStr "((((lambda false ((lambda true ((lambda and ((and true) true)) (lambda b (lambda c ((b c) false))))) (lambda x (lambda y x)))) (lambda x (lambda y y))) t) f)" `shouldBe` "t"
            
                it "false and true = false" $ do
                    evalStr "((((lambda false ((lambda true ((lambda and ((and false) true)) (lambda b (lambda c ((b c) false))))) (lambda x (lambda y x)))) (lambda x (lambda y y))) t) f)" `shouldBe` "f"

                it "true and false = false" $ do
                    evalStr "((((lambda false ((lambda true ((lambda and ((and true) false)) (lambda b (lambda c ((b c) false))))) (lambda x (lambda y x)))) (lambda x (lambda y y))) t) f)" `shouldBe` "f"

                it "false and false = false" $ do
                    evalStr "((((lambda false ((lambda true ((lambda and ((and false) false)) (lambda b (lambda c ((b c) false))))) (lambda x (lambda y x)))) (lambda x (lambda y y))) t) f)" `shouldBe` "f"

            describe "OR" $ do
                it "true or true = true" $ do
                    evalStr "((((lambda false ((lambda true ((lambda or ((or true) true)) (lambda b (lambda c ((b true) c))))) (lambda x (lambda y x)))) (lambda x (lambda y y))) t) f)" `shouldBe` "t"
            
                it "false or true = true" $ do
                    evalStr "((((lambda false ((lambda true ((lambda or ((or false) true)) (lambda b (lambda c ((b true) c))))) (lambda x (lambda y x)))) (lambda x (lambda y y))) t) f)" `shouldBe` "t"

                it "true or false = true" $ do
                    evalStr "((((lambda false ((lambda true ((lambda or ((or true) false)) (lambda b (lambda c ((b true) c))))) (lambda x (lambda y x)))) (lambda x (lambda y y))) t) f)" `shouldBe` "t"

                it "false or false = false" $ do
                    evalStr "((((lambda false ((lambda true ((lambda or ((or false) false)) (lambda b (lambda c ((b true) c))))) (lambda x (lambda y x)))) (lambda x (lambda y y))) t) f)" `shouldBe` "f"

            describe "NOT" $ do
                it "not true = false" $ do
                    evalStr "((((lambda false ((lambda true ((lambda not (not true)) (lambda b (lambda t (lambda f ((b f) t)))))) (lambda x (lambda y x)))) (lambda x (lambda y y))) t) f)" `shouldBe` "f"

                it "not false = true" $ do
                    evalStr "((((lambda false ((lambda true ((lambda not (not false)) (lambda b (lambda t (lambda f ((b f) t)))))) (lambda x (lambda y x)))) (lambda x (lambda y y))) t) f)" `shouldBe` "t"

        describe "Church numerals (Robinson arithmetic)" $ do
            describe "equality" $ do
                it "is reflexive" $ do
                    pending

                it "is symmetric" $ do
                    pending

                it "is transitive" $ do
                    pending

            it "zero is not the successor of any number" $ do
                pending

            it "if succ x = succ y then x = y" $ do
                pending

            it "every number is either zero or a successor" $ do
                pendingWith "not formulatable with quickcheck"

            it "x + 0 = x" $ do
                pending

            it "x + succ y = succ (x + y)" $ do
                pending

            it "x * 0 = 0" $ do
                pending

            it "x * succ y = x * y + x" $ do
                pending
