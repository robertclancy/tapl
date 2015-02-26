module ParserSpec (spec) where

import Test.Hspec
import SimplyTyped.Terms

spec :: Spec
spec = do
        describe "example" $ do
            it "should be true" $ do
                True `shouldBe` True
