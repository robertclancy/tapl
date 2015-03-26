module ParserSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Test.QuickCheck.Property as QP
import Control.Monad
import Language.Inference.Syntax
import Language.Inference.Parser
import Language.Inference.Pretty
import Text.PrettyPrint (render)
import Arbitrary

spec :: Spec
spec = do
        describe "printing" $ do
            prop "should be a right inverse of parsing" $
                \x -> let printed = showTerm x in
                    either (\x -> QP.property $ QP.failed { QP.reason = show x ++ "\n" ++ printed }) (\y -> x === y) $ readTerm printed
