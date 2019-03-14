module NIMSpec where

import qualified NIM
import Test.Hspec

spec :: Spec
spec = do
    describe "sanity check" $ do
        it "[0,5,0,0]" $ NIM.chooseMove [0,5,0,0] `shouldBe` (1,5)
        it "[0,3,5]" $ NIM.chooseMove [0,3,5] `shouldBe` (2,2)
        it "[3,3,6]" $ NIM.chooseMove [3,3,6] `shouldBe` (2,6)
