module DiophSpec where

import Dioph
import Test.Hspec
import Test.HUnit

spec :: Spec
spec = do
    describe "playDigits" $ do
        it "1st series" $ do
            solequa 5 `shouldBe` [(3, 1)]
            solequa 12 `shouldBe` [(4, 1)]
            solequa 13 `shouldBe` [(7, 3)]
            solequa 16 `shouldBe` [(4, 0)]
            solequa 17 `shouldBe` [(9, 4)]
