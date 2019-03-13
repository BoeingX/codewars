module ZerosSpec where

import Zeros
import Test.Hspec

spec :: Spec
spec = do
  describe "zeros" $ do 
    it "0! has 0 trailing zeros" $ do
      zeros 0 `shouldBe` 0
    it "6! has 1 trailing zeros" $ do
      zeros 6 `shouldBe` 1      
    it "30! has 7 trailing zeros" $ do
      zeros 30 `shouldBe` 7
    it "100! has 24 trailing zeros" $ do
      zeros 100 `shouldBe` 24      
    it "1000! has 249 trailing zeros" $ do
      zeros 1000 `shouldBe` 249
    it "100000! has 24999 trailing zeros" $ do
      zeros 100000 `shouldBe` 24999
    it "1000000000! has 249999998 trailing zeros" $ do
      zeros 1000000000 `shouldBe` 249999998
