module MiddlePermutationSpec where

import MiddlePermutation (middlePermutation)
import Test.Hspec
import System.Random
import Data.List

spec :: Spec
spec = do
   describe "Sample test" $ do
     it "abc" $ do
      middlePermutation "abc" `shouldBe` "bac"
     it "abcd" $ do
       middlePermutation "abcd" `shouldBe` "bdca"
     it "abcdx" $ do
      middlePermutation "abcdx" `shouldBe` "cbxda"
     it "abcdxg" $ do
       middlePermutation "abcdxg" `shouldBe` "cxgdba"
     it "abcdxgz" $ do
       middlePermutation "abcdxgz" `shouldBe` "dczxgba"
