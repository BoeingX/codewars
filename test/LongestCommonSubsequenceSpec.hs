module LongestCommonSubsequenceSpec where

import LongestCommonSubsequence (lcs)
import Test.Hspec

spec :: Spec
spec = do
  describe "lcs" $ do
    it "should work on some examples" $ do
      lcs "a"         "b"         `shouldBe` ""
      lcs "abcdef"    "abc"       `shouldBe` "abc"
      lcs "132535365" "123456789" `shouldBe` "12356"
