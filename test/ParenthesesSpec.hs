module ParenthesesSpec where

import Parentheses (validParentheses)
import Test.Hspec

spec :: Spec
spec = do
  describe "validParentheses" $ do
    it "should work for some examples" $ do
      validParentheses "()" `shouldBe` True
      validParentheses ")(" `shouldBe` False
      validParentheses ")"  `shouldBe` False
      validParentheses "(())((()())())"  `shouldBe` True
