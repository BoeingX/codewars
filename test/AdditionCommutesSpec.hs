module AdditionCommutesSpec where

import AdditionCommutes
import Test.Hspec
import Test.QuickCheck

zero = NumZ
one = NumS NumZ
two = NumS one
three = NumS two
four = NumS three
five = NumS four

hundred = NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS NumZ

twoHundreds = NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS $ NumS hundred

spec :: Spec
spec = do
  describe "My own tests" $ do
    it "one plus two" $ plusCommutes one two `shouldBe` reflexive three
    it "two plus two" $ plusCommutes two two `shouldBe` reflexive four
    it "big plus big" $ plusCommutes hundred hundred `shouldBe` reflexive twoHundreds
