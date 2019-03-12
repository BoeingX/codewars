module RecoverSecretFromTripletsSpec where

import RecoverSecretFromTriplets (recoverSecret)
import Test.Hspec

secret = "whatisup"
triplets = ["tup"
           ,"whi"
           ,"tsu"
           ,"ats"
           ,"hap"
           ,"tis"
           ,"whs"
           ]

spec :: Spec
spec = do
    it "Example" $ do
        recoverSecret triplets `shouldBe` secret
