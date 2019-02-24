module SmileSpec where

import Test.Hspec
import Smile

spec :: Spec
spec = do
  it "Handles basic tests" $ do
    countSmileys []                               `shouldBe` 0
    countSmileys [":D",":~)",";~D",":)"]          `shouldBe` 4
    countSmileys [":)",":(",":D",":O",":;"]       `shouldBe` 2
    countSmileys [";]", ":[", ";*", ":$", ";-D"]  `shouldBe` 1
    countSmileys [";", ")", ";*", ":$", "8-D"]    `shouldBe` 0
    countSmileys ["",":)~","","",";)","V\818473;","\768000y",")-:","Y",")~:",":D",":)","\1106028P","","",":~D",")-:",":D",":D",":)","-;D",";)",";D-",":~D","\208349}\149617",":D~",":-)",";~)",";)",":)-",":~)","",";D",":)-",";-)",":D",";)","D;~","D:~",";)",":)","\RS\724504",":~)",":D","\233643\DC2",";)","",":-)"]      `shouldBe` 23
