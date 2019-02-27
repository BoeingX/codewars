module MoleculeToAtomsWithAlexHappySpec where

import MoleculeToAtomsWithAlexHappy.Parser (parseMolecule)
import Data.List (sort)
import Test.Hspec
import Test.HUnit

spec :: Spec
spec = do
  describe "Molecules" $ do
    assertParse "H" [("H",1)] "hydrogen"
    assertParse "O2" [("O",2)] "oxygen"
    assertParse "H2O" [("H",2),("O",1)] "water"
    assertParse "Mg(OH)2" [("Mg",1),("O",2),("H",2)] "magnesium hydroxide"
    assertParse "K4[ON(SO3)2]2" [("K",4),("O",14),("N",2),("S",4)] "Fremy's salt"
    assertParse "(C5H5)Fe(CO)2CH3" [("C",8),("Fe",1),("H",8),("O",2)] "cyclopentadienyliron dicarbonyl dimer"
  
  describe "Errors" $ do
    assertFail "pie" "Not a valid molecule"
    assertFail "Mg(OH" "Mismatched parenthesis"
    assertFail "Mg(OH}2" "Mismatched parenthesis"
  
assertParse formula expected name = it (name ++ ": " ++ formula) $
  assertEqual "" (Right $ sort expected) (fmap sort $ parseMolecule formula)
  
assertFail formula name = it (name ++ ": " ++ formula) $
  parseMolecule formula `shouldSatisfy` isLeft
  
isLeft (Left _) = True
isLeft _ = False
