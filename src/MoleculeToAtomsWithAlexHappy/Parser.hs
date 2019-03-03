module MoleculeToAtomsWithAlexHappy.Parser where

import MoleculeToAtomsWithAlexHappy.Lexer
import MoleculeToAtomsWithAlexHappy.Grammar

import qualified Data.Map as M

type Composition = M.Map String Int

mul :: Index -> Composition -> Composition
mul One = id
mul (Mul n) = M.map (n *)

merge :: Composition -> Composition -> Composition
merge = M.unionWith (+)

eval :: Molecule -> Composition
eval (Singleton s i) = mul i (M.fromList [(s, 1)])
eval (Multiply m i) = mul i (eval m)
eval (Compound m1 m2) = merge (eval m1) (eval m2)

parseMolecule :: String -> Either String [(String,Int)]
parseMolecule formula = M.toList . eval <$> parse (alexScanTokens formula)
