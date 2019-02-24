{- See https://www.codewars.com/kata/molecule-to-atoms/train/haskell
 
For a given chemical formula represented by a string, count the number of atoms of each element contained in the molecule and return an object (associative array in PHP, Dictionary<string, int> in C#, Map in Java).

For example:

>>> parseMolecule "H2O" -- water
Right [("H",2),("O",1)]

>>> parseMolecule "Mg(OH)2" -- magnesium hydroxide
Right [("Mg",1),("O",2),("H",2)]

>>> parseMolecule "K4[ON(SO3)2]2" -- Fremy's salt
Right [("K",4),("O",14),("N",2),("S",4)]

>>> parseMolecule "pie"
Left "Not a valid molecule"

As you can see, some formulas have brackets in them. The index outside the brackets tells you that you have to multiply count of each atom inside the bracket on this index. For example, in Fe(NO3)2 you have one iron atom, two nitrogen atoms and six oxygen atoms.

Note that brackets may be round, square or curly and can also be nested. Index after the braces is optional.
-}
{-# OPTIONS_GHC -Wall #-}
module MoleculeToAtoms where

import Text.ParserCombinators.ReadP
import qualified Data.Map as M

type Composition  = M.Map String Int
type OpenBracket  = Char
type CloseBracket = Char

-- | Atom symbols
-- See https://en.wikipedia.org/wiki/Symbol_(chemistry)
atoms :: [String]
atoms = ["H","He","Li","Be","B","C","N","O","F","Ne","Na","Mg","Al","Si","P","S","Cl","Ar","K","Ca","Sc","Ti","V","Cr","Mn","Fe","Co","Ni","Cu","Zn","Ga","Ge","As","Se","Br","Kr","Rb","Sr","Y","Zr","Nb","Mo","Tc","Ru","Rh","Pd","Ag","Cd","In","Sn","Sb","Te","I","Xe","Cs","Ba","La","Ce","Pr","Nd","Pm","Sm","Eu","Gd","Tb","Dy","Ho","Er","Tm","Yb","Lu","Hf","Ta","W","Re","Os","Ir","Pt","Au","Hg","Tl","Pb","Bi","Po","At","Rn","Fr","Ra","Ac","Th","Pa","U","Np","Pu","Am","Cm","Bk","Cf","Es","Fm","Md","No","Lr","Rf","Db","Sg","Bh","Hs","Mt","Ds","Rg","Cn","Nh","Fl","Mc","Lv","Ts","Og"]

-- | Multiply the atom composition by an index
mul :: Int -> Composition -> Composition
mul index = M.map ((*) index)

-- | Merge two atom compositions by summation
merge :: [Composition] -> Composition
merge = foldr f M.empty
    where f = M.unionWith (+)

-- | Parse a decimal number
parseNumber :: ReadP Int
parseNumber = read <$> many1 (choice (map char "0123456789"))

-- | Parse a single atom with an optional index
parseAtom :: ReadP Composition
parseAtom = do
    x <- choice $ map string atoms
    index <- option 1 parseNumber
    return $ mul index (M.fromList [(x, 1)])

-- | Parse molecule between an open and close bracket
parseBracket :: OpenBracket -> CloseBracket -> ReadP Composition
parseBracket open close = do
    subMolecule <- between (char open) (char close) parse
    index <- option 1 parseNumber
    return $ mul index subMolecule

-- | Parse a sub molecule
parseSubMolecule :: ReadP Composition
parseSubMolecule = choice $ map (uncurry parseBracket) [('(', ')'), ('[', ']'), ('{', '}')]

-- | Parse a molecule to atom composition
parse :: ReadP Composition
parse = merge <$> many1 (parseAtom +++ parseSubMolecule)

-- | Main routine with error handling
parseMolecule :: String -> Either String [(String,Int)]
parseMolecule formula = case readP_to_S (parse <* eof) formula of
    [] -> Left "Not a valid molecule"
    [(x, "")] -> Right (M.toList x)
    _ -> undefined
