{
module MoleculeToAtomsWithAlexHappy.Grammar where

import MoleculeToAtomsWithAlexHappy.Lexer (Token(..))
}

%name parse
%tokentype { Token }
%error { parseError }
%monad { Either String }
%token
    index           { TIndex $$ } 
    atom            { TAtom $$ }
    '('             { TLParen }
    ')'             { TRParen }
    '['             { TLBracket }
    ']'             { TRBracket }
    '{'             { TLBrace }
    '}'             { TRBrace }
%%

Molecule : atom Index               { Singleton $1 $2 }
         | Molecule Molecule        { Compound $1 $2 }
         | '(' Molecule ')' Index { Multiply $2 $4 } 
         | '(' Molecule ')' Index Molecule { Compound (Multiply $2 $4) $5 } 
         | '[' Molecule ']' Index { Multiply $2 $4 }
         | '[' Molecule ']' Index Molecule { Compound (Multiply $2 $4) $5 } 
         | '{' Molecule '}' Index { Multiply $2 $4 } 
         | '{' Molecule '}' Index Molecule { Compound (Multiply $2 $4) $5 } 

Index : {- empty -} { One }
      | index       { Mul $1 } 

{
data Index = One | Mul Int
    deriving (Eq, Show)

data Molecule = Singleton String Index
              | Multiply Molecule Index
              | Compound Molecule Molecule

parseError :: [Token] -> Either String a
parseError _ = Left "Not a valid molecule"
}
