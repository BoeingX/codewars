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
    singleton       { TAtom $$ }
    '('             { TLParen }
    ')'             { TRParen }
    '['             { TLBracket }
    ']'             { TRBracket }
    '{'             { TLBrace }
    '}'             { TRBrace }
%%

Molecule : {- empty -} { Nil }
         | singleton Index Molecule { Compound (Singleton $1 $2) $3 }
         | '(' Molecule ')' Index Molecule { Compound (Simple $2 $4) $5}
         | '[' Molecule ']' Index Molecule { Compound (Simple $2 $4) $5}
         | '{' Molecule '}' Index Molecule { Compound (Simple $2 $4) $5}

Index : {- empty -} { One }
      | index {Mul $1 }

{
parseError :: [Token] -> Either String a
parseError _ = Left "Not a valid molecule"

data Index = One | Mul Int deriving (Eq, Show)

data Molecule = Singleton String Index
              | Simple Molecule Index
              | Compound Molecule Molecule
              | Nil
              deriving (Eq, Show)
}
