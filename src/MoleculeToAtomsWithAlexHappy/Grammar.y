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

Molecule : M                 { Molecule $1 }
         | Molecule Molecule { Compound $1 $2 }

M : atom Index              { Singleton $1 $2 }
  | '(' Molecule ')' Index  { Multiply $2 $4 }
  | '[' Molecule ']' Index  { Multiply $2 $4 }
  | '{' Molecule '}' Index  { Multiply $2 $4 }

Index : {- empty -} { One }
      | index       { Mul $1 } 

{
type Atom = String

data Molecule = Molecule M 
              | Compound Molecule Molecule
    deriving (Eq, Show)

data M = Singleton Atom Index
       | Multiply Molecule Index
    deriving (Eq, Show)

data Index = One | Mul Int
    deriving (Eq, Show)

parseError :: [Token] -> Either String a
parseError _ = Left "Not a valid molecule"
}
