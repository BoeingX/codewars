{
module MoleculeToAtomsWithAlexHappy.Grammar where

import MoleculeToAtomsWithAlexHappy.Lexer (Token(..))
}

%name parse
%tokentype { Token }
%error { parseError }
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

Molecule : {- empty -} { Nil }
         | atom Index Molecule { Compound (Atom $1 $2) $3 }
         | '(' Molecule ')' Index Molecule { Compound (Simple $2 $4) $5}
         | '[' Molecule ']' Index Molecule { Compound (Simple $2 $4) $5}
         | '{' Molecule '}' Index Molecule { Compound (Simple $2 $4) $5}

Index : {- empty -} { One }
      | index {Mul $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Index = One | Mul Int deriving (Eq, Show)

data Molecule = Atom String Index
              | Simple Molecule Index
              | Compound Molecule Molecule
              | Nil
              deriving (Eq, Show)
}
