{
module MoleculeToAtomsWithAlexHappy.Lexer where
}

%wrapper "basic"

tokens :-
    $white+         ;
    [A-Z][a-z]?     {\s -> TAtom s}
    [0-9]+          {\s -> TIndex (read s)}
    \(              {\s -> TLParen}
    \)              {\s -> TRParen}
    \[              {\s -> TLBracket}
    \]              {\s -> TRBracket}
    \{              {\s -> TLBrace}
    \}              {\s -> TRBrace}

{
data Token = TAtom String
           | TIndex Int
           | TLParen
           | TRParen
           | TLBracket
           | TRBracket
           | TLBrace
           | TRBrace
    deriving (Eq, Show)
}
