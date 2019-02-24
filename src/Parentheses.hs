{- See https://www.codewars.com/kata/52774a314c2333f0a7000688/train/haskell
 
Write a function called that takes a string of parentheses, and determines if the order of the parentheses is valid. The function should return true if the string is valid, and false if it's invalid.
Examples

"()"              =>  true
")(()))"          =>  false
"("               =>  false
"(())((()())())"  =>  true

Constraints

0 <= input.length <= 100

Along with opening (() and closing ()) parenthesis, input may contain any valid ASCII characters. Furthermore, the input string may be empty and/or not contain any parentheses at all. Do not treat other forms of brackets as parentheses (e.g. [], {}, <>).
-}
{-# OPTIONS_GHC -Wall #-}
module Parentheses where

validParentheses :: String -> Bool
validParentheses = go 0 . filter (`elem` "()")
    where go n [] = n == 0
          go n (x:xs)
            | n < 0 = False
            | otherwise = case x of
                '(' -> go (n + 1) xs
                _   -> go (n - 1) xs
