{- See: https://www.codewars.com/kata/simple-fun-number-159-middle-permutation/haskell
Task

You are given a string s. Every letter in s appears once.

Consider all strings formed by rearranging the letters in s. After ordering these strings in dictionary order, return the middle term. (If the sequence has a even length n, define its middle term to be the (n/2)th term.)
Example

For s = "abc", the result should be "bac". The permutations in order are: "abc", "acb", "bac", "bca", "cab", "cba" So, The middle term is "bac".
Input/Output

    [input] string s

    unique letters (2 <= length <= 26)

    [output] a string

    middle permutation.

-}
module MiddlePermutation where

import Data.List (sort, delete)

factorial :: Int -> Int
factorial n = product [1..n]

split :: String -> (String, String)
split s = go s ""
    where go x y
            | null x = (x, y)
            | null y = go (init x) [last x]
            | last x >= head y = go (init x) (last x : y)
            | otherwise = (x, y)

nextPermutation :: String -> String
nextPermutation s
    | null l = sort s
    | otherwise = let m = minimum $ filter (> last l) r
                    in (init l) ++ [m] ++ sort (last l : delete m r)
    where (l, r) = split s

middlePermutation :: String -> String
middlePermutation s = (iterate nextPermutation $ sort s) !! idx
    where idx = (factorial (length s)) `div` 2 - 1
