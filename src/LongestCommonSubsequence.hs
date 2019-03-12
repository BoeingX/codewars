{- See https://www.codewars.com/kata/52756e5ad454534f220001ef/train/haskell
 
Write a function called LCS that accepts two sequences and returns the longest subsequence common to the passed in sequences.
Subsequence

A subsequence is different from a substring. The terms of a subsequence need not be consecutive terms of the original sequence.
Example subsequence

Subsequences of "abc" = "a", "b", "c", "ab", "ac", "bc" and "abc".
LCS examples

lcs "a"         "b"         `shouldBe` ""
lcs "abcdef"    "abc"       `shouldBe` "abc"
lcs "132535365" "123456789" `shouldBe` "12356"

Notes

    Both arguments will be strings
    Return value must be a string
    Return an empty string if there exists no common subsequence
    Both arguments will have one or more characters (in JavaScript)
    All tests will only have a single longest common subsequence. Don't worry about cases such as LCS( "1234", "3412" ), which would have two possible longest common subsequences: "12" and "34".

Note that the Haskell variant will use randomized testing, but any longest common subsequence will be valid.

Note that the OCaml variant is using generic lists instead of strings, and will also have randomized tests (any longest common subsequence will be valid).
-}
{-# OPTIONS_GHC -Wall #-}

module LongestCommonSubsequence where

import Data.List (maximumBy)
import Data.Array

comp :: String -> String -> Ordering
comp x y = compare (length x) (length y)

-- dynamic programming
lcs :: String -> String -> String
lcs x y = reverse $ v ! (m, n)
    where v = listArray ((0, 0), (m, n)) (map go [(i, j) | i <- [0..m], j <- [0..n]])
          m = length x
          n = length y
          w1 = listArray (0, m) x
          w2 = listArray (0, n) y
          go (_, 0) = ""
          go (0, _) = ""
          go (i, j) = maximumBy comp [v1, v2, v3]
            where v1 = if w1 ! (i - 1) == w2 ! (j - 1)
                        then w1 ! (i - 1) : v ! (i - 1, j - 1)
                        else v ! (i - 1, j - 1)
                  v2 = v ! (i - 1, j)
                  v3 = v ! (i, j - 1)
