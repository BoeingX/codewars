{- See: https://www.codewars.com/kata/554f76dca89983cc400000bb/train/haskell

In mathematics, a Diophantine equation is a polynomial equation, usually with two or more unknowns, such that only the integer solutions are sought or studied.

In this kata we want to find all integers x, y (x >= 0, y >= 0) solutions of a diophantine equation of the form:
x2 - 4 * y2 = n

(where the unknowns are x and y, and n is a given positive number) in decreasing order of the positive xi.

If there is no solution return [] or "[]" or "". (See "RUN SAMPLE TESTS" for examples of returns).
Examples:

solEquaStr(90005) --> "[[45003, 22501], [9003, 4499], [981, 467], [309, 37]]"
solEquaStr(90002) --> "[]"

Hint:

x2 - 4 * y2 = (x - 2*y) * (x + 2*y)
-}
module Dioph where

import Data.Maybe

isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

allFactors :: Integer -> [(Integer, Integer)]
allFactors n = [(i, n `div` i) | i <- [1..isqrt n], n `mod` i == 0]

solequa :: Integer -> [(Integer, Integer)]
solequa n = map fromJust $ filter (not . null) $ map solve (allFactors n)
    where solve (s, t)
            | r1 == 0 && r2 == 0 && x >= 0 && y >= 0 = Just (x, y)
            | otherwise = Nothing
            where (x, r1) = (s + t) `divMod` 2
                  (y, r2) = (t - s) `divMod` 4
