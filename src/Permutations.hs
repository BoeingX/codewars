module Permutations where

import Data.List (sort, delete)

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

permutations :: String -> [String]
permutations s = s : (takeWhile (/= s) $ tail $ iterate nextPermutation s)
