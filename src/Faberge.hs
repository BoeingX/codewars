module Faberge where

import Data.List

-- height n m = \sum_{k=0}^n C_m^k
-- one can find this by using generating functions
height :: Integer -> Integer -> Integer
height 0 _ = 1
height _ 0 = 1
height n m
    | n >= m = 2 ^ m
    | otherwise = sum $ foldl' go [] [0..n]
        where go []     _ = [1]
              go (x:xs) k = ((m - k + 1) * x `div` k) : x : xs

heigth :: Integer -> Integer -> Integer 
heigth n m = height n m - 1
