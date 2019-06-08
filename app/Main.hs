module Main where

import GHC.Integer.GMP.Internals
import Data.Array (array, Array, (!))

mo :: Integer
mo = 998244353

height'' :: Integer -> Integer -> Integer
height'' 0 _ = 1
height'' _ 0 = 1
height'' n m
    | n > m = height'' m m
    | otherwise = go 1 1 1
        where go k x s
                | k > n = s
                | otherwise = go (k+1) x'' s'
                    where x' = ((m - k + 1) *  k') * x
                          x'' = x' `mod` mo
                          s' = (s + x'') `mod` mo
                          k' = recipModInteger k mo

height :: Integer -> Integer -> Integer
height n m = (height'' n (m `mod` mo) - 1) `mod` mo

main :: IO ()
main = do
    print $ height 80000 100000
