module Faberge2 where

import Data.Array (listArray, Array, (!))

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
                | otherwise = go (k+1) x' s'
                    where x' = (m - k + 1) * (arr ! k)  * x `mod` mo
                          s' = (s + x') `mod` mo
              arr :: Array Integer Integer
              arr = listArray (1, 80000) $ map go [1..80000]
                where go 1 = 1
                      go k = -(arr ! m) * d `mod` mo
                        where (d, m) = mo `divMod` k

height :: Integer -> Integer -> Integer
height n m = (height'' n (m `mod` mo) - 1 ) `mod` mo
