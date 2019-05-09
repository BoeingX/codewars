module Fibonacci where

import Data.List

-- For fib n, n >= 0
-- fibs = [0, 1, 1, 2, 3, 5, 8, ...]
fibs :: [Integer]
fibs = scanl' (+) 0 (1:fibs)

fib :: Integer -> Integer
fib n
    | n >= 0    = fibs !! fromInteger n
    | otherwise = (fibs !! fromInteger (-n)) * k
    where k = if n `mod` 2 == 0 then -1 else 1
