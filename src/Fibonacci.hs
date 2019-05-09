-- See https://www.codewars.com/kata/the-millionth-fibonacci-kata/haskell
module Fibonacci where

newtype Matrix a = Matrix (a, a, a, a)

-- We only implement the multiplication
instance (Num a) => Num (Matrix a) where
    Matrix (a11, a12, a21, a22) * Matrix (a11', a12', a21', a22') = Matrix (a11'', a12'', a21'', a22'')
        where a11'' = a11 * a11' + a12 * a21'
              a12'' = a11 * a12' + a12 * a22'
              a21'' = a21 * a11' + a22 * a21'
              a22'' = a21 * a12' + a22 * a22'

toFib :: Matrix Integer -> Integer
toFib (Matrix (a, _, _, _)) = a

e :: Matrix Integer
e = Matrix (0, 1, 1, 1)

fib :: Integer -> Integer
fib n
    | n >= 0     = toFib $ e ^ (n + 1)
    | otherwise = fib (-n) * k
    where k = if n `mod` 2 == 0 then -1 else 1
