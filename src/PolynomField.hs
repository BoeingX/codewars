{- See https://www.codewars.com/kata/54f1b7b3f58ba8ee720005a8/train/haskell
-}
module PolynomField where

import Data.Bits
import Data.Int
import Data.List

newtype BinaryPolynom = BinaryPolynom {getInt :: Int}
    deriving (Eq, Ord)

zero, one :: BinaryPolynom
zero = BinaryPolynom 0
one  = BinaryPolynom 1

deg :: BinaryPolynom -> Int
deg (BinaryPolynom 0) = -1
deg (BinaryPolynom n) = go 16
    where go k = case bit k .&. n of
                    0 -> go (k - 1)
                    _ -> k

-- | Constructs a monom with the given degree.
polyFromDeg :: Int -> BinaryPolynom
polyFromDeg = BinaryPolynom . bit

polyFromPowers :: [Int] -> BinaryPolynom
polyFromPowers = foldr go zero
    where go k (BinaryPolynom n) = BinaryPolynom (setBit n k)

instance Show BinaryPolynom where
    show (BinaryPolynom 0) = "0"
    show (BinaryPolynom n) = intercalate " + " $ filter (not . null) $ map go [16, 15 .. 0]
        where go k
                | testBit n k = if k == 0 then "1" else "x^" ++ show k
                | otherwise   = ""

-- | Multiplication in the polynom ring.
multiply :: BinaryPolynom -> BinaryPolynom -> BinaryPolynom
multiply x y = foldr ((.+.) . go ) zero [16, 15 .. 0]
    where go k
            | testBit (getInt x) k = BinaryPolynom (shift (getInt y) k)
            | otherwise            = zero

m :: BinaryPolynom
m = polyFromPowers [8, 4, 3, 1, 0]

-- | Addition and multiplication in the polynom field.
(.+.), (.*.) :: BinaryPolynom -> BinaryPolynom -> BinaryPolynom
x .+. y = BinaryPolynom (getInt x `xor` getInt y)
x .*. y = snd $ polyDivMod (multiply x y) m

polyDivMod :: BinaryPolynom -> BinaryPolynom -> (BinaryPolynom, BinaryPolynom)
polyDivMod x y
    | deg x < deg y = (zero, x)
    | otherwise = (d .+. ds, r)
    where d = polyFromDeg (deg x - deg y)
          (ds, r) = polyDivMod (multiply y d .+. x) y
