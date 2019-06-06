{-# LANGUAGE NoImplicitPrelude, GADTs , DataKinds, TypeFamilies, TypeOperators, RankNTypes, DeriveFunctor #-}
{-- See https://www.codewars.com/kata/singletons/train/haskell --}
module Singletons where

import Prelude hiding (drop, take, head, tail, index, zipWith, replicate, map, (++))

data Vec a n where
  VNil :: Vec a Zero
  VCons :: a -> Vec a n -> Vec a (Succ n)

-- promoted to type level by data kinds
data Nat = Zero | Succ Nat

data SNat a where
  SZero :: SNat Zero
  SSucc :: SNat a -> SNat (Succ a)

type family (a :: Nat) :< (b :: Nat) :: Bool
type instance m :< Zero = False
type instance Zero :< Succ n = True
type instance (Succ m) :< (Succ n) = m :< n

type family (Add (a :: Nat) (b :: Nat)) :: Nat
type instance Add Zero m = m
type instance Add (Succ n) m = Succ (Add n m)

type family (Min (a :: Nat) (b :: Nat)) :: Nat
type instance Min m Zero = Zero
type instance Min Zero m = Zero
type instance Min (Succ n) (Succ m) = Succ (Min n m)

type family (Minus (a :: Nat) (b :: Nat)) :: Nat
type instance Minus m Zero = m
type instance Minus (Succ n) (Succ m) = Minus n m

map :: (a -> b) -> Vec a n -> Vec b n
map f VNil = VNil
map f (VCons x xs) = VCons (f x) (map f xs)

index :: ((a :< b) ~ True) => SNat a -> Vec s b -> s
index SZero (VCons x xs) = x
index (SSucc n) (VCons x xs) = index n xs

replicate :: s -> SNat a -> Vec s a
replicate _ SZero = VNil
replicate s (SSucc n) = VCons s (replicate s n)

-- Both vectors must be of equal length
zipWith :: (a -> b -> c) -> Vec a n -> Vec b n -> Vec c n
zipWith _ VNil VNil = VNil
zipWith f (VCons x xs) (VCons y ys) = VCons (f x y) (zipWith f xs ys)

(++) :: Vec v m -> Vec v n -> Vec v (Add m n)
VNil ++ ys = ys
(VCons x xs) ++ ys = VCons x (xs ++ ys)

-- The semantics should match that of take for normal lists.
take :: SNat a -> Vec s b -> Vec s (Min a b)
take SZero _ = VNil
take _ VNil = VNil
take (SSucc a) (VCons x xs) = VCons x (take a xs)

-- The semantics should match that of drop for normal lists.
drop :: SNat a -> Vec s b -> Vec s (Minus b (Min a b))
drop SZero v = v
drop _ VNil = VNil
drop (SSucc a) (VCons x xs) = drop a xs

head :: Vec s (Succ a) -> s
head (VCons x xs) = x

tail :: Vec s (Succ a) -> Vec s a
tail (VCons x xs) = xs
