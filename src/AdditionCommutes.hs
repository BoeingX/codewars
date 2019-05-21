{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}

module AdditionCommutes where

data Z
data S n

-- | Predicate describing natural numbers.
-- | This allows us to reason with `Nat`s.
data Natural :: * -> * where
    NumZ :: Natural Z
    NumS :: Natural n -> Natural (S n)

instance Show (Natural x) where
    show NumZ = "Z"
    show (NumS n) = "S (" ++ show n  ++ ")"

-- | Predicate describing equality of natural numbers.
data Equal :: * -> * -> * where
    EqlZ :: Equal Z Z
    EqlS :: Equal n m -> Equal (S n) (S m)

show' :: Equal a b -> (String, String)
show' EqlZ = ("Z", "Z")
show' (EqlS eq) = ("S (" ++ s1 ++ ")", "S (" ++ s2 ++ ")")
    where (s1, s2) = show' eq

instance Show (Equal x y) where
    show x = "Equal (" ++ s1 ++ ") (" ++ s2 ++ ")"
        where (s1, s2) = show' x

instance Eq (Equal x y) where
    EqlZ == EqlZ = True
    EqlS x == EqlS y = x == y
    _ == _ = False

-- | Peano definition of addition.
type family (:+:) (n :: *) (m :: *) :: *
type instance Z :+: m = m
type instance S n :+: m = S (n :+: m)

-- These are some lemmas that may be helpful.
-- They will *not* be tested, so rename them
-- if you so desire. Good luck!

-- | For any n, n = n.
reflexive :: Natural n -> Equal n n
reflexive NumZ = EqlZ
reflexive (NumS n) = EqlS (reflexive n)

-- | if a = b, then b = a.
symmetric :: Equal a b -> Equal b a
symmetric EqlZ = EqlZ
symmetric (EqlS x) = EqlS (symmetric x)

-- | if a = b and b = c, then a = c.
transitive :: Equal a b -> Equal b c -> Equal a c
transitive EqlZ EqlZ = EqlZ
transitive (EqlS x) (EqlS y) = EqlS (transitive x y)

zeroCommutes :: Natural a -> Equal (Z :+: a) (a :+: Z)
zeroCommutes NumZ = EqlZ
zeroCommutes (NumS n) = EqlS (zeroCommutes n)

addToSecond :: Natural a -> Natural b -> Equal (S (a :+: b)) (a :+: S b)
addToSecond NumZ m = reflexive (NumS m)
addToSecond (NumS n) m = EqlS (addToSecond n m)

-- This is the proof that the kata requires.
-- | a + b = b + a
plusCommutes :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusCommutes NumZ m = zeroCommutes m
plusCommutes (NumS n) m = transitive (EqlS (plusCommutes n m)) (addToSecond m n)
