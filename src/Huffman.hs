-- | see https://www.codewars.com/kata/54cf7f926b85dcc4e2000d9d/train/haskell
module Huffman where

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

data Bit = Z | O deriving (Eq, Show)

data Tree a = Node Int (Tree a) (Tree a) | Leaf Int a
        deriving (Eq, Show)

getFrequency :: Tree a -> Int
getFrequency (Node n _ _) = n
getFrequency (Leaf n _ )  = n

instance (Ord a) => Ord (Tree a) where
    t1 <= t2 = (getFrequency t1) <= (getFrequency t2)

singleton :: (a, Int) -> Tree a
singleton (a, n) = Leaf n a

mergeTrees :: [Tree a] -> Maybe (Tree a)
mergeTrees [] = Nothing
mergeTrees [t] = Just t
mergeTrees (t1:t2:ts) = mergeTrees (sortOn getFrequency (t':ts))
    where t' = Node (getFrequency t1 + getFrequency t2) t1 t2

fromFrequencies :: [(a, Int)] -> Maybe (Tree a)
-- | Frequency lists with just one or less elements should get rejected.
-- (Because then there is no information we could encode, but the length.)
fromFrequencies [ ]  = Nothing
fromFrequencies [_] = Nothing
fromFrequencies xs = (mergeTrees . sortOn getFrequency . map singleton) xs

toEncodeMap :: (Ord a) => Tree a -> M.Map a [Bit]
toEncodeMap (Leaf _ a) = M.singleton a []
toEncodeMap (Node _ t1 t2) = M.union mp1 mp2
    where mp1 = M.map ((:) Z) $ toEncodeMap t1
          mp2 = M.map ((:) O) $ toEncodeMap t2

-- | Calculate symbol frequencies of a text.
frequencies :: Ord a => [a] -> [(a, Int)]
frequencies = M.toList . foldr go M.empty
        where go c = M.insertWith (+) c 1

-- | Encode a sequence using the given frequencies.
encode :: Ord a => [(a, Int)] -> [a] -> Maybe [Bit]
encode freqs xs = let mp = toEncodeMap <$> fromFrequencies freqs in case mp of
    Nothing -> Nothing
    Just m  -> Just (concatMap ((M.!) m) xs)

walkTree :: Tree a -> Tree a -> [Bit] -> [a]
walkTree root tree bs = reverse $ go root tree bs []
    where go root (Leaf _ c) bs xs = go root root bs (c:xs)
          go _ _ [] xs = xs
          go root (Node _ t1 t2) (Z:bs) xs = go root t1 bs xs 
          go root (Node _ t1 t2) (O:bs) xs = go root t2 bs xs 

-- | Decode a bit sequence using the given frequencies.
decode :: [(a, Int)] -> [Bit] -> Maybe [a]
decode freqs bs = let tree = fromFrequencies freqs in case tree of
    Nothing -> Nothing
    Just root -> Just $ walkTree root root bs
