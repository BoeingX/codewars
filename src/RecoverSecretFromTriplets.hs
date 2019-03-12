module RecoverSecretFromTriplets where

import Data.List

type Node = Char
type Edge = (Char, Char)
type Graph = ([Node], [Edge])

mkGraph :: [String] -> Graph
mkGraph xs = (ns, es)
    where es = concatMap (\[x,y,z] -> [(x,y), (y,z), (x,z)]) xs
          ns = nub $ concat xs

inDegree :: Graph -> Node -> Int
inDegree (_, es) n = length $ filter (\(f, t) -> t == n) es

removeNode :: Graph -> Node -> Graph
removeNode (ns, es) n = (ns', es')
    where ns' = delete n ns
          es' = filter (\(f, t) -> f /= n && t /= n) es

tsort :: Graph -> [Node]
tsort ([], []) = []
tsort g@(ns, _) = n : tsort g'
    where n = fst $ head $ filter (\(node, deg) -> deg == 0) $ map (\n -> (n, inDegree g n)) ns
          g' = removeNode g n

recoverSecret :: [String] -> String
recoverSecret = tsort . mkGraph
