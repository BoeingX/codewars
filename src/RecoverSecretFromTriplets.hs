module RecoverSecretFromTriplets where

import Data.List (nub)
import Data.Graph
import Data.Map (fromList, (!))

recoverSecret :: [String] -> String
recoverSecret xs = map (i2v !) $ topSort $ buildG (1, length vs) es
    where vs = nub $ concat xs
          v2i = fromList $ zip vs [1..]
          i2v = fromList $ zip [1..] vs
          es = concatMap (\[x,y,z] -> [(x, y), (y, z), (x, z)]) $ map (map (v2i !)) xs
