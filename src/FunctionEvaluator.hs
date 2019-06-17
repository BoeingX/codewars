module FunctionEvaluator where

import qualified Control.Monad.Trans.State as S
import qualified Data.Map as M

eval :: (Ord a) => (a -> Either b ([a], [b] -> b)) -> a -> S.State (M.Map a b) b
eval f a = do
    m <- S.get
    case M.lookup a m of
        Just b -> return b
        Nothing -> do
          b <- case f a of
                 Left b -> return b
                 Right (as, g) -> g <$> mapM (eval f) as
          S.modify $ M.insert a b
          return b

evaluateFunction :: Ord a => (a -> Either b ([a], [b] -> b)) -> a -> b
evaluateFunction f a = S.evalState (eval f a) M.empty
