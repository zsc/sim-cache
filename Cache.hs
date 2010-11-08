{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

-- ghc --make Cache.hs BoundedMap.hs -main-is Cache
module Cache where
import Prelude hiding (lookup)
import BoundedMap
import Control.Monad
import Control.Monad.State
--import qualified Data.Map as M
--class MapLike m k a | k a -> m where
--        lookup :: k -> m -> Maybe a
--        create :: b -> a -> m
--instance Ord k => MapLike (M.Map k a) k a where
--        lookup = M.lookup
--        create n epsilon = M.empty

accept :: (Ord a, Floating a, Ord k) => k -> State (a, BoundedMap k a) a
accept addr = do
        (r,m) <- get
        let exi = maybe 1 (\z -> 1 - r ** z) (lookup addr m)
        put (r,insert addr 0 (addOffset exi m))
        return exi
        
main = do
        let str = "abacdb"
        let n = 4
        let threshold = log 0.1 / log (1 - 1/n)
        putStrLn.show $ runState (sequence.map accept $ str) (1 - 1/n, create threshold) 