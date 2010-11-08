
module BoundedMap where
import qualified Data.Map as M
import Control.Monad

data BoundedMap key a = BoundedMap {
        h' :: M.Map key a,
        h'' :: M.Map key a, 
        offset :: a,
        threshold :: a,
        c :: a 
        } deriving (Eq,Show)

create :: (Num b) => b -> BoundedMap key b         
create threshold =
       BoundedMap {
        h' = M.empty,
        h'' = M.empty,
        offset = 0,
        threshold = threshold,
        c = 0
       }
       
lookup :: (Ord key, Num b) => key -> BoundedMap key b -> Maybe b
lookup addr t = fmap (+offset t) (M.lookup addr (h' t) `mplus` M.lookup addr (h'' t))

insert :: (Ord  k, Num a) => k -> a -> BoundedMap  k a -> BoundedMap  k a
insert k v t = t { h' = M.insert k (v - offset t) (h' t)}

addOffset :: (Num a, Ord a) => a -> BoundedMap k a -> BoundedMap k a
addOffset off t = if off + c t > threshold t 
        then t {h' = h'' t, h'' = M.empty, offset = offset t + off, c = c t + off - threshold t}
        else t {offset = offset t + off, c = c t + off}