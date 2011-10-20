{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module IndexMap where

import Control.Arrow
import Data.Functor
import Control.DeepSeq

import qualified Data.IntMap as M
import qualified IndexSet as IxS
import Indices

import GHC.Exts ( build )

instance NFData a => NFData (M.IntMap a) where rnf m = rnf (M.toList m)

newtype Map a b = IndexMap { unIndexMap :: M.IntMap b }
  deriving (NFData, Show)

empty :: Map t a
empty = IndexMap M.empty

(!) :: Map t a -> Index t1 -> a
(IndexMap m) ! (Index i) = m M.! i

lookup :: Index t -> Map t a -> Maybe a
lookup (Index i) (IndexMap m) = M.lookup i m

notMember :: Index t -> Map t a -> Bool
Index x `notMember` IndexMap s = x `M.notMember` s

member :: Index t -> Map t a -> Bool
Index x `member` IndexMap s = x `M.member` s


findWithDefault :: a -> Index t -> Map t a -> a
findWithDefault d (Index i) (IndexMap m) = M.findWithDefault d i m

union :: Map t b -> Map t b -> Map t b
IndexMap m1 `union` IndexMap m2 = IndexMap (m1 `M.union` m2)

unions :: [Map t b] -> Map t b
unions = IndexMap . M.unions . Prelude.map unIndexMap

unionsWith ::  (b -> b -> b) -> [Map t b] -> Map a b
unionsWith f = IndexMap . M.unionsWith f . Prelude.map unIndexMap

keysSet :: Map t b -> IxS.Set t
keysSet (IndexMap m) = IxS.fromDistinctAscList $ Prelude.map Index $ M.keys m


size :: Map t a -> Int
size (IndexMap s) = M.size s

difference :: Map t b -> Map t1 b1 -> Map a b
IndexMap m1 `difference` IndexMap m2 = IndexMap (m1 `M.difference` m2)

unionWith :: (b -> b -> b) -> Map t b -> Map t1 b -> Map a b
unionWith f (IndexMap m1) (IndexMap m2) = IndexMap (M.unionWith f m1 m2)

keys :: Map t a1 -> [Index a]
keys (IndexMap m1) = Index <$> M.keys m1

elems :: Map t a1 -> [a1]
elems (IndexMap m1) = M.elems m1

toList :: Map t d -> [(Index a, d)]
toList (IndexMap m1) = first Index <$> M.toList m1

{-# RULES "IndexMap/toList" forall m . toList m = build (\c n -> foldWithKey (\k x xs -> c (k,x) xs) n m) #-}

fromListWith :: (b -> b -> b) -> [(Index a1, b)] -> Map a b
fromListWith f l = IndexMap $ M.fromListWith f (first unIndex <$> l)

fromList :: [(Index a1, b)] -> Map a b
fromList l = IndexMap $ M.fromList (first unIndex <$> l)

fromAscList :: [(Index a1, b)] -> Map a b
fromAscList l = IndexMap $ M.fromAscList (first unIndex <$> l)

fromDistinctAscList :: [(Index a1, b)] -> Map a b
fromDistinctAscList l = IndexMap $ M.fromDistinctAscList (first unIndex <$> l)

filterWithKey :: (Index a1 -> b -> Bool) -> Map t b -> Map a b
filterWithKey f (IndexMap m) = IndexMap $ M.filterWithKey (\k v -> f (Index k) v) m

insertWith :: (b -> b -> b) -> Index t -> b -> Map t b -> Map a b
insertWith f (Index k) v (IndexMap m) = IndexMap $ M.insertWith f k v m

filter :: (b -> Bool) -> Map t b -> Map a b
filter f (IndexMap m) = IndexMap $ M.filter f m

map :: (a1 -> b) -> Map t a1 -> Map t b
map f (IndexMap m) = IndexMap $ M.map f m

mapWithKey :: (Index t -> a1 -> b) -> Map t a1 -> Map t b
mapWithKey f (IndexMap m) = IndexMap $ M.mapWithKey (\i v -> f (Index i) v) m

mapMaybeWithKey :: (Index t -> a1 -> Maybe b) -> Map t a1 -> Map t b
mapMaybeWithKey f (IndexMap m) = IndexMap $ M.mapMaybeWithKey (\i v -> f (Index i) v) m

foldWithKey :: (Index t -> a -> b -> b) -> b -> Map t a -> b
foldWithKey f x (IndexMap m) = M.foldWithKey (\i v x' -> f (Index i) v x') x m

fold :: (a -> b -> b) -> b -> Map t a -> b
fold f x (IndexMap m) = M.fold f x m

