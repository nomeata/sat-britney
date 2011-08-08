{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module IndexMap where

import Control.Arrow
import Data.Functor
import Control.DeepSeq

import qualified Data.IntMap as M
import Indices

newtype Map a b = IndexMap { unIndexMap :: M.IntMap b }
  deriving (NFData)

(!) :: Map t a -> Index t1 -> a
(IndexMap m) ! (Index i) = m M.! i

findWithDefault :: a -> Index t -> Map t1 a -> a
findWithDefault d (Index i) (IndexMap m) = M.findWithDefault d i m

union :: Map t b -> Map t1 b -> Map a b
IndexMap m1 `union` IndexMap m2 = IndexMap (m1 `M.union` m2)

difference :: Map t b -> Map t1 b1 -> Map a b
IndexMap m1 `difference` IndexMap m2 = IndexMap (m1 `M.difference` m2)

unionWith :: (b -> b -> b) -> Map t b -> Map t1 b -> Map a b
unionWith f (IndexMap m1) (IndexMap m2) = IndexMap (M.unionWith f m1 m2)

keys :: Map t a1 -> [Index a]
keys (IndexMap m1) = Index <$> M.keys m1

toList :: Map t d -> [(Index a, d)]
toList (IndexMap m1) = first Index <$> M.toList m1

fromListWith :: (b -> b -> b) -> [(Index a1, b)] -> Map a b
fromListWith f l = IndexMap $ M.fromListWith f (first unIndex <$> l)

fromList :: [(Index a1, b)] -> Map a b
fromList l = IndexMap $ M.fromList (first unIndex <$> l)

fromDistinctAscList :: [(Index a1, b)] -> Map a b
fromDistinctAscList l = IndexMap $ M.fromDistinctAscList (first unIndex <$> l)

filterWithKey :: (Index a1 -> b -> Bool) -> Map t b -> Map a b
filterWithKey f (IndexMap m) = IndexMap $ M.filterWithKey (\k v -> f (Index k) v) m

map :: (a1 -> b) -> Map t a1 -> Map a b
map f (IndexMap m) = IndexMap $ M.map f m

