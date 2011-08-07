{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module IndexMap where

import Control.Arrow
import Data.Functor
import Control.DeepSeq

import qualified Data.IntMap as M
import Indices

newtype Map a b = IndexMap { unIndexMap :: M.IntMap b }
  deriving (NFData)

(IndexMap m) ! (Index i) = m M.! i
findWithDefault d (Index i) (IndexMap m) = M.findWithDefault d i m

(IndexMap m1) `union` (IndexMap m2) = IndexMap (m1 `M.union` m2)
(IndexMap m1) `difference` (IndexMap m2) = IndexMap (m1 `M.difference` m2)

unionWith f (IndexMap m1) (IndexMap m2) = IndexMap (M.unionWith f m1 m2)

keys (IndexMap m1) = Index <$> M.keys m1
toList (IndexMap m1) = first Index <$> M.toList m1

fromListWith f l = IndexMap $ M.fromListWith f (first unIndex <$> l)
fromList l = IndexMap $ M.fromList (first unIndex <$> l)
fromDistinctAscList l = IndexMap $ M.fromDistinctAscList (first unIndex <$> l)
