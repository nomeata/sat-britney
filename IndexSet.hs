{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module IndexSet where

import Data.Functor
import Control.DeepSeq
import Unsafe.Coerce

import qualified Data.IntSet as S
import Indices

newtype Set a = IndexSet { unIndexSet :: S.IntSet }
  deriving (NFData)

empty :: Set a
empty = IndexSet S.empty

insert :: Index a -> Set a -> Set a
Index x `insert` IndexSet s = IndexSet $ x `S.insert` s

notMember :: Index a -> Set a -> Bool
Index x `notMember` IndexSet s = x `S.notMember` s

member :: Index a -> Set a -> Bool
Index x `member` IndexSet s = x `S.member` s

size :: Set a -> Int
size (IndexSet s) = S.size s

union :: Set a -> Set a -> Set a
IndexSet m1 `union` IndexSet m2 = IndexSet (m1 `S.union` m2)

difference :: Set a -> Set a -> Set a
IndexSet m1 `difference` IndexSet m2 = IndexSet (m1 `S.difference` m2)

toList :: Set a -> [Index a]
toList (IndexSet m1) = Index <$> S.toList m1

toAscList :: Set a -> [Index a]
toAscList (IndexSet m1) = Index <$> S.toAscList m1

fromList :: [Index a] -> Set a
fromList l = IndexSet $ S.fromList (unIndex <$> l)

fromDistinctAscList :: [Index a] -> Set a
fromDistinctAscList l = IndexSet $ S.fromDistinctAscList (unIndex <$> l)

filter :: (Index a -> Bool) -> Set a -> Set a
filter f (IndexSet s) = IndexSet $ S.filter (f . Index) s

fold :: (Index a -> b -> b) -> b -> Set a -> b
fold f x (IndexSet s) = S.fold (\i x' -> f (Index i) x') x s

generalize :: Set a -> Set b
-- generalize (IndexSet s) = IndexSet $ S.mapMonotonic (\(Index i) -> Index i)
generalize = unsafeCoerce 
