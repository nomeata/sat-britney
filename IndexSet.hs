{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module IndexSet where

import Data.Functor
import Data.BitArray
import Control.DeepSeq
import Unsafe.Coerce
import qualified Data.Vector.Unboxed as U
import Data.Int

import qualified Data.IntSet as S
import qualified ZArray as Z
import Indices

import Prelude hiding ( foldr )

import GHC.Exts ( build )

-- instance NFData S.IntSet

newtype Set a = IndexSet { unIndexSet :: S.IntSet }
  deriving (NFData, Show)


class IndexPred a where
    member :: Index i -> a i -> Bool
    notMember :: Index i -> a i -> Bool
    toList :: a i -> [Index i]
    size :: a i -> Int
    null :: a i -> Bool


instance IndexPred Set where
    Index x `member` IndexSet s = x `S.member` s
    {-# INLINE member #-}
    Index x `notMember` IndexSet s = x `S.notMember` s
    {-# INLINE notMember #-}
    toList (IndexSet s) = map Index $ S.toList s
    {-# INLINE toList #-}
    size (IndexSet s) = S.size s
    {-# INLINE size #-}
    null (IndexSet s) = S.null s
    {-# INLINE null #-}

{-# RULES "IndexSet/toList" forall is . toList is = build (\c n -> foldr c n is) #-}

newtype Pred a = IndexPred { unIndexPred :: Z.Array }  
  deriving (Show)

instance IndexPred Pred where
    Index x `member` IndexPred s = binSearch (fromIntegral x) s 
    {-# INLINE member #-}
    x `notMember` s = not $ x `member` s
    {-# INLINE notMember #-}
    toList (IndexPred s) = map (Index . fromIntegral) $ Z.toList s
    {-# INLINE toList #-}
    size (IndexPred s) = Z.length s
    {-# INLINE size #-}
    null (IndexPred s) = Z.null s
    {-# INLINE null #-}


binSearch :: Int32 -> Z.Array -> Bool
binSearch b v = go 0 (Z.length v - 1)
  where go l u = if l > u then False else
            let m = (l + u) `div` 2
            in  case b `compare` Z.unsafeIndex m v of
                LT -> go l (m-1)
                EQ -> True
                GT -> go (m +1) u

seal :: Set a -> Pred a
seal (IndexSet s) = IndexPred $ Z.fromList $ map fromIntegral $ S.toList s

empty :: Set a
empty = IndexSet S.empty

insert :: Index a -> Set a -> Set a
Index x `insert` IndexSet s = IndexSet $ x `S.insert` s

singleton :: Index a -> Set a
singleton (Index x) = IndexSet $ S.singleton x

isSubsetOf :: Set a -> Set a -> Bool
IndexSet m1 `isSubsetOf` IndexSet m2 = m1 `S.isSubsetOf` m2

union :: Set a -> Set a -> Set a
IndexSet m1 `union` IndexSet m2 = IndexSet (m1 `S.union` m2)

intersection :: Set a -> Set a -> Set a
IndexSet m1 `intersection` IndexSet m2 = IndexSet (m1 `S.intersection` m2)

unions :: [Set a] -> Set a
unions = IndexSet . S.unions . Prelude.map unIndexSet

difference :: Set a -> Set a -> Set a
IndexSet m1 `difference` IndexSet m2 = IndexSet (m1 `S.difference` m2)

toAscList :: Set a -> [Index a]
toAscList (IndexSet m1) = Index <$> S.toAscList m1

{-# RULES "IndexSet/toAscList" forall is . toList is = build (\c n -> foldr c n is) #-}

fromList :: [Index a] -> Set a
fromList l = IndexSet $ S.fromList (unIndex <$> l)

fromDistinctAscList :: [Index a] -> Set a
fromDistinctAscList l = IndexSet $ S.fromDistinctAscList (unIndex <$> l)

filter :: (Index a -> Bool) -> Set a -> Set a
filter f (IndexSet s) = IndexSet $ S.filter (f . Index) s

fold :: (Index a -> b -> b) -> b -> Set a -> b
fold f x (IndexSet s) = S.fold (\i x' -> f (Index i) x') x s

foldr :: (Index a -> b -> b) -> b -> Set a -> b
foldr f x (IndexSet s) = S.foldr (\i x' -> f (Index i) x') x s

generalize :: Set a -> Set b
-- generalize (IndexSet s) = IndexSet $ S.mapMonotonic (\(Index i) -> Index i)
generalize = unsafeCoerce 
