{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module IndexSet where

import Data.Functor
import Data.BitArray
import Control.DeepSeq
import Unsafe.Coerce

import qualified Data.DenseIntSet as S
import Indices

import Prelude hiding ( foldr )

import GHC.Exts ( build )

instance NFData S.IntSet

newtype Set a = IndexSet { unIndexSet :: S.IntSet }
  deriving (NFData, Show)


class IndexPred a where
    member :: Index i -> a i -> Bool
    notMember :: Index i -> a i -> Bool

instance IndexPred Set where
    Index x `member` IndexSet s = x `S.member` s
    {-# INLINE member #-}
    Index x `notMember` IndexSet s = x `S.notMember` s
    {-# INLINE notMember #-}


newtype Pred a = IndexPred { unIndexPred :: BitArray }  

instance IndexPred Pred where
    Index x `member` IndexPred s =
        let (l,u) = bitArrayBounds s
        in if x<l || x>u then False else unsafeLookupBit s x
    {-# INLINE member #-}
    x `notMember` s = not $ x `member` s
    {-# INLINE notMember #-}

seal :: Set a -> Pred a
seal (IndexSet s) = if S.null s
                    then IndexPred $ bitArray (0,0) []
                    else IndexPred $ bitArray (min,max) [ (x,True) | x <- S.toList s ]
  where min = S.findMin s
        max = S.findMax s


empty :: Set a
empty = IndexSet S.empty

insert :: Index a -> Set a -> Set a
Index x `insert` IndexSet s = IndexSet $ x `S.insert` s

singleton :: Index a -> Set a
singleton (Index x) = IndexSet $ S.singleton x


size :: Set a -> Int
size (IndexSet s) = S.size s

null :: Set a -> Bool
null (IndexSet s) = S.null s

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

toList :: Set a -> [Index a]
toList (IndexSet m1) = Index <$> S.toList m1

toAscList :: Set a -> [Index a]
toAscList (IndexSet m1) = Index <$> S.toAscList m1

{-# RULES "IndexSet/toList" forall is . toList is = build (\c n -> foldr c n is) #-}
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
