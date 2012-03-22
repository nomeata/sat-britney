{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ArchMap where

import Control.Arrow
import Data.Functor
import Control.DeepSeq

import qualified Data.IntMap as M
import Arches

import qualified GHC.Exts ( build )

-- instance NFData a => NFData (M.IntMap a) where rnf m = rnf (M.toList m)

newtype Map b = ArchMap { unArchMap :: M.IntMap b }
  deriving (NFData, Show)

empty :: Map a
empty = ArchMap M.empty

(!) :: Map a -> Arch -> a
(ArchMap m) ! (Arch i) = m M.! i

lookup :: Arch-> Map a -> Maybe a
lookup (Arch i) (ArchMap m) = M.lookup i m

notMember :: Arch -> Map a -> Bool
Arch x `notMember` ArchMap s = x `M.notMember` s

member :: Arch -> Map a -> Bool
Arch x `member` ArchMap s = x `M.member` s


findWithDefault :: a -> Arch -> Map a -> a
findWithDefault d (Arch i) (ArchMap m) = M.findWithDefault d i m

union :: Map b -> Map b -> Map b
ArchMap m1 `union` ArchMap m2 = ArchMap (m1 `M.union` m2)

unions :: [Map b] -> Map b
unions = ArchMap . M.unions . Prelude.map unArchMap

unionsWith ::  (b -> b -> b) -> [Map b] -> Map b
unionsWith f = ArchMap . M.unionsWith f . Prelude.map unArchMap

size :: Map a -> Int
size (ArchMap m) = M.size m

difference :: Map b -> Map b1 -> Map b
ArchMap m1 `difference` ArchMap m2 = ArchMap (m1 `M.difference` m2)

unionWith :: (b -> b -> b) -> Map b -> Map b -> Map b
unionWith f (ArchMap m1) (ArchMap m2) = ArchMap (M.unionWith f m1 m2)

keys :: Map a1 -> [Arch]
keys (ArchMap m1) = Arch <$> M.keys m1

elems :: Map a1 -> [a1]
elems (ArchMap m1) = M.elems m1

toList :: Map d -> [(Arch, d)]
toList (ArchMap m1) = first Arch <$> M.toList m1

{-# RULES "ArchMap/toList" forall m . toList m = GHC.Exts.build (\c n -> foldWithKey (\k x xs -> c (k,x) xs) n m) #-}

fromListWith :: (b -> b -> b) -> [(Arch, b)] -> Map b
fromListWith f l = ArchMap $ M.fromListWith f (first unArch <$> l)

fromList :: [(Arch, b)] -> Map b
fromList l = ArchMap $ M.fromList (first unArch <$> l)

fromAscList :: [(Arch, b)] -> Map b
fromAscList l = ArchMap $ M.fromAscList (first unArch <$> l)

fromDistinctAscList :: [(Arch, b)] -> Map b
fromDistinctAscList l = ArchMap $ M.fromDistinctAscList (first unArch <$> l)

filterWithKey :: (Arch -> b -> Bool) -> Map b -> Map b
filterWithKey f (ArchMap m) = ArchMap $ M.filterWithKey (\k v -> f (Arch k) v) m

insertWith :: (b -> b -> b) -> Arch -> b -> Map b -> Map b
insertWith f (Arch k) v (ArchMap m) = ArchMap $ M.insertWith f k v m

filter :: (b -> Bool) -> Map b -> Map b
filter f (ArchMap m) = ArchMap $ M.filter f m

map :: (a1 -> b) -> Map a1 -> Map b
map f (ArchMap m) = ArchMap $ M.map f m

mapWithKey :: (Arch -> a1 -> b) -> Map a1 -> Map b
mapWithKey f (ArchMap m) = ArchMap $ M.mapWithKey (\i v -> f (Arch i) v) m

mapMaybeWithKey :: (Arch -> a1 -> Maybe b) -> Map a1 -> Map b
mapMaybeWithKey f (ArchMap m) = ArchMap $ M.mapMaybeWithKey (\i v -> f (Arch i) v) m

foldWithKey :: (Arch -> a -> b -> b) -> b -> Map a -> b
foldWithKey f x (ArchMap m) = M.foldWithKey (\i v x' -> f (Arch i) v x') x m

fold :: (a -> b -> b) -> b -> Map a -> b
fold f x (ArchMap m) = M.fold f x m

build :: [Arch] -> (Arch -> b) -> Map b
build as f = fromList [ (a, f a) | a <- as ]

buildM :: [Arch] -> (Arch -> IO b) -> IO (Map b)
buildM as f = fromList <$> sequence [ do { x <- f a ; return (a , x) } | a <- as ]
