{-# LANGUAGE MagicHash, ScopedTypeVariables #-}
module ZArray where

import Data.Primitive.Types
import qualified Data.Primitive.ByteArray as PA
import Control.Monad
import Control.Monad.ST
import Data.Int
import Data.List
import GHC.Exts

newtype Array = Array PA.ByteArray

fromList :: [Int32] -> Array
fromList l = Array $ runST $ do
    ba <- PA.newByteArray (length l * I# (sizeOf# (undefined :: Int32)))
    forM_ (zip [0..] l) $ \(i,v) -> do
        PA.writeByteArray ba i v
    PA.unsafeFreezeByteArray ba

singleton :: Int32 -> Array
singleton x = fromList [x]

toList :: Array -> [Int32]
toList (Array v) = map (PA.indexByteArray v) [0..len-1]
  where len = PA.sizeofByteArray v `div` I# (sizeOf# (undefined :: Int32))

any :: (Int32 -> Bool) -> Array -> Bool
any p (Array v) = Prelude.any (\i -> p (PA.indexByteArray v i)) [0..len-1]
  where len = PA.sizeofByteArray v `div` I# (sizeOf# (undefined :: Int32))

all :: (Int32 -> Bool) -> Array -> Bool
all p (Array v) = Prelude.all (\i -> p (PA.indexByteArray v i)) [0..len-1]
  where len = PA.sizeofByteArray v `div` I# (sizeOf# (undefined :: Int32))

null :: Array -> Bool
null (Array v) = PA.sizeofByteArray v == 0

filter :: (Int32 -> Bool) -> Array -> Array
filter p v | ZArray.all p v = v
           | otherwise = fromList (Prelude.filter p (toList v))

sort :: Array -> Array
sort v = fromList (Data.List.sort (toList v))

instance Eq Array where
    v1 == v2 = toList v1 == toList v2
instance Ord Array where
    v1 `compare` v2 = toList v1 `compare` toList v2
