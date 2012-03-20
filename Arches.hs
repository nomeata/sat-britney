{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Arches where

import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import Control.DeepSeq
import qualified Data.Vector as V

newtype Arch = Arch { unArch :: Int }
    deriving (Ord, Eq, NFData)

archNames :: V.Vector String
archNames = V.fromList $ sort $ words $ "i386 sparc powerpc armel ia64 mips mipsel s390 amd64 kfreebsd-i386 kfreebsd-amd64"

archNamesBS :: V.Vector BS.ByteString
archNamesBS = V.map BS.pack archNames

nArches :: Int
nArches = V.length archNamesBS

allArches :: V.Vector Arch
allArches = V.map read archNames

instance Show Arch where
    show (Arch i) = archNames V.! i

instance Read Arch where
    readsPrec _ ss = mapMaybe (\(n,i) ->
        if n `isPrefixOf` ss
        then Just (Arch i, drop (length n) ss)
        else Nothing
        ) $ zip (V.toList archNames) [0,1..]
    
instance Enum Arch where
    toEnum n | 0 <= n && n < nArches = Arch n
    toEnum _ = error "Arch toEnum out of range"
    fromEnum (Arch n) = n

archFromByteString bs =
    maybe (error "Arch.fromByteString: Bad arch name")
          Arch
          (V.elemIndex bs archNamesBS)

archToByteString (Arch i) = archNamesBS V.! i
