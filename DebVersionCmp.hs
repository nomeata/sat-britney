{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
-- |
-- Module: DebVersionCmp
-- Copyright: (c) 2011 Joachim Breitner
-- License: GPL-2
--
module DebVersionCmp where

import System.IO.Unsafe
import Foreign.Ptr
import Foreign.Marshal
import Foreign.C.Types
import Foreign.C.String

import qualified Data.ByteString as BS

data ParsedVersion

data DpkgError

foreign import ccall unsafe "dpkg/dpkg-db.h parseversion"
    c_ParseVersion :: Ptr ParsedVersion -> CString -> Ptr DpkgError -> IO CInt

foreign import ccall unsafe "dpkg/version.h dpkg_version_compare"
    c_VersionCompare :: Ptr ParsedVersion -> Ptr ParsedVersion -> IO CInt

foreign export ccall "thisname" blubb :: ()

blubb :: ()
blubb = ()

versionCompare :: BS.ByteString -> BS.ByteString -> Ordering
versionCompare v1 v2 = unsafePerformIO $
    -- 12 bytes is enough to carry a struct versionrevision
    allocaBytes 12 $ \ptr1 -> allocaBytes 12 $ \ptr2 ->  
    allocaBytes 16 $ \eptr -> 
    BS.useAsCString v1 $ \v1p -> BS.useAsCString v2 $ \v2p -> do
        r1 <- c_ParseVersion ptr1 v1p eptr
        if (r1 /= 0)
            then peekCString (castPtr (eptr `plusPtr` 8)) >>= \err -> error $ "Failed to parse " ++ show v1 ++ ": " ++ err
            else do
        r2 <- c_ParseVersion ptr2 v2p eptr
        if (r2 /= 0) 
            then peekCString (castPtr (eptr `plusPtr` 8)) >>= \err -> error $ "Failed to parse " ++ show v2 ++ ": " ++ err
            else do
        r3 <- c_VersionCompare ptr1 ptr2
        return $ if r3 < 0 then LT else if r3 == 0 then EQ else GT
