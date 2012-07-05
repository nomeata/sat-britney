{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: ControlParser
-- Copyright: (c) 2011 Joachim Breitner
-- License: GPL-2
--
module ControlParser where

import Data.Word
import qualified Data.ByteString as DBS
import qualified Data.ByteString.Char8 as BS
import Data.Functor
import Data.Char

data Para = Para
    { packageField :: !BS.ByteString
    , versionField :: !BS.ByteString
    , architectureField :: !BS.ByteString
    , dependsField :: !BS.ByteString
    , preDependsField :: !BS.ByteString
    , providesField :: !BS.ByteString
    , conflictsField :: !BS.ByteString
    , breaksField :: !BS.ByteString
    , sourceField :: !BS.ByteString
    , sectionField :: !BS.ByteString
    }
    deriving (Eq, Show)

emptyPara :: Para
emptyPara = Para BS.empty BS.empty BS.empty BS.empty BS.empty BS.empty BS.empty BS.empty BS.empty BS.empty

parseControlFile :: FilePath -> IO [Para]
parseControlFile filename = parseLines . BS.lines <$> BS.readFile filename

parseLines :: [BS.ByteString] -> [Para]
parseLines = go emptyPara
  where go para (l:ls) | BS.null l && valid para = para : go emptyPara ls
                       | BS.null l               =        go emptyPara ls
                       | otherwise               =        go (addField l para) ls
        go para []     | valid para              = [para]
                       | otherwise               = []

valid :: Para -> Bool
valid = not . BS.null . packageField

addField :: BS.ByteString -> Para -> Para
addField l para =
    if n == "Package"      then para { packageField = v } else
    if n == "Version"      then para { versionField = v } else
    if n == "Architecture" then para { architectureField = v } else
    if n == "Depends"      then para { dependsField = v } else
    if n == "Pre-Depends"  then para { preDependsField = v } else
    if n == "Provides"     then para { providesField = v } else
    if n == "Conflicts"    then para { conflictsField = v } else
    if n == "Breaks"       then para { breaksField = v } else
    if n == "Source"       then para { sourceField = v } else
    if n == "Section"      then para { sectionField = v } else
    para
 where (n,v') = DBS.breakByte colon l
       v = BS.dropWhile isSpace (BS.tail v')

colon :: Word8
colon = fromIntegral (ord ':')
