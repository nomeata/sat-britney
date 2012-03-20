{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: Heidi
-- Copyright: (c) 2011 Joachim Breitner
-- License: GPL-2
--
module Heidi where

import qualified Data.Set as S
import qualified Data.Strict as ST
import qualified Data.ByteString.Lazy.Char8 as L

import Arches
import Types
import Indices
import AtomIndex

generateHeidi :: AtomIndex -> S.Set AtomI -> L.ByteString
generateHeidi ai = L.concat . map heidiLine . S.toList
  where heidiLine i = case ai `lookupAtom` i of
            SrcAtom (Source (SourceName n) (DebianVersion v))
                -> L.fromChunks [n, " ", v, " source\n"]
            BinAtom (Binary (BinName n) (DebianVersion v) ST.Nothing)
                -> L.fromChunks [n, " ", v, " all\n"]
            BinAtom (Binary (BinName n) (DebianVersion v) (ST.Just arch))
                -> L.fromChunks [n, " ", v, " ", archToByteString arch,"\n"]
            _   -> L.empty
