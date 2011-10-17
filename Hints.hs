{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: Hints
-- Copyright: (c) 2011 Joachim Breitner
-- License: GPL-2
--
module Hints where

import qualified Data.Set as S
import qualified Data.Strict as ST
import qualified Data.ByteString.Lazy.Char8 as L

import Types
import Indices
import AtomIndex
import qualified IndexMap as IxM
import qualified IndexSet as IxS

generateHints :: AtomIndex -> SuiteInfo -> SuiteInfo -> IxM.Map Binary SrcI -> S.Set AtomI -> L.ByteString
generateHints ai testing unstable builtBy newAtoms =
    (if length hintStrings == 1 then ("# " `L.append`) else id) .
    (`L.append` "\n" ) . ("easy " `L.append`) . L.unwords $ hintStrings
  where hintStrings = 
            [ L.fromChunks [srcName , "/", srcVersion ]
            | srcI <- IxS.toList addedSources
            , let (Source (SourceName srcName) (DebianVersion srcVersion)) = ai `lookupSrc` srcI
            ] ++ 
            [ L.fromChunks [srcName , "/", arch, "/", srcVersion ]
            | (srcI,Arch arch) <- S.toList binNMUedSources
            , let (Source (SourceName srcName) (DebianVersion srcVersion)) = ai `lookupSrc` srcI
            ]
        (newSourcesIs, newBinariesIs, _) = splitAtomIs ai newAtoms
        addedSources = newSourcesIs `IxS.difference` sources testing
        addedBinaries = newBinariesIs `IxS.difference` binaries testing
        
        binNMUedSources = S.fromList 
            [ (srcI,arch)
            | binI <- IxS.toList addedBinaries
            , let srcI = builtBy IxM.! binI 
            , srcI `IxS.notMember` addedSources
            , Binary _ _ (ST.Just arch) <- [ ai `lookupBin` binI]
            ]

splitAtomIs ai = (\(l1,l2,l3) -> (IxS.fromList l1, IxS.fromList l2, IxS.fromList l3)) .
             S.fold select ([],[],[])
  where select (Index i) ~(l1,l2,l3) = case ai `lookupAtom` Index i of
            (SrcAtom x) -> (Index i:l1,l2,l3)
            (BinAtom x) -> (l1,Index i:l2,l3)
            (BugAtom x) -> (l1,l2,Index i:l3)
            _           -> (l1,l2,l3)
