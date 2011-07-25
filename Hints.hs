{-# LANGUAGE OverloadedStrings #-}
module Hints where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Strict as ST
import qualified Data.ByteString.Lazy.Char8 as L

import Types

generateHints :: AtomIndex -> SuiteInfo -> SuiteInfo -> S.Set AtomI -> L.ByteString
generateHints ai testing unstable newAtoms =
    (`L.append` "\n" ) . ("easy " `L.append`) . L.unwords $
    [ L.fromChunks [srcName , "/", srcVersion ]
    | srcI <- S.toList addedSources
    , let (Source (SourceName srcName) (DebianVersion srcVersion)) = ai `lookupSrc` srcI
    ] ++ 
    [ L.fromChunks [srcName , "/", srcVersion, "/", arch ]
    | (srcI,Arch arch) <- S.toList binNMUedSources
    , let (Source (SourceName srcName) (DebianVersion srcVersion)) = ai `lookupSrc` srcI
    ]
  where (newSourcesIs, newBinariesIs, _) = splitAtomIs ai newAtoms
        addedSources = newSourcesIs `S.difference` sources testing
        addedBinaries = newBinariesIs `S.difference` binaries testing
        
        binNMUedSources = S.fromList 
            [ (srcI,arch)
            | binI <- S.toList addedBinaries
            , let srcI = builtBy unstable M.! binI 
            , srcI `S.notMember` addedSources
            , Binary _ _ (ST.Just arch) <- [ ai `lookupBin` binI]
            ]

splitAtomIs ai = (\(l1,l2,l3) -> (S.fromList l1, S.fromList l2, S.fromList l3)) .
             S.fold select ([],[],[])
  where select (Index i) ~(l1,l2,l3) = case ai `lookupAtom` Index i of
            (SrcAtom x) -> (Index i:l1,l2,l3)
            (BinAtom x) -> (l1,Index i:l2,l3)
            (BugAtom x) -> (l1,l2,Index i:l3)
