{-# LANGUAGE RecordWildCards, Rank2Types, ImpredicativeTypes, TupleSections #-}
-- |
-- Module: TransRules
-- Copyright: (c) 2011 Joachim Breitner
-- License: GPL-2
--
module TransRules where

import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified Data.Strict as ST
import qualified Data.Map as M
import Data.Functor
import Data.Function
import Control.Arrow ((&&&), first)
import Control.Monad.State
import Debug.Trace
import Safe
import GHC.Exts (build)

import ParseHints
import Indices
import Types
import AtomIndex
import LitSat
import PrettyPrint
import qualified Data.Set as S
import qualified IndexSet as IxS
import qualified IndexMap as IxM


-- Sources and binaries that will not be in testing, in any case. Can be used
-- to skip certain things, most notable generating the dependency information.
findNonCandidates :: Config -> AtomIndex -> SuiteInfo -> SuiteInfo -> GeneralInfo -> BuiltBy -> HintResults
    -> Producer (SrcI, String)
findNonCandidates config ai unstable testing general builtBy hr f x =
    (toProducer $ outdated ++ missingArch ++ obsolete ++ tooyoung ++ blocked ++ isMoreBuggy) f x
  where tooyoung = 
            -- packages need to be old enough
            [ (src, "it is " ++ show age ++ " days old, needs " ++ show minAge) |
                src <- IxS.toList sourcesOnlyUnstable,
                Just age <- [src `M.lookup` ages general],
                let minAge = fromMaybe (defaultMinAge config) $
                             urgencies general `combine` minAges config $ src,
                age <= minAge
            ] 
        outdated = 
            -- release architectures ought not to be out of date
            [ (newer, "is out of date: " ++ show (ai `lookupBin` binI) ++ " exists in unstable") |
                binI <- IxS.toList (binaries unstable),
                let srcI = builtBy IxM.! binI,
                -- TODO: only release architecture here
                newer <- newerSources unstable IxM.! srcI,
                newer `IxS.notMember` sources testing
            ]
        missingArch = 
            -- release architectures ought not be missing in unstable
            [ (srcIu, "is out of date: " ++ show a ++ " not built in unstable") |
                (srcIt, archSt) <- IxM.toList (buildsArches testing),
                let (Source pkg vt) = ai `lookupSrc` srcIt,
                srcIu <- fromMaybe [] $ M.lookup pkg (sourceNames unstable),
                srcIu `IxS.notMember` sources testing,
                let archSu = fromMaybe S.empty $ IxM.lookup srcIu (buildsArches unstable),
                a <- S.toList $ archSt S.\\ archSu
            ]
        obsolete = 
            -- never add a source package to testing that is already superceded
            [ (src, "it is already superceded by " ++ show (ai `lookupSrc` s)) |
                src <- IxS.toList sourcesOnlyUnstable,
                (s:_) <- [newerSources unstable IxM.! src]
            ]
        blocked = 
            [ (src, "is blocked by the release team") |
                src <- IxS.toList (blockedSources hr)
            ]
        isMoreBuggy = 
            [ (srcI, "has new bug " ++ show (ai `lookupBug` bugI)) |
                (aI,bugIs) <- IxM.toList $ bugs unstable,
                BinAtom (Binary pkg _ arch) <- return $ ai `lookupAtom` aI,
                let inTesting = [ bugI' |
                        Just (binI's) <- return $ M.lookup (pkg, ST.fromMaybe (archForAll config) arch) $ binaryNames testing,
                        binI' <- binI's,
                        bugI'st <- return $ fromMaybe [] $ IxM.lookup (genIndex binI') $ bugs testing,
                        bugI'su <- return $ fromMaybe [] $ IxM.lookup (genIndex binI') $ bugs unstable,
                        bugI' <- bugI'st ++ bugI'su
                        ],
                bugI <- bugIs,
                bugI `notElem` inTesting,
                let srcI = builtBy IxM.! aI
            ]
            

        sourcesOnlyUnstable = IxS.difference (sources unstable) (sources testing)

-- Binaries that are in testing and will stay there.
-- TODO: Force removals
findUnmodified :: Config -> SuiteInfo -> SuiteInfo -> IxS.Set Source -> IxS.Set Binary
findUnmodified config unstable testing nonCandidates =
    binaries testing `IxS.intersection` binaries unstable
  where nonCandidateBins = {-# SCC "nonCandidateBins" #-} IxS.fromList $
            concatMap (builds unstable IxM.!) $
            IxS.toList nonCandidates

transitionRules :: Config -> AtomIndex -> SuiteInfo -> SuiteInfo -> GeneralInfo -> BuiltBy -> Producer (SrcI, String)
     -> Producer (Clause AtomI)
transitionRules config ai unstable testing general builtBy nc f x = (toProducer $
    keepSrc ++ keepBin ++ uniqueBin ++ needsSource ++ binNMUsync ++ newSourceSync ++ completeBuild ++ nonCandidates ++ buggy ) f x
  where keepSrc = 
            -- A source that exists both in unstable and in testing has to stay in testing
            [OneOf atoms ("source " ++ show name ++ " was in testing before.") |
                (name,pkgs) <- M.toList sourcesBoth,
                let atoms = map genIndex (nub pkgs)
            ]
        keepBin = 
            -- A binary that exists both in unstable and in testing has to stay in testing
            [OneOf atoms ("binary " ++ show name ++ " on " ++ show arch ++ " was in testing before.") |
                ((name,arch),pkgs) <- M.toList binariesBoth,
                let atoms = map genIndex (nub pkgs)
            ]
        uniqueBin = 
            -- At most one binary per name and architecture
            [AtMostOne (nub pkgs) ("at most version of " ++ show name ++ " ought to be unique on " ++ show arch) |
                ((name,arch),pkgs') <- M.toList binariesBoth,
                let pkgs = map genIndex (nub pkgs'),
                length pkgs > 1
            ]
        binNMUsync = 
            -- For each source, keep all binary packages from unstable on an
            -- architecture together. Assumes that the one-binary-package
            -- condition is fulfilled in unstable
            [AllOrNone binIs ("builds should not be separated") |
                (src, bins) <- IxM.toList (builds unstable),
                binPerArch <- groupBy ((==) `on` binArch . snd) .
                              sortBy  (compare `on` binArch . snd) .
                              map (id &&& (ai `lookupBin`)) $ bins,
                length binPerArch > 1,
                -- Not for arch all, not required
                ST.isJust $ binArch (snd (head binPerArch)),
                binGroup   <- groupBy ((==)    `on` binVersion . snd) .
                              sortBy  (compare `on` binVersion . snd) $ binPerArch,
                length binGroup > 1,
                let binIs = map (genIndex . fst) binGroup
            ]
        completeBuild = 
            -- For each source and each arch each binary name built by the
            -- source, depend on all binaries with that name. There is exactly
            -- one such binary, unless there are binNMUs.
            [Implies (genIndex src) binIs ("all binaries stay with the source") |
                src <- IxS.toList (sources testing),
                let bins = IxS.toList $ IxS.fromList $
                        fromMaybe [] (IxM.lookup src (builds unstable)) ++
                        fromMaybe [] (IxM.lookup src (builds testing)),
                binsPerArchAndName <-
                    map (map fst) .
                    groupBy ((==) `on` (binArch &&& binName) . snd) .
                    sortBy  (compare `on` (binArch &&& binName) . snd) .
                    map (id &&& (ai `lookupBin`)) $ bins,
                -- Do not force this if there is not a binary in testing already.
                any (`IxS.member` binaries testing) binsPerArchAndName,
                let binIs = map genIndex binsPerArchAndName
            ]
        newSourceSync =
            [Implies (genIndex src) binIs ("all binaries stay with the source") |
                (src, bins) <- IxM.toList (builds unstable),
                src `IxS.notMember` sources testing,
                -- This might be redundant, as all these sets will be
                -- singletons anyways
                binsPerArchAndName <-
                    map (map fst) .
                    groupBy ((==) `on` (binArch &&& binName) . snd) .
                    sortBy  (compare `on` (binArch &&& binName) . snd) .
                    map (id &&& (ai `lookupBin`)) $ bins,
                let binIs = map genIndex binsPerArchAndName
            ]
        needsSource = 
            -- a package needs its source
            [Implies (genIndex bin) [genIndex src] ("of the DFSG") |
                (bin, src) <- IxM.toList builtBy,
                bin `IxS.notMember` smoothBinaries testing 
            ]
        nonCandidates =
            [ Not (genIndex atom) reason
            | (atom, reason) <- build nc
            ]
        buggy = 
            -- no new RC bugs
            [Implies atom [bug] ("it has this bug") |
                (atom, bugs) <- IxM.toList bugsUnion,
                bug <- genIndex <$> nub bugs
            ] ++
            [Not atom ("it was not in testing before") |
                
                atom <- genIndex <$> IxS.toList forbiddenBugs
            ]

        sourcesBoth = {-# SCC "sourcesBoth" #-} M.intersectionWith (++) (sourceNames unstable) (sourceNames testing)
        binariesBoth = {-# SCC "binariesBoth" #-} M.intersectionWith (++) (binaryNames unstable) (binaryNames testing)
        -- We assume that the dependency information is the same, even from different suites

        bugsUnion = {-# SCC "bugsUnion" #-} IxM.unionWith (++) (bugs unstable) (bugs testing)

        -- This does not work, as bugs with tag "sid" would appear as new bugs
        -- bugsInTesting = IxS.fromList (concat (M.elems (bugs testing)))
        -- bugsInUnstable = {-# SCC "bugsInUnstable" #-} IxS.fromList (concat (M.elems (bugs unstable)))
        bugsInTesting = {-# SCC "bugsInTesting" #-} IxS.fromList [ bug |
            atom <- IxS.toList (atoms testing),
            bug <- IxM.findWithDefault [] atom bugsUnion ]
        bugsInUnstable = {-# SCC "bugsInUnstable" #-} IxS.fromList [ bug |
            atom <- IxS.toList (atoms unstable),
            bug <- IxM.findWithDefault [] atom bugsUnion ]
        forbiddenBugs = {-# SCC "forbiddenBugs" #-} bugsInUnstable `IxS.difference` bugsInTesting

desiredAtoms :: SuiteInfo -> SuiteInfo -> Producer AtomI
desiredAtoms unstable testing f x = (toProducer $
    fmap genIndex $ IxS.toList $ binaries unstable `IxS.difference` binaries testing) f x

unwantedAtoms :: SuiteInfo -> SuiteInfo -> Producer AtomI
unwantedAtoms unstable testing f x = (toProducer $ 
    fmap genIndex $ IxS.toList $ binaries testing `IxS.difference` binaries unstable) f x

combine :: (Ord a, Ord b) => M.Map a b -> M.Map b c -> a -> Maybe c
combine m1 m2 x = (x `M.lookup` m1) >>= (`M.lookup` m2)
