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


calculateBuiltBy :: [RawPackageInfo] -> BuiltBy
calculateBuiltBy = IxM.unions . map builtByR

-- Sources and binaries that will not be in testing, in any case. Can be used
-- to skip certain things, most notable generating the dependency information.
findNonCandidates :: Config -> AtomIndex -> SuiteInfo -> SuiteInfo -> GeneralInfo -> BuiltBy -> HintResults
    -> Producer (SrcI, String)
findNonCandidates config ai unstable testing general builtBy hr f x =
    (toProducer $ outdated ++ obsolete ++ tooyoung ++ blocked) f x
  where tooyoung = 
            -- packages need to be old enough
            [ (src, "it is " ++ show age ++ " days old, needs " ++ show minAge) |
                src <- IxM.keys buildsOnlyUnstable,
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
        obsolete = 
            -- never add a source package to testing that is already superceded
            [ (src, "it is already superceded by " ++ show (ai `lookupSrc` s)) |
                (src, bins) <- IxM.toList buildsOnlyUnstable,
                (s:_) <- [newerSources unstable IxM.! src]
            ]
        blocked = 
            [ (src, "is blocked by the release team") |
                src <- IxS.toList (blockedSources hr)
            ]

        buildsOnlyUnstable = {-# SCC "buildsOnlyUnstable" #-} IxM.difference (builds unstable) (builds testing)

-- Binaries that are in testing and will stay there.
-- TODO: Force removals
findUnmodified :: Config -> SuiteInfo -> SuiteInfo -> IxS.Set Source -> IxS.Set Binary
findUnmodified config unstable testing nonCandidates =
    binaries testing `IxS.intersection` binaries unstable
  where nonCandidateBins = {-# SCC "nonCandidateBins" #-} IxS.fromList $
            concatMap (builds unstable IxM.!) $
            IxS.toList nonCandidates

-- Wrapper around transitionRules' that prevents sharing. We _want_ to
-- recalculate the rules everytime 'build' is called upon the producer.
transitionRules :: Config -> AtomIndex -> SuiteInfo -> SuiteInfo -> GeneralInfo -> BuiltBy -> Producer (SrcI, String)
     -> Producer (Clause AtomI)
transitionRules config ai unstable testing general builtBy nc = toProducer $
    keepSrc ++ keepBin ++ uniqueBin ++ needsSource ++ needsBinary ++ releaseSync ++ completeBuild ++ nonCandidates ++ buggy 
  where keepSrc = 
            -- A source that exists both in unstable and in testing has to stay in testing
            {-# SCC "keepSrc" #-}
            [OneOf atoms ("source " ++ show name ++ " was in testing before.") |
                (name,pkgs) <- M.toList sourcesBoth,
                let atoms = map genIndex (nub pkgs)
            ]
        keepBin = 
            {-# SCC "keepBin" #-}
            -- A binary that exists both in unstable and in testing has to stay in testing
            [OneOf atoms ("binary " ++ show name ++ " on " ++ show arch ++ " was in testing before.") |
                ((name,arch),pkgs) <- M.toList binariesBoth,
                let atoms = map genIndex (nub pkgs)
            ]
        uniqueBin = 
            {-# SCC "uniqueBin" #-}
            -- At most one binary per name and architecture
            [AtMostOne (nub pkgs) ("at most version of " ++ show name ++ " ought to be unique on " ++ show arch) |
                ((name,arch),pkgs') <- M.toList binariesBoth,
                let pkgs = map genIndex (nub pkgs'),
                length pkgs > 1
            ]
        releaseSync = 
            {-# SCC "releaseSync" #-}
            -- release architectures ought to all migrate
            [Implies (genIndex src) [bin] ("release architectures ought to migrate completely") |
                (src, bins) <- IxM.toList buildsOnlyUnstable,
                -- BEWARE: If more than one binary with the same name built by the same
                -- source on the same architecture exists in unstable, this will
                -- contradict with the unique binary package rule.
                bin <- genIndex <$> nub bins
            ] 
        completeBuild = 
            {-# SCC "completeBuild" #-}
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

        needsSource = 
            {-# SCC "needsSource" #-}
            -- a package needs its source
            [Implies (genIndex bin) [genIndex src] ("of the DFSG") |
                (bin, src) <- IxM.toList builtBy
            ]
        needsBinary =
            {-# SCC "needsBinary" #-}
            -- a source needs a binary
            [Implies (genIndex src) bins ("it were useless otherwise") |
                (src, binIs) <- IxM.toList buildsUnion,
                let bins = map genIndex (nub binIs)
            ]
        nonCandidates =
            [ Not (genIndex atom) reason
            | (atom, reason) <- build nc
            ]
        buggy = 
            {-# SCC "buggy1" #-}
            -- no new RC bugs
            [Implies atom [bug] ("it has this bug") |
                (atom, bugs) <- IxM.toList bugsUnion,
                bug <- genIndex <$> nub bugs
            ] ++
            {-# SCC "buggy2" #-}
            [Not atom ("it was not in testing before") |
                
                atom <- genIndex <$> IxS.toList forbiddenBugs
            ]

        sourcesBoth = {-# SCC "sourcesBoth" #-} M.intersectionWith (++) (sourceNames unstable) (sourceNames testing)
        binariesBoth = {-# SCC "binariesBoth" #-} M.intersectionWith (++) (binaryNames unstable) (binaryNames testing)
        -- We assume that the dependency information is the same, even from different suites
        buildsUnion = {-# SCC "buildsUnion" #-} IxM.unionWith (++) (builds unstable) (builds testing)

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

        buildsOnlyUnstable = {-# SCC "buildsOnlyUnstable" #-} IxM.difference (builds unstable) (builds testing)


desiredAtoms :: SuiteInfo -> SuiteInfo -> Producer AtomI
desiredAtoms unstable testing = toProducer $
    fmap genIndex $ IxS.toList $ binaries unstable `IxS.difference` binaries testing

unwantedAtoms :: SuiteInfo -> SuiteInfo -> Producer AtomI
unwantedAtoms unstable testing = toProducer $ 
    fmap genIndex $ IxS.toList $ sources testing `IxS.difference` sources unstable

combine :: (Ord a, Ord b) => M.Map a b -> M.Map b c -> a -> Maybe c
combine m1 m2 x = (x `M.lookup` m1) >>= (`M.lookup` m2)
