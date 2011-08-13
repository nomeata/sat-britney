-- |
-- Module: TransRules
-- Copyright: (c) 2011 Joachim Breitner
-- License: GPL-2
--
module TransRules where

import Data.List
import Data.Map ((!))
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified Data.Strict as ST
import qualified Data.Map as M
import Data.Functor
import Data.Function
import Control.Arrow ((&&&))

import Types
import LitSat
import qualified IndexSet as IxS
import qualified IndexMap as IxM

thinSuite config ai suite general = SuiteInfo
    { sources = sources'
    , binaries = binaries'
    , atoms = atoms'
    , sourceNames = M.map (filter (`IxS.member` sources')) $ sourceNames suite
    , binaryNames = M.map (filter (`IxS.member` binaries')) $ binaryNames suite
    , builds = IxM.filterWithKey (\k _ -> k `IxS.member` sources') $ builds suite
    , builtBy = IxM.filterWithKey (\k _ -> k `IxS.member` binaries') $ builtBy suite
    , depends = IxM.filterWithKey (\k _ -> k `IxS.member` binaries') $ depends suite
    , provides = M.map (filter (`IxS.member` binaries')) $ provides suite
    , conflicts = IxM.filterWithKey (\k _ -> k `IxS.member` binaries') $ conflicts suite
    , breaks = IxM.filterWithKey (\k _ -> k `IxS.member` binaries') $ breaks suite
    , newerSources = IxM.filterWithKey (\k _ -> k `IxS.member` sources') $
                     IxM.map (filter (`IxS.member` sources')) $ newerSources suite
    , bugs = IxM.filterWithKey (\k _ -> k `IxS.member` atoms') $ bugs suite
    }
  where sources' = IxS.filter (not . isTooYoung) $ sources suite
        binaries' = IxS.filter ((`IxS.member` sources') . (builtBy suite IxM.!)) $ binaries suite
        atoms' = IxS.generalize sources' `IxS.union` IxS.generalize binaries'

        isTooYoung src = case src `M.lookup` ages general of
            Just age -> let minAge = fromMaybe (defaultMinAge config) $
                                urgencies general `combine` minAges config $ src
                        in  age <= minAge
            Nothing -> False

transitionRules config ai unstable testing general =
    ( keepSrc ++ keepBin ++ uniqueBin ++ needsSource ++ needsBinary ++ releaseSync ++ completeBuild ++ outdated ++ obsolete ++ tooyoung ++ buggy
    , conflictClauses ++ dependencies
    , desired , unwanted )
  where relaxable = conflictClauses ++ dependencies
        keepSrc = 
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
        dependencies =
            {-# SCC "dependencies" #-}
            -- Dependencies
            [Implies (genIndex binI) deps ("the package depends on \"" ++ BS.unpack reason ++ "\".") |
                (binI,depends) <- IxM.toList dependsUnion,
                let Binary _ _ arch = ai `lookupBin` binI,
                (disjunction, reason) <- depends,
                let deps = map genIndex . nub . concatMap ({-# SCC "resolve" #-} resolve arch) $ disjunction
            ]
        conflictClauses =
            {-# SCC "conflictClauses" #-}
            -- Conflicts
            [NotBoth (genIndex binI) (genIndex confl) ("the package conflicts with \"" ++ BS.unpack reason ++ "\".") |
                (binI,depends) <- IxM.toList conflictsUnion,
                let Binary _ _ arch = ai `lookupBin` binI,
                (disjunction, reason) <- depends,
                rel@(DepRel _ (ST.Just vr) _ ) <- disjunction,
                hasUpperBound vr,
                confl <- nub (resolve arch rel)
            ] ++
            [NotBoth (genIndex binI) (genIndex confl) ("the package breaks on \"" ++ BS.unpack reason ++ "\".") |
                (binI,depends) <- IxM.toList breaksUnion,
                let Binary _ _ arch = ai `lookupBin` binI,
                (disjunction, reason) <- depends,
                rel@(DepRel _ (ST.Just vr) _ ) <- disjunction,
                -- hasUpperBound vr,
                confl <- nub (resolve arch rel)
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

        tooyoung = 
            {-# SCC "tooyoung" #-}
            -- packages need to be old enough
            [Not (genIndex src) ("it is " ++ show age ++ " days old, needs " ++ show minAge) |
                src <- IxM.keys buildsOnlyUnstable,
                Just age <- [src `M.lookup` ages general],
                let minAge = fromMaybe (defaultMinAge config) $
                             urgencies general `combine` minAges config $ src,
                age <= minAge
            ] 
        needsSource = 
            {-# SCC "needsSource" #-}
            -- a package needs its source
            [Implies (genIndex bin) [genIndex src] ("of the DFSG") |
                (bin, src) <- IxM.toList builtByUnion
            ]
        needsBinary =
            {-# SCC "needsBinary" #-}
            -- a source needs a binary
            [Implies (genIndex src) bins ("it were useless otherwise") |
                (src, binIs) <- IxM.toList buildsUnion,
                let bins = map genIndex (nub binIs)
            ]
        outdated = 
            {-# SCC "outdated" #-}
            -- release architectures ought not to be out of date
            [Not (genIndex newer) ("is out of date: " ++ show (ai `lookupBin` binI) ++ " exists in unstable") |
                (binI, src) <- IxM.toList (builtBy unstable),
                -- TODO: only release architecture here
                newer <- newerSources unstable IxM.! src,
                newer `IxS.notMember` sources testing
            ]
        obsolete = 
            {-# SCC "obsolete" #-}
            -- never add a source package to testing that is already superceded
            [Not (genIndex src) ("it is already superceded by " ++ show (ai `lookupSrc` s)) |
                (src, bins) <- IxM.toList buildsOnlyUnstable,
                (s:_) <- [newerSources unstable IxM.! src]
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

        -- Some duplication wrt above code
        obsoleteSource = IxS.fromList [ src |
                (src, bins) <- IxM.toList buildsOnlyUnstable,
                (s:_) <- [newerSources unstable IxM.! src]
            ]
        youngSource = IxS.fromList [ src |
                src <- IxM.keys buildsOnlyUnstable,
                Just age <- [src `M.lookup` ages general],
                let minAge = fromMaybe (defaultMinAge config) $
                             urgencies general `combine` minAges config $ src,
                age <= minAge
            ]

        desired  = fmap genIndex $ IxS.toList $
            sources unstable `IxS.difference` sources testing
                             `IxS.difference` obsoleteSource
                             `IxS.difference` youngSource
        unwanted = [] -- fmap genIndex $ IxS.toList $ sources testing `IxS.difference` sources unstable

        sourcesBoth = {-# SCC "sourcesBoth" #-} M.intersectionWith (++) (sourceNames unstable) (sourceNames testing)
        binariesBoth = {-# SCC "binariesBoth" #-} M.intersectionWith (++) (binaryNames unstable) (binaryNames testing)
        binariesUnion = {-# SCC "binariesUnion" #-} M.unionWith (++) (binaryNames unstable) (binaryNames testing)
        providesUnion = {-# SCC "providesUnion" #-} M.unionWith (++) (provides unstable) (provides testing)
        -- We assume that the dependency information is the same, even from different suites
        dependsUnion = {-# SCC "dependsUnion" #-} IxM.union (depends unstable) (depends testing)
        conflictsUnion = {-# SCC "conflictsUnion" #-} IxM.union (conflicts unstable) (conflicts testing)
        breaksUnion = {-# SCC "breaksUnion" #-} IxM.union (breaks unstable) (breaks testing)

        builtByUnion = {-# SCC "builtByUnion" #-} IxM.union (builtBy unstable) (builtBy testing)
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
        atomsOnlyUnstable = {-# SCC "atomsOnlyUnstable" #-} IxS.difference (atoms unstable) (atoms testing)

        resolve :: ST.Maybe Arch -> DepRel -> [BinI]
        resolve mbArch (DepRel name mbVerReq mbArchReq)
            | checkArchReq mbArchReq = 
                [ binI |
                    binI <- M.findWithDefault [] (name, arch) binariesUnion,
                    let Binary pkg version _ = ai `lookupBin` binI,
                    checkVersionReq mbVerReq (Just version)
                ] ++ 
                if ST.isJust mbVerReq then [] else 
                [ binI |
                    binI <- M.findWithDefault [] (name, arch) providesUnion
                ]
            | otherwise = []
          where arch = ST.fromMaybe (archForAll config) mbArch 
                checkArchReq ST.Nothing = True
                checkArchReq (ST.Just (ArchOnly arches)) = arch `elem` arches
                checkArchReq (ST.Just (ArchExcept arches)) = arch `notElem` arches

-- |Check if a version number satisfies a version requirement.
checkVersionReq :: ST.Maybe VersionReq -> Maybe DebianVersion -> Bool
checkVersionReq ST.Nothing _ = True
checkVersionReq _ Nothing = False
checkVersionReq (ST.Just (SLT v1)) (Just v2) =
    v2 `cmpDebianVersion` v1 == LT
checkVersionReq (ST.Just (LTE v1)) (Just v2) =
    v2 `cmpDebianVersion` v1 <= EQ
checkVersionReq (ST.Just (EEQ v1)) (Just v2) =
    v2 `cmpDebianVersion` v1 == EQ
checkVersionReq (ST.Just (GRE v1)) (Just v2) =
    v2 `cmpDebianVersion` v1 >= EQ
checkVersionReq (ST.Just (SGR v1)) (Just v2) =
    v2 `cmpDebianVersion` v1 == GT

    

combine :: (Ord a, Ord b) => M.Map a b -> M.Map b c -> a -> Maybe c
combine m1 m2 x = (x `M.lookup` m1) >>= (`M.lookup` m2)
