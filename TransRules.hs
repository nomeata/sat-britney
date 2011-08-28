{-# LANGUAGE RecordWildCards #-}
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

import Types
import LitSat
import qualified IndexSet as IxS
import qualified IndexMap as IxM

thinSuite :: Config -> SuiteInfo -> RawPackageInfo -> GeneralInfo -> (SuiteInfo, RawPackageInfo)
thinSuite config suite rawPackageInfo general = (SuiteInfo
    { sources = sources'
    , binaries = binaries'
    , atoms = atoms'
    , sourceNames =  M.map (filter (`IxS.member` sources'))  $ sourceNames suite
    , binaryNames =  M.map (filter (`IxS.member` binaries')) $ binaryNames suite
    , builds =       filterKeySrc $ builds suite
    , newerSources = filterKeySrc $
                     IxM.map (filter (`IxS.member` sources')) $ newerSources suite
    , bugs =         filterKeyAtoms $ bugs suite
    }, RawPackageInfo
    { providesR =    M.map (filter (`IxS.member` binaries')) $ providesR rawPackageInfo
    , breaksR =      filterKeyBin $ breaksR rawPackageInfo
    , builtByR =     filterKeyBin $ builtByR rawPackageInfo
    , dependsR =     filterKeyBin $ dependsR rawPackageInfo
    , conflictsR =   filterKeyBin $ conflictsR rawPackageInfo
    , binaryNamesR = M.map (filter (`IxS.member` binaries')) $ binaryNamesR rawPackageInfo
    })
  where sources' = IxS.filter (not . isTooYoung) $ sources suite
        binaries' = IxS.filter ((`IxS.member` sources') . (builtByR rawPackageInfo IxM.!)) $ binaries suite
        atoms' = IxS.generalize sources' `IxS.union` IxS.generalize binaries'

        isTooYoung src = case src `M.lookup` ages general of
            Just age -> let minAge = fromMaybe (defaultMinAge config) $
                                urgencies general `combine` minAges config $ src
                        in  age <= minAge
            Nothing -> False

        filterKeyBin = IxM.filterWithKey (\k _ -> k `IxS.member` binaries') 
        filterKeySrc = IxM.filterWithKey (\k _ -> k `IxS.member` sources') 
        filterKeyAtoms = IxM.filterWithKey (\k _ -> k `IxS.member` atoms') 

resolvePackageInfo :: Config -> AtomIndex -> [RawPackageInfo] -> PackageInfo
resolvePackageInfo config ai rawPackageInfos = PackageInfo{..}
  where builtBy = IxM.unions $ map builtByR rawPackageInfos
        
        depends = IxM.mapWithKey 
                    (\binI -> let Binary _ _ arch = ai `lookupBin` binI
                              in map $ first $ nub . concatMap (resolveDep arch))
                    (IxM.unions (map dependsR rawPackageInfos))

        dependsRel = IxM.filter (not . IxS.null) $
                     IxM.map (IxS.fromList . concatMap fst) $
                     depends

        revDependsRel = reverseRel dependsRel

        dependsHull = transitiveHull dependsRel

        conflicts = IxM.unionWith (++)
                    ( IxM.mapWithKey
                        (\binI -> let Binary pkg _ arch = ai `lookupBin` binI
                                  in map $ first $ nub . concatMap (resolveConf pkg arch)
                                                       -- . filter depRelHasUpperBound
                        )
                        (IxM.unions (map conflictsR rawPackageInfos))
                    )
                    ( IxM.mapWithKey
                        (\binI -> let Binary pkg _ arch = ai `lookupBin` binI
                                  in map $ first $ nub . concatMap (resolveConf pkg arch)
                        )
                        (IxM.unions (map breaksR rawPackageInfos))
                    )

        depRelHasUpperBound (DepRel _ (ST.Just vr) _ ) = hasUpperBound vr
        depRelHasUpperBound _ = False

        flatConflicts :: IxM.Map Binary (IxS.Set Binary)
        flatConflicts = IxM.filter (not . IxS.null) $
                        IxM.map (IxS.fromList . concatMap fst) $
                        conflicts

        conflictsRel :: IxM.Map Binary (IxS.Set Binary)
        conflictsRel = IxM.unionWith (IxS.union) flatConflicts (reverseRel flatConflicts)

        hasConflict = IxM.keysSet conflictsRel

        hasConflictInDeps = go hasConflict IxS.empty
          where go new cid | IxS.null new = cid
                           | otherwise = 
                    let new' = IxS.unions $ mapMaybe (`IxM.lookup` revDependsRel) $ IxS.toList new
                        cid' = cid `IxS.union` new
                    in  go (new' `IxS.difference` cid') cid'

        hasBadConflictInDeps = flip IxS.filter hasConflictInDeps $ \p -> 
            let deps  = IxM.findWithDefault IxS.empty p dependsHull
                other = IxS.unions $
                        mapMaybe (`IxM.lookup` conflictsRel) $
                        IxS.toList deps 
            in  not $ IxS.null $ other `IxS.intersection` deps

        binaryNamesUnion = M.unionsWith (++) (map binaryNamesR rawPackageInfos)

        providesUnion = M.unionsWith (++) (map providesR rawPackageInfos)

        resolveDep  = resolve Nothing
        resolveConf = resolve . Just 

        resolve :: Maybe BinName -> ST.Maybe Arch -> DepRel -> [BinI]
        resolve mbPkg mbArch (DepRel name mbVerReq mbArchReq)
            | checkArchReq mbArchReq = 
                [ binI |
                    binI <- M.findWithDefault [] (name, arch) binaryNamesUnion,
                    let Binary pkg version _ = ai `lookupBin` binI,
                    maybe True (/= pkg) mbPkg, -- conflicts only with different names
                    checkVersionReq mbVerReq (Just version)
                ] ++ 
                if ST.isJust mbVerReq then [] else 
                [ binI |
                    binI <- M.findWithDefault [] (name, arch) providesUnion,
                    let Binary pkg _ _ = ai `lookupBin` binI,
                    maybe True (/= pkg) mbPkg -- conflicts only with different names
                ]
            | otherwise = []
          where arch = ST.fromMaybe (archForAll config) mbArch 
                checkArchReq ST.Nothing = True
                checkArchReq (ST.Just (ArchOnly arches)) = arch `elem` arches
                checkArchReq (ST.Just (ArchExcept arches)) = arch `notElem` arches

-- Could be implemented better
transitiveHull rel = IxM.fromList $
    [ (p,d) | (p,_) <- IxM.toList rel, let d = execState (deps p) IxS.empty ]
  where deps d = do
            ex <- gets (d `IxS.member`)
            unless ex $ do
                modify (IxS.insert d)
                mapM_ deps $ IxS.toList (IxM.findWithDefault IxS.empty d rel)

reverseRel rel = foldr (uncurry (IxM.insertWith IxS.union)) IxM.empty $ 
               [ (x1, IxS.singleton x2) |
                    (x2,x1S) <- IxM.toList rel,
                    x1 <- IxS.toList x1S
               ]

generateInstallabilityAtoms :: PackageInfo -> AtomIndex -> AtomIndex
generateInstallabilityAtoms pi ai =
    IxM.foldWithKey
        (\p s ai -> IxS.fold (\d -> fst . (`addInst` Inst p d)) ai s) ai $
        IxM.filterWithKey (\k _ -> k `IxS.member` hasBadConflictInDeps pi) $
        dependsHull pi


transitionRules
  :: Config -> AtomIndex -> SuiteInfo -> SuiteInfo -> GeneralInfo -> PackageInfo
     -> ([Clause AtomI], [Clause AtomI], [AtomI], [AtomI])
transitionRules config ai unstable testing general pi =
    ( keepSrc ++ keepBin ++ uniqueBin ++ needsSource ++ needsBinary ++ releaseSync ++ completeBuild ++ outdated ++ obsolete ++ tooyoung ++ buggy ++ hardDependencies
    , conflictClauses ++ softDependencies
    , desired
    , unwanted
    )
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
        softDependencies | fullDependencies config =
            [Implies (genIndex forI) [instI] "the package ought to be installable." |
                forI <- IxS.toList binariesUnion,
                forI `IxS.member` hasBadConflictInDeps pi,
                let instI = genIndex . fromJustNote "Z" . indexInst ai . Inst forI $ forI
            ] ++
            [Implies (genIndex binI) deps ("the package depends on \"" ++ BS.unpack reason ++ "\".") |
                (binI,depends) <- IxM.toList (depends pi),
                binI `IxS.notMember` hasBadConflictInDeps pi,
                (disjunction, reason) <- depends,
                let deps = map genIndex disjunction
            ]
                         | otherwise = 
            -- Any package needs to be installable
            [Implies (genIndex binI) deps ("the package depends on \"" ++ BS.unpack reason ++ "\".") |
                (binI,depends) <- IxM.toList (depends pi),
                (disjunction, reason) <- depends,
                let deps = map genIndex disjunction
            ]
        hardDependencies | fullDependencies config =
            {-# SCC "dependencies" #-}
            -- Dependencies
            [ Implies instI deps ("the package depends on \"" ++ BS.unpack reason ++ "\".") |
                (forI,binIs) <- IxM.toList (dependsHull pi),
                forI `IxS.member` hasBadConflictInDeps pi,
                binI <- IxS.toList binIs,
                let instI = genIndex . fromJustNote "Y" . indexInst ai . Inst forI $ binI,
                (disjunction, reason) <- depends pi IxM.! binI,
                let deps = map (genIndex . fromJustNote "X" . indexInst ai . Inst forI) disjunction
            ]
                         | otherwise = []
        conflictClauses =
            {-# SCC "conflictClauses" #-}
            -- Conflicts
            [NotBoth (genIndex binI) (genIndex confl) ("the package conflicts with \"" ++ BS.unpack reason ++ "\".") |
                (binI,depends) <- IxM.toList (conflicts pi),
                (disjunction, reason) <- depends,
                confl <- disjunction
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
                (bin, src) <- IxM.toList (builtBy pi)
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
                binI <- IxS.toList (binaries unstable),
                let srcI = builtBy pi IxM.! binI,
                -- TODO: only release architecture here
                newer <- newerSources unstable IxM.! srcI,
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

        binariesUnion = binaries testing `IxS.union` binaries unstable

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
        atomsOnlyUnstable = {-# SCC "atomsOnlyUnstable" #-} IxS.difference (atoms unstable) (atoms testing)

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
