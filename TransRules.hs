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
import qualified Data.Set as S
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


resolvePackageInfo :: Config -> AtomIndex -> IxS.Set Source -> [SuiteInfo] -> [RawPackageInfo] -> PackageInfo
resolvePackageInfo config ai nonCandidates sis rawPackageInfos = PackageInfo{..}
  where builtBy = IxM.unions $ map builtByR rawPackageInfos

        buildsUnion = {-# SCC "buildsUnion" #-} IxM.unionsWith (++) $ map builds sis

        nonCandidateBins = IxS.seal $ IxS.fromList $
            concatMap (buildsUnion IxM.!) $
            IxS.toList nonCandidates
        
        depends = {-# SCC "depends" #-} IxM.mapWithKey 
                    (\binI -> let Binary _ _ arch = ai `lookupBin` binI
                              in map $ first $
                                filter (`IxS.notMember` nonCandidateBins) .
                                nub .
                                concatMap (resolveDep arch)
                    ) $
                    IxM.unions $
                    map (IxM.filterWithKey $ \k v -> 
                            k `IxS.notMember` nonCandidateBins
                        ) $
                    map dependsR rawPackageInfos

        dependsRel = {-# SCC "dependsRel" #-} IxM.filter (not . IxS.null) $
                     IxM.map (IxS.fromList . concatMap fst) $
                     depends

        dependsRelWithConflicts = {-# SCC "dependsRelWithConflicts" #-}
            IxM.map (IxS.filter (`IxS.member` hasConflictInDepsProp)) dependsRel

        revDependsRel = {-# SCC "revDependsRel" #-} reverseRel dependsRel

        -- dependsHull = transitiveHull dependsRel

        {-
        dependsBadHull = {-# SCC "dependsBadHull" #-} IxM.fromList
                [ (p,transitiveHull1 dependsRelWithConflicts p)
                | (p,_) <- IxM.toList dependsRelWithConflicts
                , p `IxS.member` hasReallyBadConflictInDeps]
        -}

        relevantConflicts = {-# SCC "relevantConflicts" #-} 
            IxM.filter (not . S.null) $
            flip IxM.mapWithKey depends $
                \p deps -> S.fromList $
                    [ (c1,c2)
                    | (deps1,deps2s) <- allPairs' $
                        IxS.singleton p : map (
                            IxS.unions .
                            map (transitiveHull1 dependsRelWithConflicts) .
                            filter (`IxS.member` hasConflictInDepsProp) .
                            fst
                        ) deps
                    , c1 <- IxS.toList $ deps1 `IxS.intersection` (IxM.keysSet conflictsRel)
                    , let c2s = conflictsRel IxM.! c1
                    , not $ IxS.null c2s
                    , dep2s <- deps2s
                    , c2 <- IxS.toList $ c2s `IxS.intersection` dep2s
                    ]

        dependsRelBad = {-# SCC "dependsRelBad" #-} 
            IxM.filter (not . IxS.null) $
            flip IxM.mapWithKey dependsRelWithConflicts $
                \p deps -> IxS.fromList $
                    concat [ [d1, d2]
                    | (d1,d2) <- allPairs (IxS.toList deps)
                    , let deps1 = transitiveHull1 dependsRelWithConflicts d1
                          deps2 = {-# SCC "deps2" #-} transitiveHull1 dependsRelWithConflicts d2
                          other1 = {-# SCC "other1" #-} IxS.unions $ mapMaybe (`IxM.lookup` conflictsRel) $ IxS.toList deps1
                          other2 = {-# SCC "other2" #-} IxS.unions $ mapMaybe (`IxM.lookup` conflictsRel) $ IxS.toList deps2
                    , not $ IxS.null $ other1 `IxS.intersection` deps2
                    , not $ IxS.null $ other2 `IxS.intersection` deps1 -- Obsolete by symmetry?
                    ] ++
                    [ d1
                    | d1 <- IxS.toList deps
                    , let deps1 = transitiveHull1 dependsRelWithConflicts d1
                          other1 = {-# SCC "other1" #-} IxS.unions $ mapMaybe (`IxM.lookup` conflictsRel) $ IxS.toList deps1
                    , p `IxS.member` other1
                    ]

        {-
        dependsBadHull = {-# SCC "dependsBadHull" #-} IxM.fromList
                [ (p,p `IxS.insert` depsHull)
                | (p,deps) <- IxM.toList dependsRelBad
                , let depsHull = IxS.unions $
                        map (transitiveHull1 dependsRelWithConflicts) $
                        IxS.toList deps
                , not (IxS.null depsHull)
                ]
        -}

        dependsBadHull = {-# SCC "dependsBadHull" #-} IxM.fromListWith IxS.union
                [ (p,depsHull)
                | (p,conflicts) <- IxM.toList relevantConflicts
                , let deps = transitiveHull1 dependsRelWithConflicts p
                , let revDependsRel' = restrictRel revDependsRel deps
                , (c1,c2) <- S.toList conflicts
                , let depsHull =
                        (transitiveHull1 revDependsRel' c1 `IxS.intersection` deps) `IxS.union`
                        (transitiveHull1 revDependsRel' c2 `IxS.intersection` deps)
                , if p `IxS.member` depsHull then True else error "p not in depsHull"
                ]


        conflicts = {-# SCC "conflicts" #-} IxM.unionWith (++)
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

        depRelHasUpperBound (DepRel _ (ST.Just vr) _ ) = {-# SCC "depRelHasUpperBound" #-} hasUpperBound vr
        depRelHasUpperBound _ = {-# SCC "depRelHasUpperBound" #-} False

        flatConflicts :: IxM.Map Binary (IxS.Set Binary)
        flatConflicts = {-# SCC "flatConflicts" #-} IxM.filter (not . IxS.null) $
                        IxM.map (IxS.fromList . concatMap fst) $
                        conflicts

        conflictsRel :: IxM.Map Binary (IxS.Set Binary)
        conflictsRel = {-# SCC "conflictsRel" #-} IxM.unionWith (IxS.union) flatConflicts (reverseRel flatConflicts)

        hasConflict = {-# SCC "hasConflict" #-} IxM.keysSet conflictsRel

        hasConflictInDepsProp = {-# SCC "hasConflictInDepsProp" #-} IxS.seal hasConflictInDeps
        hasConflictInDeps = {-# SCC "hasConflictInDeps" #-} go hasConflict IxS.empty
          where go new cid | IxS.null new = {-# SCC "where" #-} cid
                           | otherwise = 
                    let new' = {-# SCC "let" #-} IxS.unions $ mapMaybe (`IxM.lookup` revDependsRel) $ IxS.toList new
                        cid' = {-# SCC "cid" #-} cid `IxS.union` new
                    in  go (new' `IxS.difference` cid') cid'

        hasBadConflictInDeps = {-# SCC "hasBadConflictInDeps" #-} flip IxS.filter hasConflictInDeps $ \p -> 
            let deps  = {-# SCC "let" #-} transitiveHull1 dependsRel p
                other = {-# SCC "other" #-} IxS.unions $
                        mapMaybe (`IxM.lookup` conflictsRel) $
                        IxS.toList deps 
            in  not $ IxS.null $ other `IxS.intersection` deps

        hasReallyBadConflictInDeps = {-# SCC "hasReallyBadConflictInDeps" #-} flip IxS.filter hasBadConflictInDeps $ \p -> 
            not $ null [ ()
                | ((d1,_),(d2,_)) <- allPairs (depends IxM.! p)
                , let deps1 = IxS.unions $ map (transitiveHull1 dependsRel) d1
                      deps2 = {-# SCC "deps2" #-} IxS.unions $ map (transitiveHull1 dependsRel) d2
                      other1 = {-# SCC "other1" #-} IxS.unions $ mapMaybe (`IxM.lookup` conflictsRel) $ IxS.toList deps1
                      other2 = {-# SCC "other2" #-} IxS.unions $ mapMaybe (`IxM.lookup` conflictsRel) $ IxS.toList deps2
                , not $ IxS.null $ other1 `IxS.intersection` deps2
                , not $ IxS.null $ other2 `IxS.intersection` deps1 -- Obsolete by symmetry?
                ] && null
                [ ()
                | (d1,_) <- depends IxM.! p
                , let deps1 = IxS.unions $ map (transitiveHull1 dependsRel) d1
                      other1 = {-# SCC "other1" #-} IxS.unions $ mapMaybe (`IxM.lookup` conflictsRel) $ IxS.toList deps1
                , not $ p `IxS.member` other1
                ]
                    
        binaryNamesUnion = {-# SCC "binaryNamesUnion" #-} M.unionsWith (++) (map binaryNamesR rawPackageInfos)

        providesUnion = {-# SCC "providesUnion" #-} M.unionsWith (++) (map providesR rawPackageInfos)

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

allPairs :: [a] -> [(a,a)]
allPairs [] = []
allPairs (x:xs) = [ (x,y) | y <- xs ] ++ allPairs xs

allPairs' :: [a] -> [(a,[a])]
allPairs' [] = []
allPairs' (x:xs) = (x,xs) : allPairs' xs

-- Could be implemented better
transitiveHull ::  IxM.Map a1 (IxS.Set a1) -> IxM.Map a (IxS.Set a1)
transitiveHull rel = IxM.fromList $ [ (p,transitiveHull1 rel p) | (p,_) <- IxM.toList rel]

transitiveHull1 :: IxM.Map a (IxS.Set a) -> Index a -> IxS.Set a
transitiveHull1 rel x = addTransitiveHull1 rel x IxS.empty

addTransitiveHull1 :: IxM.Map t (IxS.Set t) -> Index t -> IxS.Set t -> IxS.Set t
addTransitiveHull1 rel x = execState (deps x)
  where deps d = do
            ex <- gets (d `IxS.member`)
            unless ex $ do
                modify (IxS.insert d)
                mapM_ deps $ IxS.toList (IxM.findWithDefault IxS.empty d rel)

transitiveHull1s :: IxM.Map t (IxS.Set t) -> [Index t] -> IxS.Set t
transitiveHull1s rel = foldr (addTransitiveHull1 rel) IxS.empty 
{-# RULES "unions/transitiveHull1" 
    forall rel xs . IxS.unions (map (transitiveHull1 rel) xs) = transitiveHull1s rel xs #-}
{-# RULES "unions/./transitiveHull1" 
    forall rel . IxS.unions . map (transitiveHull1 rel) = transitiveHull1s rel #-}

reverseRel rel = foldr (uncurry (IxM.insertWith IxS.union)) IxM.empty $ 
               [ (x1, IxS.singleton x2) |
                    (x2,x1S) <- IxM.toList rel,
                    x1 <- IxS.toList x1S
               ]

restrictRel :: IxM.Map a (IxS.Set a) -> IxS.Set a -> IxM.Map a (IxS.Set a)
restrictRel rel set = IxM.fromAscList $
    flip mapMaybe (IxS.toAscList set) $ \k ->
        (k,) . IxS.filter (`IxS.member` set) <$> IxM.lookup k rel

generateInstallabilityAtoms :: Config -> PackageInfo -> AtomIndex -> AtomIndex
generateInstallabilityAtoms config pi ai =
    if not (fullDependencies config) then ai else
    foldl' (\ai (p,s) -> 
        foldl' (\ ai d -> fst (ai `addInst` Inst p d)) ai (IxS.toList s)
    ) ai $
    IxM.toList (dependsBadHull pi)

-- Sources and binaries that will not be in testing, in any case. Can be used
-- to skip certain things, most notable generating the dependency information.
findNonCandidates :: Config -> AtomIndex -> SuiteInfo -> SuiteInfo -> GeneralInfo -> PackageInfo -> HintResults
    -> Producer (SrcI, String)
findNonCandidates config ai unstable testing general pi hr f x =
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
                let srcI = builtBy pi IxM.! binI,
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


-- Wrapper around transitionRules' that prevents sharing. We _want_ to
-- recalculate the rules everytime 'build' is called upon the producer.
transitionRules :: Config -> AtomIndex -> SuiteInfo -> SuiteInfo -> GeneralInfo -> PackageInfo -> Producer (SrcI, String)
     -> (Producer (Clause AtomI), Producer (Clause AtomI), Producer AtomI, Producer AtomI)
transitionRules config ai unstable testing general pi nc =
    ( hardTransitionRules config ai unstable testing general pi nc 
    , softTransitionRules config ai unstable testing general pi nc 
    , desiredAtoms config ai unstable testing general pi nc 
    , unwantedAtoms config ai unstable testing general pi nc 
    )

hardTransitionRules 
  :: Config -> AtomIndex -> SuiteInfo -> SuiteInfo -> GeneralInfo -> PackageInfo -> Producer (SrcI, String)
     -> Producer (Clause AtomI)
hardTransitionRules config ai unstable testing general pi nc f x =
    let (r,_,_,_) = transitionRules' config ai unstable testing general pi nc
    in r f x

softTransitionRules 
  :: Config -> AtomIndex -> SuiteInfo -> SuiteInfo -> GeneralInfo -> PackageInfo -> Producer (SrcI, String)
     -> Producer (Clause AtomI)
softTransitionRules config ai unstable testing general pi nc f x =
    let (_,r,_,_) = transitionRules' config ai unstable testing general pi nc
    in r f x

desiredAtoms 
  :: Config -> AtomIndex -> SuiteInfo -> SuiteInfo -> GeneralInfo -> PackageInfo -> Producer (SrcI, String)
      -> Producer AtomI
desiredAtoms config ai unstable testing general pi nc f x =
    let (_,_,a,_) = transitionRules' config ai unstable testing general pi nc
    in a f x

unwantedAtoms 
  :: Config -> AtomIndex -> SuiteInfo -> SuiteInfo -> GeneralInfo -> PackageInfo -> Producer (SrcI, String)
      -> Producer AtomI
unwantedAtoms config ai unstable testing general pi nc f x =
    let (_,_,_,a) = transitionRules' config ai unstable testing general pi nc
    in a f x


transitionRules'
  :: Config -> AtomIndex -> SuiteInfo -> SuiteInfo -> GeneralInfo -> PackageInfo -> Producer (SrcI, String)
     -> (Producer (Clause AtomI), Producer (Clause AtomI), Producer AtomI, Producer AtomI)
transitionRules' config ai unstable testing general pi nc =
    if fullDependencies config then
    ( toProducer $ keepSrc ++ keepBin ++ uniqueBin ++ needsSource ++ needsBinary ++ releaseSync ++ completeBuild ++ nonCandidates ++ buggy ++ hardDependenciesFull
    , toProducer $ softDependenciesFull
    , toProducer $ desired
    , toProducer $ unwanted
    )
    else error "unsupported" {-
    ( toProducer $ keepSrc ++ keepBin ++ uniqueBin ++ needsSource ++ needsBinary ++ releaseSync ++ completeBuild ++ outdated ++ obsolete ++ tooyoung ++ buggy
    , toProducer $ conflictClauses ++ softDependenciesNonFull
    , desired
    , unwanted
    ) -}
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
        softDependenciesFull =
            [Implies (genIndex forI) [instI] "the package ought to be installable." |
                forI <- IxS.toList binariesUnion,
                forI `IxM.member` dependsBadHull pi,
                let instI = genIndex . fromJustNote "X" . indexInst ai . Inst forI $ forI
            ] ++
            [Implies (genIndex binI) deps ("the package depends on \"" ++ BS.unpack reason ++ "\".") |
                (binI,depends) <- IxM.toList (depends pi),
                binI `IxM.notMember` dependsBadHull pi,
                (disjunction, reason) <- depends,
                let deps = map genIndex disjunction
            ]
        softDependenciesNonFull =
            -- Any package needs to be installable
            [Implies (genIndex binI) deps ("the package depends on \"" ++ BS.unpack reason ++ "\".") |
                (binI,depends) <- IxM.toList (depends pi),
                (disjunction, reason) <- depends,
                let deps = map genIndex disjunction
            ]
        hardDependenciesFull =
            {-# SCC "dependencies" #-}
            -- Dependencies
            [ Implies instI deps ("the package depends on \"" ++ BS.unpack reason ++ "\".") |
                (forI,binIs) <- IxM.toList (dependsBadHull pi),
                binI <- IxS.toList binIs,
                let instI = genIndex . fromJustNote "Y" . indexInst ai . Inst forI $ binI,
                (disjunction, reason) <- depends pi IxM.! binI,
                let deps = [ case indexInst ai (Inst forI depI) of
                                Just instI -> genIndex instI
                                Nothing    -> genIndex depI
                           | depI <- disjunction ]
            ] ++
            [ NotBoth instI conflI ("the package conflicts with \"" ++ BS.unpack reason ++ "\".") |
                (forI,binIs) <- IxM.toList (dependsBadHull pi),
                binI <- IxS.toList binIs,
                let instI = genIndex . fromJustNote "Y" . indexInst ai . Inst forI $ binI,
                (disjunction, reason) <- conflicts pi IxM.! binI,
                confl <- disjunction,
                confl `IxS.member` binIs,
                let conflI = genIndex . fromJustNote "Z" . indexInst ai . Inst forI $ confl
            ] ++
            [ Implies instI [genIndex binI] "the package needs to be present" |
                (forI,binIs) <- IxM.toList (dependsBadHull pi),
                binI <- IxS.toList binIs,
                let instI = genIndex . fromJustNote "Y" . indexInst ai . Inst forI $ binI
            ]
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
        unwanted = fmap genIndex $ IxS.toList $ sources testing `IxS.difference` sources unstable

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
