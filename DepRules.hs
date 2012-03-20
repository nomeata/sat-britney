{-# LANGUAGE RecordWildCards, Rank2Types, ImpredicativeTypes, TupleSections #-}
-- |
-- Module: DepRules
-- Copyright: (c) 2011 Joachim Breitner
-- License: GPL-2
--
module DepRules where

import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.ByteString.Char8 as BS
import qualified Data.Strict as ST
import qualified Data.Map as M
import Data.Functor
import Data.Function
import Control.Arrow ((&&&), first)
import Debug.Trace
import Safe
import GHC.Exts (build)

import ParseHints
import Indices
import Types
import Arches
import AtomIndex
import LitSat
import PrettyPrint
import qualified Data.Set as S
import qualified IndexSet as IxS
import qualified IndexMap as IxM
import qualified ArchMap as AM


buildArchMap :: Config -> (Arch -> a) -> AM.Map a
buildArchMap config f = AM.fromList [ (a, f a) | a <- arches config ]

resolvePackageInfo :: Config -> AtomIndex -> IxS.Set Source -> IxS.Set Binary -> [SuiteInfo] -> [RawPackageInfo] -> PackageInfo
resolvePackageInfo config ai nonCandidates unmod sis rawPackageInfos = PackageInfoOut{..}
  where buildsUnion = {-# SCC "buildsUnion" #-} IxM.unionsWith (++) $ map builds sis

        binariesUnion = {-# SCC "binariesUnion" #-} IxS.unions $ map binaries sis

        affected = {-# SCC "affected" #-} IxS.unions . map (transitiveHull1 revDependsRel) $ IxS.toList $ binariesUnion `IxS.difference` unmod

        nonCandidateBins = {-# SCC "nonCandidateBins" #-} IxS.seal $ IxS.fromList $
            concatMap (buildsUnion IxM.!) $
            IxS.toList nonCandidates
        
        depends = {-# SCC "depends" #-} buildArchMap$ \arch ->
            IxM.fromList [(binI, map (first
                            (filter (`IxS.notMember` nonCandidateBins) .
                            nub .
                            concatMap (resolveDep arch)) )
                            deps) |
                (binI, deps) <- concatMap dependsR rawPackageInfos,
                -- Consider only candidates
                binI `IxS.notMember` nonCandidateBins,
                -- Consider arch:all packages or packages from this architecture
                let Binary _ _ archMB = ai `lookupBin` binI,
                ST.maybe True (== arch) archMB
                ]


        dependsRel = {-# SCC "dependsRel" #-} AM.map
                    (IxM.filter (not . IxS.null) .  IxM.map (IxS.fromList . concatMap fst))
                    depends

        dependsRelWithConflicts = {-# SCC "dependsRelWithConflicts" #-}
            IxM.map (IxS.filter (`IxS.member` hasConflictInDepsProp)) dependsRel

        dependsRelWithConflictsHull = {-# SCC "dependsRelWithConflictsHull" #-}
            transitiveHull dependsRelWithConflicts

        revDependsRel = {-# SCC "revDependsRel" #-} reverseRel dependsRel

        revDependsHull = {-# SCC "revDependsHull" #-}
            transitiveHull revDependsRel

        -- dependsHull = transitiveHull dependsRel
        
        conflictHistogram = {-# SCC "conflictHistogram" #-}
            reverse $
            sortBy (comparing snd) $
            M.toList $ 
            M.unionsWith (+) $
            map (\(_,cs) ->
                M.fromListWith (+) $
                map (\c -> (c,1)) $
                filter (\(c1,c2) -> c1 < c2) $
                S.toList cs
            ) $
            IxM.toList relevantConflicts

        relevantDepHistogram = {-# SCC "relevantDepHistogram" #-}
            reverse $
            sortBy (comparing snd) $
            M.toList $ 
            M.unionsWith (+) $
            map (\(_,deps) ->
                M.fromListWith (+) $
                map (\c -> (c,1)) $
                IxS.toList deps
            ) $
            IxM.toList dependsBadHull

        relevantConflicts = {-# SCC "relevantConflicts" #-} 
            -- Here we throw out the packages that are not affected by the
            -- transition. Only affected packages can have relevant conflicts;
            -- but even packages that are not affected can contribute a
            -- relevant conflict.
            IxM.filterWithKey (\p cs -> p `IxS.member` affected && not (S.null cs)) $
            flip IxM.mapWithKey dependsRelWithConflictsHull $ \p deps ->
                S.fromList
                    [ (c1,c2)
                    | c1 <- IxS.toList $ deps `IxS.intersection` hasConflict
                    , let c2s = conflictsRel IxM.! c1
                    , c2 <- IxS.toList $ c2s `IxS.intersection` deps
                    ]

        {- Broken attempt to only consider conflicts as relevant that span dependencies -}
        {-
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
        -}

        dependsBadHull = {-# SCC "dependsBadHull" #-}
            IxM.map IxS.seal $
            IxM.unionWith IxS.union (IxM.mapWithKey (\p _ -> IxS.singleton p) depends) $
            IxM.filter (not . IxS.null) $
            flip IxM.mapWithKey relevantConflicts $ \p relConfs ->
                let deps = dependsRelWithConflictsHull IxM.! p
                    revDependsRel' = restrictRel revDependsRel deps
                in IxS.unions [ hull
                    | (c1,c2) <- S.toList relConfs
                    , c1 < c2
                    , let hull =
                            (IxM.findWithDefault IxS.empty c1 revDependsHull `IxS.intersection` deps) `IxS.union`
                            (IxM.findWithDefault IxS.empty c2 revDependsHull `IxS.intersection` deps)
                    -- , traceShow (pp ai p, pp ai c1, pp ai c2) True
                    -- , traceShow (map (pp ai) $ IxS.toList hull) True
                    , if p `IxS.member` hull then True else error "p not in depsHull"
                    ]


        conflicts = {-# SCC "conflicts" #-} IxM.unionWith (++)
                    ( IxM.fromList $
                        map (\(binI,deps) ->
                            let Binary pkg _ arch = ai `lookupBin` binI
                            in (binI, map (first $ nub . concatMap (resolveConf pkg arch)) deps)
                                                       -- . filter depRelHasUpperBound
                        ) $
                        concatMap conflictsR rawPackageInfos
                    )
                    ( IxM.fromList $
                        map (\(binI,deps) ->
                            let Binary pkg _ arch = ai `lookupBin` binI
                            in (binI, map (first $ nub . concatMap (resolveConf pkg arch)) deps)
                        ) $
                        concatMap breaksR rawPackageInfos
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
        hasConflictInDeps = {-# SCC "hasConflictInDeps" #-}
            go hasConflict IxS.empty
          where go new cid | IxS.null new = {-# SCC "where" #-} cid
                           | otherwise = 
                    let new' = IxS.unions $ mapMaybe (`IxM.lookup` revDependsRel) $ IxS.toList new
                        cid' =  cid `IxS.union` new
                    in  go (new' `IxS.difference` cid') cid'

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
transitiveHull ::  IxM.Map a (IxS.Set a) -> IxM.Map a (IxS.Set a)
transitiveHull rel = IxM.mapWithKey (\p _ -> transitiveHull1 rel p) rel

transitiveHull1 :: IxM.Map a (IxS.Set a) -> Index a -> IxS.Set a
transitiveHull1 rel x = addTransitiveHull1 rel x IxS.empty

addTransitiveHull1 :: IxM.Map t (IxS.Set t) -> Index t -> IxS.Set t -> IxS.Set t
addTransitiveHull1 rel x hull = go hull [x]
  where go hull [] = hull
        go hull (x:xs) | x `IxS.member` hull = go hull xs
                       | otherwise = go (IxS.insert x hull) 
                                        (maybe [] IxS.toList (IxM.lookup x rel) ++ xs)

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
restrictRel rel set = {-# SCC "restrictRel" #-} IxM.fromAscList $
    flip mapMaybe (IxS.toAscList set) $ \k ->
        (k,) . IxS.filter (`IxS.member` set) <$> IxM.lookup k rel

initialInstallabilityAtoms :: Config -> PackageInfo -> AtomIndex -> [Inst]
initialInstallabilityAtoms config pi ai = [
    Inst binI binI a |
    binI <- IxM.keys (depends pi),
    let Binary _ _ a' = ai `lookupBin` binI,
    a <- ST.maybe (arches config) (:[]) a'
    ]

installabilityAtoms :: Config -> PackageInfo -> AtomIndex -> [Inst]
installabilityAtoms config pi ai =
    [ Inst p d a |
    Inst p _ a <- initialInstallabilityAtoms config pi ai,
    d <- maybe [p] IxS.toList $ IxM.lookup p (dependsBadHull pi)
    ]

generateInstallabilityAtoms :: Config -> PackageInfo -> AtomIndex -> AtomIndex
generateInstallabilityAtoms config pi ai =
    foldl' (\ai i ->  fst (ai `addInst` i)) ai $ installabilityAtoms config pi ai

instForItself :: Config -> AtomIndex -> BinI -> Inst
instForItself config ai binI = 
    let Binary _ _ a' = ai `lookupBin` binI
        a = ST.fromMaybe (archForAll config) a'
    in  Inst binI binI a

instIForItself :: Config -> AtomIndex -> BinI -> InstI
instIForItself config ai binI = fromJustNote "instIForItself" $
    indexInst ai (instForItself config ai binI)

hardDependencyRules :: Config -> AtomIndex -> PackageInfo -> Producer (Clause AtomI)
hardDependencyRules config ai pi = (toProducer $ hardDependencies)
  where hardDependencies =
            {-# SCC "hardDependencies" #-}
            -- Dependencies
            [ Implies instI deps ("the package depends on \"" ++ BS.unpack reason ++ "\".") |
                (forI,binIs) <- IxM.toList (dependsBadHull pi),
                binI <- IxS.toList binIs,
                let Binary _ _ a' = ai `lookupBin` forI,
                a <- ST.maybe (arches config) (:[]) a',
                let instI = genIndex . fromJustNote "Y" . indexInst ai $ Inst forI binI a,
                (disjunction, reason) <- depends pi IxM.! binI,
                let deps = [ case indexInst ai (Inst forI depI a) of
                                Just instI -> genIndex instI
                                Nothing    -> genIndex . fromJustNote "X" . indexInst ai $ Inst depI depI a
                           | depI <- disjunction ]
            ] ++
            [ NotBoth instI conflI ("the package conflicts with \"" ++ BS.unpack reason ++ "\".") |
                (forI,binIs) <- IxM.toList (dependsBadHull pi),
                binI <- IxS.toList binIs,
                let Binary _ _ a' = ai `lookupBin` forI,
                a <- ST.maybe (arches config) (:[]) a',
                let instI = genIndex . fromJustNote "Y" . indexInst ai $ Inst forI binI a,
                (disjunction, reason) <- conflicts pi IxM.! binI,
                confl <- disjunction,
                confl `IxS.member` binIs,
                let conflI = genIndex . fromJustNote "Z" . indexInst ai $ Inst forI confl a
            ] ++
            [ Implies instI [genIndex binI] "the package needs to be present" |
                inst@(Inst binI _ _) <- installabilityAtoms config pi ai,
                let instI = genIndex . fromJustNote "Y" . indexInst ai $ inst
            ] ++
            [Implies (genIndex binI) deps ("the package depends on \"" ++ BS.unpack reason ++ "\".") |
                (binI,depends) <- IxM.toList (depends pi),
                binI `IxS.member` affected pi,
                binI `IxM.notMember` dependsBadHull pi,
                let instI = instIForItself config ai,
                (disjunction, reason) <- depends,
                let deps = map (genIndex . instIForItself config ai) disjunction
            ]

softDependencyRules :: Config -> AtomIndex -> PackageInfo -> Producer (Clause AtomI)
softDependencyRules config ai pi = toProducer $ softDependencies
  where softDependencies =
            {-# SCC "softDependencies" #-}
            [Implies (genIndex forI) [genIndex instI] "the package ought to be installable." |
                forI <- IxM.keys (depends pi),
                let instI = instIForItself config ai forI
            ]


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
