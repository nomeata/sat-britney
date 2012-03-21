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

resolvePackageInfo :: Config -> AtomIndex -> IxS.Set Source -> IxS.Set Binary -> Arch -> [SuiteInfo] -> [RawPackageInfo] -> PackageInfo
resolvePackageInfo config ai nonCandidates unmod piArch sis rawPackageInfos = PackageInfoOut{..}
  where buildsUnion = {-# SCC "buildsUnion" #-} IxM.unionsWith (++) $ map builds sis

        -- All binaries from this arch and arch all
        binariesUnion = {-# SCC "binariesUnion" #-}
            IxS.filter (\binI ->
                let Binary _ _ arch = ai `lookupBin` binI
                in  ST.maybe True (== piArch) arch ) $
            IxS.unions $
            map binaries sis

        affected = {-# SCC "affected" #-} IxS.unions . map (transitiveHull1 revDependsRel) $ IxS.toList $ binariesUnion `IxS.difference` unmod

        nonCandidateBins = {-# SCC "nonCandidateBins" #-} IxS.seal $ IxS.fromList $
            concatMap (buildsUnion IxM.!) $
            IxS.toList nonCandidates

        -- A package is relevant here if it is from this architecture, or it is
        -- a dependency, or it is arch all and this arch is archForAll.
        isArchForAll = piArch == archForAll config
        relevantBins = 
            IxS.unions .
            map (transitiveHull1 dependsRel) .
            IxS.toList .
            IxS.filter (\binI ->
                let Binary _ _ arch = ai `lookupBin` binI
                in  binI `IxS.notMember` nonCandidateBins &&
                    ST.maybe isArchForAll (const True) arch) $
            binariesUnion
        
        depends = {-# SCC "depends" #-} IxM.fromList $  
                    map (\(binI,deps) -> 
                        (binI, map (first
                            (
                            filter (`IxS.notMember` nonCandidateBins) .
                            nub .
                            concatMap resolveDep) )
                            deps)
                    ) $
                    filter (\(k,v) -> 
                            k `IxS.member` binariesUnion && 
                            k `IxS.notMember` nonCandidateBins
                        ) $
                    concatMap dependsR rawPackageInfos

        dependsRel = {-# SCC "dependsRel" #-} IxM.filter (not . IxS.null) $
                     IxM.map (IxS.fromList . concatMap fst) $
                     depends

        dependsRelWithConflicts = {-# SCC "dependsRelWithConflicts" #-}
            IxM.map (IxS.filter (`IxS.member` hasConflictInDepsProp)) dependsRel

        dependsRelWithConflictsHull = {-# SCC "dependsRelWithConflictsHull" #-}
            transitiveHull dependsRelWithConflicts

        revDependsRel = {-# SCC "revDependsRel" #-} reverseRel dependsRel

        revDependsHull = {-# SCC "revDependsHull" #-}
            transitiveHull revDependsRel

        dependsHull = transitiveHull dependsRel
        
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
                            let Binary pkg _ _ = ai `lookupBin` binI
                            in (binI, map (first $ nub . concatMap (resolveConf pkg)) deps)
                        ) $
                        concatMap conflictsR rawPackageInfos
                    )
                    ( IxM.fromList $
                        map (\(binI,deps) ->
                            let Binary pkg _ arch = ai `lookupBin` binI
                            in (binI, map (first $ nub . concatMap (resolveConf pkg)) deps)
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

        resolve :: Maybe BinName ->  DepRel -> [BinI]
        resolve mbPkg (DepRel name mbVerReq mbArchReq)
            | checkArchReq mbArchReq = 
                [ binI |
                    binI <- M.findWithDefault [] (name, piArch) binaryNamesUnion,
                    let Binary pkg version _ = ai `lookupBin` binI,
                    maybe True (/= pkg) mbPkg, -- conflicts only with different names
                    checkVersionReq mbVerReq (Just version)
                ] ++ 
                if ST.isJust mbVerReq then [] else 
                [ binI |
                    binI <- M.findWithDefault [] (name, piArch) providesUnion,
                    let Binary pkg _ _ = ai `lookupBin` binI,
                    maybe True (/= pkg) mbPkg -- conflicts only with different names
                ]
            | otherwise = []
          where checkArchReq ST.Nothing = True
                checkArchReq (ST.Just (ArchOnly arches)) = piArch `elem` arches
                checkArchReq (ST.Just (ArchExcept arches)) = piArch `notElem` arches

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
    Inst binI binI (piArch pi) |
    binI <- IxS.toList (relevantBins pi)
    ]

installabilityAtoms :: Config -> PackageInfo -> AtomIndex -> [Inst]
installabilityAtoms config pi ai =
    [ Inst p d (piArch pi) |
    Inst p _ _ <- initialInstallabilityAtoms config pi ai,
    d <- IxS.toList $ reflexiveLookup p (dependsBadHull pi)
    ]

reflexiveLookup :: Index a -> IxM.Map a (IxS.Pred a) -> IxS.Pred a
reflexiveLookup x m = fromMaybe (IxS.seal $ IxS.singleton x) $ IxM.lookup x m

generateInstallabilityAtoms :: Config -> PackageInfo -> AtomIndex -> AtomIndex
generateInstallabilityAtoms config pi ai =
    foldl' (\ai i ->  fst (ai `addInst` i)) ai $ installabilityAtoms config pi ai

hardDependencyRules :: Config -> AtomIndex -> PackageInfo -> Producer (Clause AtomI)
hardDependencyRules config ai pi f x = (toProducer $ hardDependencies) f x 
  where hardDependencies =
            {-# SCC "hardDependencies" #-}
            -- Dependencies
            [ Implies instI deps ("the package depends on \"" ++ BS.unpack reason ++ "\".") |
                forI <- IxS.toList (relevantBins pi),
                let binIs = reflexiveLookup forI (dependsBadHull pi),
                binI <- IxS.toList binIs,
                let instI = genIndex . fromJustNote "Y" . indexInst ai $ Inst forI binI (piArch pi),
                (disjunction, reason) <- depends pi IxM.! binI,
                let deps = [ case indexInst ai (Inst forI depI (piArch pi)) of
                                Just instI -> genIndex instI
                                Nothing    -> genIndex . fromJustNote "X" . indexInst ai $ Inst depI depI (piArch pi)
                           | depI <- disjunction ]
            ] ++
            [ NotBoth instI conflI ("the package conflicts with \"" ++ BS.unpack reason ++ "\".") |
                forI <- IxS.toList (relevantBins pi),
                let binIs = reflexiveLookup forI (dependsBadHull pi),
                binI <- IxS.toList binIs,
                let instI = genIndex . fromJustNote "X" . indexInst ai $ Inst forI binI (piArch pi),
                (disjunction, reason) <- conflicts pi IxM.! binI,
                confl <- disjunction,
                confl `IxS.member` binIs,
                let conflI = genIndex . fromJustNote "Z" . indexInst ai $ Inst forI confl (piArch pi)
            ] ++
            [ Implies instI [genIndex binI] "the package needs to be present" |
                inst@(Inst binI _ _) <- installabilityAtoms config pi ai,
                let instI = genIndex . fromJustNote "V" . indexInst ai $ inst
            ]

softDependencyRules :: Config -> AtomIndex -> PackageInfo -> Producer (Clause AtomI)
softDependencyRules config ai pi = toProducer $ softDependencies
  where softDependencies =
            {-# SCC "softDependencies" #-}
            [Implies (genIndex forI) [genIndex instI] "the package ought to be installable." |
                forI <- IxM.keys (depends pi),
                let Binary _ _ a' = ai `lookupBin` forI,
                ST.maybe (piArch pi == archForAll config) (== piArch pi) a',
                let instI = genIndex . fromJustNote "X" . indexInst ai $ Inst forI forI (piArch pi)
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
