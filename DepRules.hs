{-# LANGUAGE RecordWildCards, Rank2Types, ImpredicativeTypes, TupleSections, BangPatterns #-}
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
import qualified Data.Set as S
import Data.Functor
import Control.Arrow (first)
import Control.DeepSeq
import Debug.Trace
import Safe

import ParseSuite
import Indices
import Types
import Arches
import AtomIndex
import LitSat
import qualified IndexSet as IxS
import qualified IndexMap as IxM
import qualified ArchMap as AM

resolvePackageInfo :: Config -> Bool -> AtomIndex -> IxS.Set Source -> IxS.Set Binary -> Arch -> [SuiteInfo] -> PackagesFiles -> IO (PackageInfo, PackageStats)
resolvePackageInfo config onlyEasyPackages ai nonCandidates unmod arch sis pf = do
    (binaryNames, provides) <- parsePackageProvides config ai arch (pf AM.! arch)
    rawDepends <- parsePackageDependencies config ai arch (pf AM.! arch)
    let (depends, conflicts) = flattenRelations ai nonCandidates arch sis binaryNames provides rawDepends
    return $!! resolvePackageInfo' config onlyEasyPackages ai nonCandidates unmod arch sis depends conflicts

flattenRelations :: AtomIndex -> IxS.Set Source -> Arch -> [SuiteInfo] -> M.Map BinName [BinI] -> M.Map BinName [BinI] -> [(BinI, Dependency, Dependency)] -> (IxM.Map Binary [([BinI], BS.ByteString)], IxM.Map Binary [([BinI], BS.ByteString)])
flattenRelations ai nonCandidates arch sis binaryNames provides = foldl' go (IxM.empty, IxM.empty) 
  where go (!depM, !confM) (binI, deps, confs)
            | binI `IxS.member` nonCandidateBins = (depM, confM)
            | binI `IxM.member` depM = (depM, confM)
            | binI `IxM.member` confM = error "Entry in depM but not in confM"
            | otherwise  =
                ( IxM.insert
                    binI  
                    (map (first (nub . filter (`IxS.notMember` nonCandidateBins) . concatMap resolveDep)) deps)
                    depM
                , let thisName = binName (ai `lookupBin` binI)
                      thisConfls = map (first $ nub . concatMap (resolveConf thisName)) confs
                  in if null thisConfls then confM else IxM.insert binI thisConfls confM
                )

        buildsUnion = {-# SCC "buildsUnion" #-} IxM.unionsWith (++) $ map builds sis

        nonCandidateBins = {-# SCC "nonCandidateBins" #-} IxS.seal $ IxS.fromList $
            concatMap (buildsUnion IxM.!) $
            IxS.toList nonCandidates

        resolveDep  = resolve Nothing
        resolveConf = resolve . Just 

        resolve :: Maybe BinName ->  DepRel -> [BinI]
        resolve mbPkg (DepRel name mbVerReq mbArchReq)
            | checkArchReq mbArchReq = 
                [ binI |
                    binI <- M.findWithDefault [] name binaryNames,
                    let Binary pkg version _ = ai `lookupBin` binI,
                    maybe True (/= pkg) mbPkg, -- conflicts only with different names
                    checkVersionReq mbVerReq (Just version)
                ] ++ 
                if ST.isJust mbVerReq then [] else 
                [ binI |
                    binI <- M.findWithDefault [] name provides,
                    let Binary pkg _ _ = ai `lookupBin` binI,
                    maybe True (/= pkg) mbPkg -- conflicts only with different names
                ]
            | otherwise = []
          where checkArchReq ST.Nothing = True
                checkArchReq (ST.Just (ArchOnly arches)) = arch `elem` arches
                checkArchReq (ST.Just (ArchExcept arches)) = arch `notElem` arches


resolvePackageInfo' :: Config -> Bool -> AtomIndex -> IxS.Set Source -> IxS.Set Binary -> Arch -> [SuiteInfo] -> IxM.Map Binary [([BinI], BS.ByteString)] -> IxM.Map Binary [([BinI], BS.ByteString)] -> (PackageInfo, PackageStats)
resolvePackageInfo' config onlyEasyPackages ai nonCandidates unmod piArch sis depends conflicts = (PackageInfo{..}, PackageStats {..})
  where buildsUnion = {-# SCC "buildsUnion" #-} IxM.unionsWith (++) $ map builds sis

        -- All binaries from this arch and arch all
        binariesUnion = IxM.keysSet depends

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
            M.unionsWith (+) $
            map (\(_,cs) ->
                M.fromListWith (+) $
                map (\c -> (c,1)) $
                filter (\(c1,c2) -> c1 < c2) $
                S.toList cs
            ) $
            IxM.toList relevantConflicts

        relevantDepHistogram = {-# SCC "relevantDepHistogram" #-}
            -- reverse $
            -- sortBy (comparing snd) $
            -- M.toList $ 
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
                in if onlyEasyPackages then deps else IxS.unions [ hull
                    | (c1,c2) <- S.toList relConfs
                    , c1 < c2
                    , let hull =
                            (IxM.findWithDefault IxS.empty c1 revDependsHull `IxS.intersection` deps) `IxS.union`
                            (IxM.findWithDefault IxS.empty c2 revDependsHull `IxS.intersection` deps)
                    -- Little assertion:
                    , if p `IxS.member` hull then True else error "p not in depsHull"
                    ]


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


mergePackageStats :: [PackageStats] -> PackageStats
mergePackageStats pss = PackageStats conflictHistogram' relevantDepHistogram' hasConflict' hasConflictInDeps' conflictsRel' dependsRel'
  where conflictHistogram' = M.unionsWith (+) $ map conflictHistogram pss
        relevantDepHistogram' = M.unionsWith (+) $ map relevantDepHistogram pss
        hasConflict' = IxS.unions $ map hasConflict pss
        hasConflictInDeps' = IxS.unions $ map hasConflictInDeps pss
        conflictsRel' = IxM.unionsWith IxS.union $ map conflictsRel pss
        dependsRel' = IxM.unionsWith IxS.union $ map dependsRel pss

histogramToList :: Int -> M.Map a Int -> [(a,Int)]
histogramToList n = take n . reverse . sortBy (comparing snd) . M.toList

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
addTransitiveHull1 rel x = go [x]
  where go []     hull = hull
        go (x:xs) hull | x `IxS.member` hull = go xs hull
                       | otherwise = go (maybe [] IxS.toList (IxM.lookup x rel) ++ xs)
                                        (IxS.insert x hull) 

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
    foldl' (\ai' i ->  fst (ai' `addInst` i)) ai $ installabilityAtoms config pi ai

dependencyRules :: Config -> AtomIndex -> IxS.Set Binary -> PackageInfo -> Producer (Clause AtomI)
dependencyRules config ai uninst pi f x = (toProducer $ hardDependencies) f x 
  where hardDependencies =
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
                (disjunction, reason) <- fromMaybe [] $ IxM.lookup binI (conflicts pi),
                confl <- disjunction,
                confl `IxS.member` binIs,
                let conflI = genIndex . fromJustNote "Z" . indexInst ai $ Inst forI confl (piArch pi)
            ] ++
            [ Implies instI [genIndex binI] "the package needs to be present" |
                inst@(Inst _ binI _) <- installabilityAtoms config pi ai,
                let instI = genIndex . fromJustNote "V" . indexInst ai $ inst
            ] ++
            [Implies (genIndex forI) [genIndex instI] "the package ought to be installable." |
                forI <- IxM.keys (depends pi),
                forI `IxS.notMember` uninst,
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
