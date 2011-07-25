module TransRules where

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map ((!))
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified Data.Strict as ST
import Data.Functor

import Types
import LitSat

transitionRules config ai unstable testing general =
    ( keepSrc ++ keepBin ++ uniqueBin ++ needsSource ++ needsBinary ++ releaseSync ++ outdated ++ obsolete ++ tooyoung ++ buggy
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
            [AtMostOne (nub pkgs) ("binaries ought to be unique per architecture") |
                ((name,arch),pkgs') <- M.toList binariesBoth,
                let pkgs = map genIndex (nub pkgs'),
                length pkgs > 1
            ]
        dependencies =
            {-# SCC "dependencies" #-}
            -- Dependencies
            [Implies (genIndex binI) deps ("the package depends on \"" ++ BS.unpack reason ++ "\".") |
                (binI,depends) <- M.toList dependsUnion,
                let Binary _ _ arch = ai `lookupBin` binI,
                (disjunction, reason) <- depends,
                let deps = map genIndex . nub . concatMap ({-# SCC "resolve" #-} resolve arch) $ disjunction
            ]
        conflictClauses =
            {-# SCC "conflictClauses" #-}
            -- Conflicts
            [NotBoth (genIndex binI) (genIndex confl) ("the package conflicts with \"" ++ BS.unpack reason ++ "\".") |
                (binI,depends) <- M.toList conflictsUnion,
                let Binary _ _ arch = ai `lookupBin` binI,
                (disjunction, reason) <- depends,
                rel@(DepRel _ (Just vr) _ ) <- disjunction,
                hasUpperBound vr,
                confl <- nub (resolve arch rel)
            ] ++
            [NotBoth (genIndex binI) (genIndex confl) ("the package breaks on \"" ++ BS.unpack reason ++ "\".") |
                (binI,depends) <- M.toList breaksUnion,
                let Binary _ _ arch = ai `lookupBin` binI,
                (disjunction, reason) <- depends,
                rel@(DepRel _ (Just vr) _ ) <- disjunction,
                -- hasUpperBound vr,
                confl <- nub (resolve arch rel)
            ]
        releaseSync = 
            {-# SCC "releaseSync" #-}
            -- release architectures ought to all migrate
            [Implies (genIndex src) [bin] ("release architectures ought to migrate completely") |
                (src, bins) <- M.toList buildsOnlyUnstable,
                -- BEWARE: If more than one binary with the same name built by the same
                -- source on the same architecture exists in unstable, this will
                -- contradict with the unique binary package rule.
                bin <- genIndex <$> bins
            ] 
        tooyoung = 
            {-# SCC "tooyoung" #-}
            -- packages need to be old enough
            [Not (genIndex src) ("it is " ++ show age ++ " days old, needs " ++ show minAge) |
                src <- M.keys buildsOnlyUnstable,
                Just age <- [src `M.lookup` ages general],
                let minAge = fromMaybe (defaultMinAge config) $
                             urgencies general `combine` minAges config $ src,
                age <= minAge
            ] 
        needsSource = 
            {-# SCC "needsSource" #-}
            -- a package needs its source
            [Implies (genIndex bin) [genIndex src] ("of the DFSG") |
                (bin, src) <- M.toList builtByUnion
            ]
        needsBinary =
            {-# SCC "needsBinary" #-}
            -- a source needs a binary
            [Implies (genIndex src) bins ("it were useless otherwise") |
                (src, binIs) <- M.toList buildsUnion,
                let bins = map genIndex (nub binIs)
            ]
        outdated = 
            {-# SCC "outdated" #-}
            -- release architectures ought not to be out of date
            [Not (genIndex newer) ("is out of date: " ++ show (ai `lookupBin` binI) ++ " exists in unstable") |
                (binI, src) <- M.toList (builtBy unstable),
                -- TODO: only release architecture here
                newer <- newerSources unstable ! src,
                newer `S.notMember` sources testing
            ]
        obsolete = 
            {-# SCC "obsolete" #-}
            -- never add a source package to testing that is already superceded
            [Not (genIndex src) ("it is already superceded by " ++ show (ai `lookupSrc` s)) |
                (src, bins) <- M.toList buildsOnlyUnstable,
                (s:_) <- [newerSources unstable ! src]
            ]
        buggy = 
            {-# SCC "buggy1" #-}
            -- no new RC bugs
            [Implies atom [bug] ("it has this bug") |
                (atom, bugs) <- M.toList bugsUnion,
                bug <- genIndex <$> nub bugs
            ] ++
            {-# SCC "buggy2" #-}
            [Not atom ("it was not in testing before") |
                
                atom <- genIndex <$> S.toList forbiddenBugs
            ]

        -- Some duplication wrt above code
        obsoleteSource = S.fromList [ src |
                (src, bins) <- M.toList buildsOnlyUnstable,
                (s:_) <- [newerSources unstable ! src]
            ]
        youngSource = S.fromList [ src |
                src <- M.keys buildsOnlyUnstable,
                Just age <- [src `M.lookup` ages general],
                let minAge = fromMaybe (defaultMinAge config) $
                             urgencies general `combine` minAges config $ src,
                age <= minAge
            ]

        desired  = fmap genIndex $ S.toList $
            sources unstable `S.difference` sources testing
                             `S.difference` obsoleteSource
                             `S.difference` youngSource
        unwanted = [] -- fmap genIndex $ S.toList $ sources testing `S.difference` sources unstable

        sourcesBoth = {-# SCC "sourcesBoth" #-} M.intersectionWith (++) (sourceNames unstable) (sourceNames testing)
        binariesBoth = {-# SCC "binariesBoth" #-} M.intersectionWith (++) (binaryNames unstable) (binaryNames testing)
        binariesUnion = {-# SCC "binariesUnion" #-} M.unionWith (++) (binaryNames unstable) (binaryNames testing)
        providesUnion = {-# SCC "providesUnion" #-} M.unionWith (++) (provides unstable) (provides testing)
        -- We assume that the dependency information is the same, even from different suites
        dependsUnion = {-# SCC "dependsUnion" #-} M.union (depends unstable) (depends testing)
        conflictsUnion = {-# SCC "conflictsUnion" #-} M.union (conflicts unstable) (conflicts testing)
        builtByUnion = {-# SCC "builtByUnion" #-} M.union (builtBy unstable) (builtBy testing)
        buildsUnion = {-# SCC "buildsUnion" #-} M.unionWith (++) (builds unstable) (builds testing)

        breaksUnion = {-# SCC "breaksUnion" #-} M.union (breaks unstable) (breaks testing)
        bugsUnion = {-# SCC "bugsUnion" #-} M.unionWith (++) (bugs unstable) (bugs testing)

        -- This does not work, as bugs with tag "sid" would appear as new bugs
        -- bugsInTesting = S.fromList (concat (M.elems (bugs testing)))
        -- bugsInUnstable = {-# SCC "bugsInUnstable" #-} S.fromList (concat (M.elems (bugs unstable)))
        bugsInTesting = {-# SCC "bugsInTesting" #-} S.fromList [ bug |
            atom <- S.toList (atoms testing),
            bug <- M.findWithDefault [] atom bugsUnion ]
        bugsInUnstable = {-# SCC "bugsInUnstable" #-} S.fromList [ bug |
            atom <- S.toList (atoms unstable),
            bug <- M.findWithDefault [] atom bugsUnion ]
        forbiddenBugs = {-# SCC "forbiddenBugs" #-} bugsInUnstable `S.difference` bugsInTesting

        buildsOnlyUnstable = {-# SCC "buildsOnlyUnstable" #-} M.difference (builds unstable) (builds testing)
        atomsOnlyUnstable = {-# SCC "atomsOnlyUnstable" #-} S.difference (atoms unstable) (atoms testing)

        resolve :: ST.Maybe Arch -> DepRel -> [BinI]
        resolve mbArch (DepRel name mbVerReq mbArchReq)
            | checkArchReq mbArchReq = 
                [ binI |
                    binI <- M.findWithDefault [] (name, arch) binariesUnion,
                    let Binary pkg version _ = ai `lookupBin` binI,
                    checkVersionReq mbVerReq (Just version)
                ] ++ 
                if isJust mbVerReq then [] else 
                [ binI |
                    binI <- M.findWithDefault [] (name, arch) providesUnion
                ]
            | otherwise = []
          where arch = ST.fromMaybe (archForAll config) mbArch 
                checkArchReq Nothing = True
                checkArchReq (Just (ArchOnly arches)) = arch `elem` arches
                checkArchReq (Just (ArchExcept arches)) = arch `notElem` arches

-- |Check if a version number satisfies a version requirement.
checkVersionReq :: Maybe VersionReq -> Maybe DebianVersion -> Bool
checkVersionReq Nothing _ = True
checkVersionReq _ Nothing = False
checkVersionReq (Just (SLT v1)) (Just v2) =
    v2 `cmpDebianVersion` v1 == LT
checkVersionReq (Just (LTE v1)) (Just v2) =
    v2 `cmpDebianVersion` v1 <= EQ
checkVersionReq (Just (EEQ v1)) (Just v2) =
    v2 `cmpDebianVersion` v1 == EQ
checkVersionReq (Just (GRE v1)) (Just v2) =
    v2 `cmpDebianVersion` v1 >= EQ
checkVersionReq (Just (SGR v1)) (Just v2) =
    v2 `cmpDebianVersion` v1 == GT

    

combine :: (Ord a, Ord b) => M.Map a b -> M.Map b c -> a -> Maybe c
combine m1 m2 x = (x `M.lookup` m1) >>= (`M.lookup` m2)
