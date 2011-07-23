module TransRules where

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map ((!))
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified Data.Strict as ST
import Data.Functor

import Debian.Relation (checkVersionReq)

import Types
import LitSat

transitionRules config unstable testing =
    ( keepSrc ++ keepBin ++ uniqueBin ++ needsSource ++ releaseSync ++ outdated ++ tooyoung ++ buggy ++ dependencies
    , dependencies)
  where keepSrc = 
            -- A source that exists both in unstable and in testing has to stay in testing
            [OneOf atoms ("source " ++ show name ++ " was in testing before.") |
                (name,pkgs) <- M.toList sourcesBoth,
                let atoms = map SrcAtom (nub pkgs)
            ]
        keepBin = 
            -- A binary that exists both in unstable and in testing has to stay in testing
            [OneOf atoms ("binary " ++ show name ++ " on " ++ show arch ++ " was in testing before.") |
                ((name,arch),pkgs) <- M.toList binariesBoth,
                let atoms = map BinAtom (nub pkgs)
            ]
        uniqueBin = 
            -- At most one binary per name and architecture
            [AtMostOne (nub pkgs) ("binaries ought to be unique per architecture") |
                ((name,arch),pkgs') <- M.toList binariesBoth,
                let pkgs = map BinAtom (nub pkgs'),
                length pkgs > 1
            ]
        dependencies =
            -- Dependencies
            [Implies (BinAtom bin) deps ("the package depends on \"" ++ BS.unpack reason ++ "\".") |
                (bin@(Binary _ _ arch),depends) <- M.toList dependsUnion,
                (disjunction, reason) <- depends,
                let deps = map BinAtom . nub . concatMap (resolve arch) $ disjunction
            ]
        releaseSync = 
            -- release architectures ought to all migrate
            [Implies (SrcAtom src) [bin] ("release architectures ought to migrate completely") |
                (src, bins) <- M.toList buildsOnlyUnstable,
                -- BEWARE: If more than one binary with the same name built by the same
                -- source on the same architecture exists in unstable, this will
                -- contradict with the unique binary package rule.
                bin <- BinAtom <$> bins
            ] 
        tooyoung = 
            -- packages need to be old enough
            [Not (SrcAtom src) ("it is " ++ show age ++ " days old, needs " ++ show minAge) |
                src <- M.keys buildsOnlyUnstable,
                Just age <- [src `M.lookup` ages testing],
                let minAge = fromMaybe (defaultMinAge config) $
                             urgencies testing `combine` minAges config $ src,
                age <= minAge
            ] 
        needsSource = 
            -- a package needs its source
            [Implies (BinAtom bin) [SrcAtom src] ("of the DFSG") |
                (bin, src) <- M.toList (builtBy unstable)
            ]
        outdated = 
            -- release architectures ought not to be out of date
            [Not (SrcAtom newer) ("is out of date: " ++ show bin ++ " exists in unstable") |
                (bin, src) <- M.toList (builtBy unstable),
                -- TODO: only release architecture here
                newer <- newerSources unstable ! src,
                newer `S.notMember` sources testing
            ]
        buggy = 
            -- no new RC bugs
            [Implies atom [bug] ("it has this bugs") |
                (atom, bugs) <- M.toList bugsUnion,
                bug <- BugAtom <$> nub bugs
            ] ++
            [Not atom ("it was not in testing before") |
                
                atom <- BugAtom <$> S.toList forbiddenBugs
            ]


        sourcesBoth = M.intersectionWith (++) (sourceNames unstable) (sourceNames testing)
        binariesBoth = M.intersectionWith (++) (binaryNames unstable) (binaryNames testing)
        binariesUnion = M.unionWith (++) (binaryNames unstable) (binaryNames testing)
        providesUnion = M.unionWith (++) (provides unstable) (provides testing)
        -- We assume that the dependency information is the same, even from different suites
        dependsUnion = M.union (depends unstable) (depends testing)
        bugsUnion = M.unionWith (++) (bugs unstable) (bugs testing)

        -- This does not work, as bugs with tag "sid" would appear as new bugs
        -- bugsInTesting = S.fromList (concat (M.elems (bugs testing)))
        -- bugsInUnstable = S.fromList (concat (M.elems (bugs unstable)))
        bugsInTesting = S.fromList [ bug |
            atom <- S.toList (atoms testing),
            bug <- M.findWithDefault [] atom bugsUnion ]
        bugsInUnstable = S.fromList [ bug |
            atom <- S.toList (atoms unstable),
            bug <- M.findWithDefault [] atom bugsUnion ]
        forbiddenBugs = bugsInUnstable `S.difference` bugsInTesting

        buildsOnlyUnstable = M.difference (builds unstable) (builds testing)
        atomsOnlyUnstable = S.difference (atoms unstable) (atoms testing)

        resolve mbArch (DepRel name mbVerReq mbArchReq)
            | checkArchReq mbArchReq = 
                [ atom |
                    atom@(Binary pkg version _) <- M.findWithDefault [] 
                        (name, arch) binariesUnion,
                    checkVersionReq mbVerReq (Just version)
                ] ++ 
                if isJust mbVerReq then [] else 
                [ atom |
                    atom <- M.findWithDefault [] (name, arch) providesUnion
                ]
            | otherwise = []
          where arch = ST.fromMaybe (archForAll config) mbArch 
                checkArchReq Nothing = True
                checkArchReq (Just (ArchOnly arches)) = arch `elem` arches
                checkArchReq (Just (ArchExcept arches)) = arch `notElem` arches
    

combine :: (Ord a, Ord b) => M.Map a b -> M.Map b c -> a -> Maybe c
combine m1 m2 x = (x `M.lookup` m1) >>= (`M.lookup` m2)
