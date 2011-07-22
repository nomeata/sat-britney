module TransRules where

import Data.List
import qualified Data.Map as M
import Data.Map ((!))
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified Data.Strict as ST

import Debian.Relation

import Types
import LitSat

transitionRules config unstable testing =
    -- First rule: A source that exists both in unstable and in testing has to stay in testing
    [OneOf (nub pkgs) ("source " ++ show name ++ " was in testing before.") |
        (name,pkgs) <- M.toList sourcesBoth
    ] ++
    -- Second rule: A binary that exists both in unstable and in testing has to stay in testing
    [OneOf (nub pkgs) ("binary " ++ show name ++ " on " ++ show arch ++ " was in testing before.") |
        ((name,arch),pkgs) <- M.toList binariesBoth
    ] ++
    -- Third rule: At most one binary per name and architecture
    [AtMostOne (nub pkgs) ("binaries ought to be unique per architecture") |
        ((name,arch),pkgs') <- M.toList binariesBoth,
        let pkgs = nub pkgs',
        length pkgs > 1
    ] ++
    -- Forth rule: Dependencies
    [Implies atom (nub deps) ("the package depends on \"" ++ BS.unpack reason ++ "\".") |
        (atom@(Binary _ _ arch),depends) <- M.toList dependsUnion,
        (disjunction, reason) <- depends,
        let deps = concatMap (resolve arch) disjunction
    ] ++
    -- Fifth rule: release architectures ought to all migrate
    [Implies src [bin] ("release architectures ought to migrate completely") |
        (src, bins) <- M.toList buildsOnlyUnstable,
        -- BEWARE: If more than one binary with the same name built by the same
        -- source on the same architecture exists in unstable, this will
        -- contradict with the unique binary package rule.
        bin <- bins
    ] ++
    -- Sixth rule: release architectures ought not to be out of date
    [Not newer ("is out of date: " ++ show bin ++ " exists in unstable") |
        (bin, src) <- M.toList (builtBy unstable),
        -- TODO: only release architecture here
        newer <- newerSources unstable ! src
    ]
  where sourcesBoth = M.intersectionWith (++) (sourceNames unstable) (sourceNames testing)
        binariesBoth = M.intersectionWith (++) (binaryNames unstable) (binaryNames testing)
        binariesUnion = M.unionWith (++) (binaryNames unstable) (binaryNames testing)
        providesUnion = M.unionWith (++) (provides unstable) (provides testing)
        -- We assume that the dependency information is the same, even from different suites
        dependsUnion = M.union (depends unstable) (depends testing)

        buildsOnlyUnstable = M.difference (builds unstable) (builds testing)

        resolve mbArch (DepRel name mbVerReq mbArchReq) =
            [ atom |
                atom@(Binary pkg version _) <- M.findWithDefault [] 
                    (name, arch) binariesUnion,
                checkVersionReq mbVerReq (Just version)
            ] ++ 
            if isJust mbVerReq then [] else 
            [ atom |
                atom <- M.findWithDefault [] (name, arch) providesUnion
            ]
          where arch = ST.fromMaybe (archForAll config) mbArch 
    
