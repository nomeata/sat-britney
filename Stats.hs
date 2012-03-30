module Stats where

import Text.Printf
import System.FilePath

import qualified IndexSet as IxS
import qualified IndexMap as IxM
import qualified ArchMap as AM

import DepRules
import ParseSuite
import Types
import AtomIndex

sumArch :: AM.Map a -> (a -> Int) -> Int
sumArch m f = sum [ f v | v <- AM.elems m ]

printStats config = do
    let ai1 = emptyIndex
    (unstable, unstableRPI, ai2) <- parseSuite config ai1 (dir config </> "unstable")
    (testing, testingRPI, ai)  <- parseSuite config ai2 (dir config </> "testing")
    piM <- AM.buildM (arches config) $ \arch ->
            resolvePackageInfo config False ai IxS.empty IxS.empty arch [testing, unstable] 

    let bin = IxS.size $ binaries unstable `IxS.union` binaries testing
    printf "Binary packages: %13d\n" bin
    let confs = sumArch piM $
            sum . map IxS.size . IxM.elems . conflictsRel . snd
    printf "Conflicts: %13d\n" confs
    let deps = sumArch piM $
            sum . map length . IxM.elems . depends . fst
    printf "Dependencies: %13d\n" deps
    printf "Paper-Numbers 1: %13d atoms, %13d clauses in P_t^1\n" bin deps
    printf "Paper-Numbers 2: %13d atoms, %13d clauses in P_t^2\n" (bin + bin*bin) (deps*bin)
    let instAtoms3 = sumArch piM $
            sum . map IxS.size . IxM.elems . transitiveHull . dependsRel . snd
    let deps3 = sumArch piM $ \(pi,ps) -> 
            sum $ map (sum . map (length . (depends pi IxM.!)) . IxS.toList) $ IxM.elems $ transitiveHull $ dependsRel ps
    printf "Paper-Numbers 3: %13d atoms, %13d clauses in P_t^3\n" (bin + instAtoms3) deps3

    piM4 <- AM.buildM (arches config) $ \arch ->
            resolvePackageInfo config True ai IxS.empty IxS.empty arch [testing, unstable]
    let instAtoms4 = sumArch piM4 $
            sum . map IxS.size . IxM.elems . dependsBadHull . fst
    let deps4 = sumArch piM4 $ \(pi,ps) -> 
            sum $ map (sum . map (length . (depends pi IxM.!)) . IxS.toList) $ IxM.elems $ dependsBadHull pi
    printf "Paper-Numbers 4: %13d atoms, %13d clauses in P_t^4\n" (bin + instAtoms4) deps4


    let instAtoms5 = sumArch piM $
            sum . map IxS.size . IxM.elems . dependsBadHull . fst
    let deps5 = sumArch piM $ \(pi,ps) -> 
            sum $ map (sum . map (length . (depends pi IxM.!)) . IxS.toList) $ IxM.elems $ dependsBadHull pi
    printf "Paper-Numbers 5: %13d atoms, %13d clauses in P_t^5\n" (bin + instAtoms5) deps5
