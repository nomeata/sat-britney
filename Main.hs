{-# LANGUAGE OverloadedStrings, RecordWildCards, ImpredicativeTypes, DoAndIfThenElse #-}

-- |
-- Copyright: (c) 2011 Joachim Breitner
-- License: GPL-2
--

import System.Environment
import System.FilePath
import Text.PrettyPrint
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Strict as ST
import qualified Data.Vector as V
import Control.Monad
import System.IO
import System.Console.GetOpt
import System.Exit
import Data.Functor
import Data.Maybe
import Data.List
import GHC.Exts ( augment, build ) 
import Text.Printf

import qualified IndexSet as IxS
import qualified IndexMap as IxM
import qualified ArchMap as AM

import ParseSuite
import DebCheck
import TransRules
import DepRules
import Types
import Arches
import PrettyPrint
import ClauseSat
import Picosat
import LitSat
import Hints
import Heidi
import ParseHints
import Indices
import AtomIndex

minAgeTable = M.fromList [
    (Urgency "low", Age 10), 
    (Urgency "medium", Age 5),
    (Urgency "high", Age 2),
    (Urgency "critical", Age 2),
    (Urgency "emergency", Age 2)
    ]

defaultConfig :: Config
defaultConfig = Config "." Nothing (V.toList allArches) (V.toList allArches) i386 minAgeTable (Age 10) False 0 AsLargeAsPossible
                       Nothing False Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  where i386 = read "i386"

openH "-" = return (Just stdout)
openH filename = do
    catch ( do
            h <- openFile filename WriteMode
            hSetBuffering h LineBuffering
            return (Just h)
        ) $ \e -> do
        hPutStrLn stderr $ "Error: Couldn't open " ++ filename ++ " for writing:\n" ++ show e
        exitFailure

parseAtom :: String -> IO Atom
parseAtom s = case BS.split '_' (BS.pack s) of
    [pkg,version,arch] | arch == "src" ->
        return $ SrcAtom $ Source (SourceName pkg) (DebianVersion version)
                       | arch == "all" ->
        return $ BinAtom $ Binary (BinName pkg) (DebianVersion version) ST.Nothing
                       | otherwise ->
        return $ BinAtom $ Binary (BinName pkg) (DebianVersion version) (ST.Just (archFromByteString arch))
    _ -> do hPutStrLn stderr $ "Error: Could not parse package name \"" ++ s ++ "\", "++
                               "expecting format name_version_arch, where arch can be src."
            exitFailure

toArchList = map archFromByteString . filter (not . BS.null) . BS.splitWith (\c -> c `elem` ", ") . BS.pack

opts =
    [ Option "d" ["dir"]
      (ReqArg (\d config -> return (config { dir = d })) "DIR")
      "directory containing britney data"
    , Option "h" ["hints-dir"]
      (ReqArg (\d config -> return (config { hintDir = Just d })) "DIR")
      "directory containing britney hints"
    , Option "a" ["arches"]
      (ReqArg (\as config -> return (config { arches = toArchList as })) "ARCH,..")
      "comma-separated list of arches to consider at all.\nDefaults to all"
    , Option "r" ["release-arches"]
      (ReqArg (\as config -> return (config { releaseArches = toArchList as })) "ARCH,...")
      "comma-separated list of arches to consider release critical.\nDefaults to all"
    , Option "" ["clauses-unrelax"]
      (ReqArg (\d config -> openH d >>= \h -> return (config { clausesUnrelaxH = h })) "FILE")
      "print literate clauses before relaxation to this file"
    , Option "" ["relaxation"]
      (ReqArg (\d config -> openH d >>= \h -> return (config { relaxationH = h })) "FILE")
      "print relaxation clauses to this file"
{-    , Option "" ["relaxation"]
      (NoArg (\config -> return (config { verboseRelaxation = True })))
      "more verbose relaxation output" -}
    , Option "" ["clauses"]
      (ReqArg (\d config -> openH d >>= \h -> return (config { clausesH = h })) "FILE")
      "print literate clauses to this file"
    , Option "" ["dimacs"]
      (ReqArg (\d config -> openH d >>= \h -> return (config { dimacsH = h })) "FILE")
      "print SAT solver input in dimacs format to this file"
    , Option "" ["difference"]
      (ReqArg (\d config -> openH d >>= \h -> return (config { differenceH = h })) "FILE")
      "print result overview to this file"
    , Option "" ["hints"]
      (ReqArg (\d config -> openH d >>= \h -> return (config { hintsH = h })) "FILE")
      "print britney2 hints to this file"
    , Option "" ["heidi"]
      (ReqArg (\d config -> openH d >>= \h -> return (config { heidiH = h })) "FILE")
      "print result in heidi format to this file"
    , Option "" ["non-candidates"]
      (ReqArg (\d config -> openH d >>= \h -> return (config { nonCandidatesH = h })) "FILE")
      "print non-candidates with explanation to this file"
    , Option "" ["stats"]
      (NoArg (\config -> return (config { showStats = True })))
      "print stats for various modelings of the problem"
    , Option "" ["offset"]
      (ReqArg (\i config -> return (config { offset = read i })) "DAYS")
      "Assume we are this many days in the future"
    , Option "" ["migrate"]
      (ReqArg (\ss config -> parseAtom ss >>= \s -> return (config { migrateThis = Just s })) "PKG")
      "find a migration containing this package.\nIf it is a source package, it ignores this package's age"
    , Option "" ["large"]
      (NoArg (\config -> return (config { transSize = AsLargeAsPossible })))
      "find a transition as large as possible (default)"
    , Option "" ["small"]
      (NoArg (\config -> return (config { transSize = AsSmallAsPossible })))
      "find a transition as small as possible (useful with --migrate)"
    , Option "" ["many-small"]
      (NoArg (\config -> return (config { transSize = ManySmall })))
      "find a large transition and split it into many small\ntransitions when printing hints"
    , Option "" ["any-size"]
      (NoArg (\config -> return (config { transSize = AnySize })))
      "find any transition (slightly faster)"
    ] 

main = do
    args <- getArgs
    name <- getProgName
    let header = "Usage: " ++ name ++ " -d DIR [OPTION...]\n"
        footer = "\nInstead of FILE, \"-\" can be used to print to the standard output.\n"
        usage = hPutStr stderr $ usageInfo header opts ++ footer
    if null args then usage else do
    case getOpt Permute opts args of
        (o,[],[] ) -> do
            config <- foldM (flip id) defaultConfig o
            runBritney config 
        (_,_,errs) -> do
            hPutStr stderr $ unlines errs
            usage 

runBritney config = do
    let ai1 = emptyIndex
    (unstable, unstableRPI, ai2) <- parseSuite config ai1 (dir config </> "unstable")
    (testing, testingRPI, ai)  <- parseSuite config ai2 (dir config </> "testing")
    hPutStrLn stderr $ "Figuring out what packages are not installable in testing:"
    uninstallable <- AM.buildM (arches config) $
        findUninstallablePackages config ai (dir config </> "testing")
    hPutStrLn stderr $ "Uninstallability counts: " ++ intercalate ", "
        [ show a ++ ": " ++ show (IxS.size s) | (a,s) <- AM.toList uninstallable]

    hPutStrLn stderr $ "AtomIndex knows about " ++ show (unIndex (maxIndex ai)) ++ " atoms."

    config <- case migrateThis config of
        Nothing -> return config
        Just atom -> case ai `indexAtom` atom of 
            Nothing -> hPutStrLn stderr ("Package " ++ show atom ++ " not known") >> exitFailure
            Just i -> return $ config { migrateThisI = Just i }

    general <- parseGeneralInfo config ai

    hints <- readHintFiles config
    hPutStrLn stderr $ "Read " ++ show (length hints) ++ " hints."
    let hintResults = processHints config ai unstable testing general hints

    let builtBy = calculateBuiltBy [testingRPI, unstableRPI]
        nonCandidates :: Producer (SrcI, String)
        nonCandidates = findNonCandidates config ai unstable testing general builtBy hintResults
        nonCandidateSet = IxS.fromList $ map fst $ build nonCandidates


    hPutStrLn stderr $ "In unstable are " ++ show (IxS.size (sources unstable `IxS.difference` sources testing)) ++ " new sources, out of which " ++ show (IxS.size nonCandidateSet) ++ " are not candidates."

    mbDo (nonCandidatesH config) $ \h -> do
        hPutStrLn h $ "The non-candidates are:"
        forM_ (build nonCandidates) $ \(src, reason) ->
            hPutStrLn h $ show (pp ai src) ++ " " ++ reason

    let transRules = transitionRules config ai unstable testing general builtBy nonCandidates
        desired = desiredAtoms unstable testing
        -- In many-small-mode, we do not try to remove packages, as
        -- that would yield far too many individual removals
        unwanted | transSize config == ManySmall = toProducer [] 
                 | otherwise        = unwantedAtoms unstable testing
        cnfTrans = {-# SCC "cnfTrans" #-} conjs2SATProb (unIndex $ maxIndex ai) $ clauses2CNF transRules

    hPutStrLn stderr $ "Running transition in happy-no-dependency-world..."
    result <- runClauseSAT (maxIndex ai) desired unwanted cnfTrans
    maxTransition <- case result of 
        Left musCNF -> do
            hPutStrLn stderr "Not even in happy world, things can migrate:"
            let mus = cnf2Clauses transRules musCNF 
            print (nest 4 (vcat (map (pp ai) (build mus))))
            exitFailure
        Right (newAtomIs) -> return $ IxS.fromDistinctAscList $ S.toAscList newAtomIs

    hPutStrLn stderr $ "Difference between testing and ideal testing:"
    differenceStats testing unstable maxTransition

    mbDo (find (`IxS.member` sources testing) (IxS.toList nonCandidateSet)) $ \atom ->
        hPutStrLn stderr $ "ERROR: " ++ show (pp ai atom) ++ " is a non-candidate in testin!"

    -- From here on, we look at dependencies

    let unmod = IxS.generalize maxTransition `IxS.intersection` binaries testing
    let piOutM = AM.build (arches config) $ \arch ->
            resolvePackageInfo config ai nonCandidateSet unmod arch [testing, unstable] [testingRPI, unstableRPI]
    let piM = AM.map fst piOutM
    let ps = mergePackageStats $ map snd (AM.elems piOutM)
    let aiD = foldr (generateInstallabilityAtoms config) ai (AM.elems piM)

    case ps of
      (PackageStats {..}) -> do
        hPrintf stderr "Out of %d binary packages, %d are unmodified, but %d are possibly affected.\n"
            (IxS.size $ binaries unstable `IxS.union` binaries testing)
            (IxS.size unmod)
            (IxS.size $ IxS.unions $ map affected $ AM.elems piM)

        hPutStrLn stderr $ "The conflicts affecting most packages are:"
        mapM_ (hPutStrLn stderr) 
            [ "   " ++ show (pp ai c1) ++ " -#- " ++ show (pp ai c2) ++ " (" ++ show i ++ " packages)"
            | ((c1,c2),i) <- histogramToList 10 conflictHistogram ]

        hPutStrLn stderr $ "The packages appearing in most sets of relevant dependencies are:"
        mapM_ (hPutStrLn stderr) 
            [ "   " ++ show (pp ai p) ++ " (" ++ show i ++ " packages)"
            | (p,i) <- histogramToList 10 relevantDepHistogram ]

        when (showStats config) $ do
            let binCount = sum $ map (IxS.size . relevantBins) $ AM.elems piM
                depCount = sum $ map (sum . map length . IxM.elems .depends) $ AM.elems piM

            {-
            hPrintf stderr "Non-conflict encoding: %d atoms and %d clauses\n" binCount depCount

            hPrintf stderr "Naive encoding (calculated): %d atoms and %d clauses\n" 
                (binCount^2)
                (binCount * depCount)

            hPrintf stderr "Encoding considering cones: %d atoms and %d clauses\n" 
                (binCount + (sum $ map IxS.size $ IxM.elems $ transitiveHull (dependsRel pi)))
                (sum $ map (length . (depends pi IxM.!)) $ concatMap IxS.toList $ IxM.elems $ transitiveHull (dependsRel pi))

            hPrintf stderr "Encoding considering easy packages: %d atoms and %d clauses\n" 
                (binCount + (sum $ map IxS.size $ map (IxS.filter (not . (`IxS.member` hasConflictInDeps pi))) $ IxM.elems $ transitiveHull (dependsRel pi)))
                (sum $ map (length . (depends pi IxM.!)) $ concatMap IxS.toList $ map (IxS.filter (not . (`IxS.member` hasConflictInDeps pi))) $ IxM.elems $ transitiveHull (dependsRel pi))
            -}

            hPrintf stderr "Encoding considering only relevant conflicts/dependencies: %d atoms and %d clauses\n" 
                (binCount + (sum $ map (sum . map IxS.size . IxM.elems . dependsBadHull) $ AM.elems piM))
                (sum $ map (\pi -> sum . map (length . (depends pi IxM.!)) . concatMap IxS.toList . IxM.elems . dependsBadHull $ pi) $ AM.elems piM)

        hPrintf stderr "A total of %d packages take part in %d conflicts, %d have conflicts in dependencies, of which %d have bad conflicts.\n"
            (IxS.size $ hasConflict)
            (sum $ map (sum . map (length . concatMap fst) . IxM.elems . conflicts) $ AM.elems piM)
            (IxS.size $ hasConflictInDeps)
            (sum $ map (IxM.size . dependsBadHull) $ AM.elems piM)

        hPrintf stderr "Size of the relevant dependency hulls: %d\n"
            (sum $ map (sum . map IxS.size . IxM.elems . dependsBadHull) $ AM.elems piM)

    hPutStrLn stderr $ "After adding installability atoms, AtomIndex knows about " ++ show (unIndex (maxIndex aiD)) ++ " atoms."

    let depRules = unionMapP
            (\pi -> dependencyRules config aiD (uninstallable AM.! piArch pi) pi) $
            AM.elems piM 
        rulesT = mapP (\i -> Not i "we are investigating testing") desired `concatP`
                 mapP (\i -> OneOf [i] "we are investigating testing") unwanted `concatP`
                 depRules

    hPutStrLn stderr $ "Constructed " ++ show (length (build rulesT)) ++ " clauses, with " ++ show (length (build desired)) ++ " desired and " ++ show (length (build unwanted)) ++ " unwanted atoms."

    let extraRules = maybe [] (\si -> [OneOf [si] "it was requested"]) (migrateThisI config)
        amendedRules = toProducer extraRules `concatP` depRules
        sp = conjs2SATProb (unIndex $ maxIndex aiD)
                (clauses2CNF amendedRules `combineCNF` required cnfTrans)

    mbDo (dimacsH config) $ \h -> do
        hPutStrLn stderr $ "Writing SAT problem im DIMACS problem"
        L.hPut h $ formatCNF sp
        hFlush h

    mbDo (clausesH config) $ \h -> do
        hPutStrLn stderr $ "Writing SAT problem as literal clauses"
        mapM_ (hPrint h . nest 4 . pp aiD) (build transRules)
        mapM_ (hPrint h . nest 4 . pp aiD) (build amendedRules)
        hFlush h

    {-
    hPutStrLn stderr $ "Desired packages:"
    hPrint stderr $ nest 4 (vcat (map (pp aiD) desired))
    -}

    let (desired', unwanted') = case transSize config of
            AsLargeAsPossible -> (desired, unwanted)
            ManySmall         -> (desired, unwanted)
            AsSmallAsPossible -> (unwanted, desired)
            AnySize           -> (toProducer [], toProducer [])

    hPutStrLn stderr $ "Running main picosat run"
    result <- if transSize config == ManySmall
        then runClauseMINMAXSAT (maxIndex aiD) desired' unwanted' sp
        else fmap (\res -> (res,error "smallTransitions only exists when transSize == ManySmall")) <$>
             runClauseSAT (maxIndex aiD) desired' unwanted' sp
    case result of 
        Left musCNF -> do
            hPutStrLn stderr $
                "No suitable set of packages could be determined, " ++
                "because the following requirements conflict:"
            let mus = cnf2Clauses (transRules `concatP` amendedRules) musCNF 
            unless (isJust (migrateThis config)) $ do
                hPutStrLn stderr "(This should not happen, as this is detected earlier)"
            print (nest 4 (vcat (map (pp aiD) (build mus))))
        Right (newAtomIs,smallTransitions) -> do

            let newAtomIis = IxS.fromDistinctAscList (S.toList newAtomIs)
            hPutStrLn stderr $ "Difference between testing and new testing:"
            differenceStats testing unstable newAtomIis

            let unmodMissing = unmod `IxS.difference` IxS.generalize newAtomIis
            unless (IxS.null unmodMissing) $ do
                hPutStrLn stderr $ "Something was wrong with my assumptions, these binaries were expected not to be modified, but are now missing in testing:"
                hPutStrLn stderr $ show $ nest 4 $ fsep $ punctuate comma $ map (pp ai) $ IxS.toList unmodMissing

            mbDo (differenceH config) $ \h -> do
                let newAtoms = S.map (aiD `lookupAtom`) newAtomIs
                let (newSource, newBinaries, _) = splitAtoms newAtoms
                hPutStrLn h "Changes of Sources:"
                printDifference h (setMap (aiD `lookupSrc`) $ sources testing) newSource
                hPutStrLn h "Changes of Package:"
                printDifference h (setMap (aiD `lookupBin`) $ binaries testing) newBinaries
                hFlush h

            mbDo (hintsH config) $ \h -> do
                if transSize config == ManySmall
                then do
                    hPutStrLn h $ "# Full hint:"
                    L.hPut h $ generateHints aiD testing unstable builtBy newAtomIs
                    hPutStrLn h $ "# Small hints:"
                    forM_ smallTransitions $ \thisTransitionNewAtomsIs-> 
                        L.hPut h $ generateHints aiD testing unstable builtBy thisTransitionNewAtomsIs
                    hFlush h
                else do
                    L.hPut h $ generateHints aiD testing unstable builtBy newAtomIs
                    hFlush h

            mbDo (heidiH config) $ \h -> do
                L.hPut h $ generateHeidi aiD newAtomIs

    hPutStrLn stderr $ "Done"
    
splitAtoms = (\(l1,l2,l3) -> (S.fromList l1, S.fromList l2, S.fromList l3)) .
             S.fold select ([],[],[])
  where select (SrcAtom x) ~(l1,l2,l3) = (x:l1,l2,l3)
        select (BinAtom x) ~(l1,l2,l3) = (l1,x:l2,l3)
        select (BugAtom x) ~(l1,l2,l3) = (l1,l2,x:l3)
        select _ x = x

setMap f = S.fromList . map f . IxS.toList

differenceStats :: SuiteInfo -> SuiteInfo -> IxS.Set Atom -> IO ()
differenceStats testing unstable newAtoms = do
    let newAtomsSrc = IxS.generalize newAtoms `IxS.intersection` (sources testing `IxS.union` sources unstable)
    let newAtomsBin = IxS.generalize newAtoms `IxS.intersection` (binaries testing `IxS.union` binaries unstable)

    hPutStrLn stderr $ "  " ++
        show (IxS.size (newAtomsSrc `IxS.difference` sources testing)) ++ " sources added, " ++ 
        show (IxS.size (sources testing `IxS.difference` newAtomsSrc)) ++ " sources removed, " ++
        show (IxS.size (sources testing `IxS.intersection` newAtomsSrc)) ++ " sources remain."

    hPutStrLn stderr $ "  " ++
        show (IxS.size (newAtomsBin `IxS.difference` binaries testing)) ++ " binaries added, " ++ 
        show (IxS.size (binaries testing `IxS.difference` newAtomsBin)) ++ " binaries removed, " ++
        show (IxS.size (binaries testing `IxS.intersection` newAtomsBin)) ++ " binaries remain."

printDifference :: (Show a, Ord a) => Handle -> S.Set a -> S.Set a -> IO ()
printDifference h old new = do
    {-
    putStrLn "New state"
    forM_ (S.toList new) $ \x -> putStrLn $ "    " ++ show x
    -}
    let added = new `S.difference` old
    hPutStrLn h "Newly added:"
    forM_ (S.toList added) $ \x -> hPutStrLn h $ "    " ++ show x
    let removed = old `S.difference` new
    hPutStrLn h "Removed:"
    forM_ (S.toList removed) $ \x -> hPutStrLn h $ "    " ++ show x


removeRelated l1 l2 = filter check l1
 where  si = S.fromList [ (atom, reason) | Implies atom _ reason <- l2 ]
        sa = S.fromList [ (a1,a2) | NotBoth a1 a2 _ <- l2 ]
        check c@(Implies atom _ reason) = (atom, reason) `S.notMember` si
        check (NotBoth a1 a2 _) = (a1, a2) `S.notMember` sa
        check _ = True
