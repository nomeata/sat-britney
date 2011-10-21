{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

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

import ParseSuite
import TransRules
import DepRules
import Types
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
defaultConfig = Config "." Nothing allArches allArches i386 minAgeTable (Age 10) False AsLargeAsPossible
                       Nothing False Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  where i386 = Arch "i386"

allArches = map (Arch . BS.pack) $ words
    "i386 sparc powerpc armel ia64 mips mipsel s390 amd64 kfreebsd-i386 kfreebsd-amd64"

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
        return $ BinAtom $ Binary (BinName pkg) (DebianVersion version) (ST.Just (Arch arch))
    _ -> do hPutStrLn stderr $ "Error: Could not parse package name \"" ++ s ++ "\", "++
                               "expecting format name_version_arch, where arch can be src."
            exitFailure

toArchList = map Arch . filter (not . BS.null) . BS.splitWith (\c -> c `elem` ", ") . BS.pack

opts =
    [ Option "d" ["dir"]
      (ReqArg (\d config -> return (config { dir = d })) "DIR")
      "directory containing britney data"
    , Option "h" ["hints-dir"]
      (ReqArg (\d config -> return (config { hintDir = Just d })) "DIR")
      "directory containing britney data"
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
    , Option "" ["stats"]
      (NoArg (\config -> return (config { showStats = True })))
      "print stats for various modelings of the problem"
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

    hPutStrLn stderr $ "AtomIndex knows about " ++ show (unIndex (maxIndex ai)) ++ " atoms."

    config <- case migrateThis config of
        Nothing -> return config
        Just atom -> case ai `indexAtom` atom of 
            Nothing -> hPutStrLn stderr ("Package " ++ show atom ++ " not known") >> exitFailure
            Just i -> return $ config { migrateThisI = Just i }

    general <- parseGeneralInfo config ai

    {-
    let (unstableThin, unstableThinRPI) = thinSuite config unstable unstableRPI general
    hPutStrLn stderr $ "Thinning unstable to " ++ show (IxS.size (sources unstableThin)) ++
        " sources and " ++ show (IxS.size (binaries unstableThin)) ++ " binaries."
    -}

    hints <- readHintFiles config
    hPutStrLn stderr $ "Read " ++ show (length hints) ++ " hints."
    let hintResults = processHints config ai unstable testing general hints

    let builtBy = calculateBuiltBy [testingRPI, unstableRPI]
        nonCandidates :: Producer (SrcI, String)
        nonCandidates = findNonCandidates config ai unstable testing general builtBy hintResults
        nonCandidateSet = IxS.fromList $ map fst $ build nonCandidates


    hPutStrLn stderr $ "In unstable are " ++ show (IxS.size (sources unstable `IxS.difference` sources testing)) ++ " new sources, out of which " ++ show (IxS.size nonCandidateSet) ++ " are not candidates."


    let transRules = transitionRules config ai unstable testing general builtBy nonCandidates
        desired = desiredAtoms unstable testing
        unwanted = unwantedAtoms unstable testing
        cnfTrans = clauses2CNF (maxIndex ai) transRules

    hPutStrLn stderr $ "Running transition in happy-no-dependency-world..."
    result <- runClauseSAT (maxIndex ai) (build desired) (build unwanted) cnfTrans
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
    let PackageInfoOut{..} = resolvePackageInfo config ai nonCandidateSet unmod [testing, unstable] [testingRPI, unstableRPI]
    let aiD = generateInstallabilityAtoms config (PackageInfoIn{..}) ai

    hPutStrLn stderr $ "Out of " ++ show (IxS.size (binaries unstable `IxS.union` binaries testing)) ++ " binary packages, " ++ show (IxS.size unmod) ++ " are unmodified, but " ++ show (IxS.size affected) ++ " are possibly affected."


    when (showStats config) $ do
        let binCount = IxM.size depends
            depCount = sum $ map length $ IxM.elems depends

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
            (binCount + (sum $ map IxS.size $ IxM.elems $ dependsBadHull))
            (sum $ map (length . (depends IxM.!)) $ concatMap IxS.toList $ IxM.elems $ dependsBadHull)

    hPutStrLn stderr $ "A total of " ++ show (IxS.size hasConflict) ++ " packages take part in " ++ show (sum $ map (length . concatMap fst) $ IxM.elems $ conflicts) ++ " conflicts, " ++ show (IxS.size hasConflictInDeps) ++ " have conflicts in dependencies, of which " ++ show (IxM.size dependsBadHull) ++ " have bad conflicts."

    hPutStrLn stderr $ "Size of the relevant dependency hulls: " ++ show 
        (sum $ map IxS.size $ IxM.elems $ dependsBadHull)

    hPutStrLn stderr $ "After adding installability atoms, AtomIndex knows about " ++ show (unIndex (maxIndex aiD)) ++ " atoms."

    let depHard = hardDependencyRules config aiD (PackageInfoIn{..})
        depSoft = softDependencyRules config aiD (PackageInfoIn{..})
        relaxable = depSoft
        rulesT = mapP (\i -> Not i "we are investigating testing") desired `concatP`
                 mapP (\i -> OneOf [i] "we are investigating testing") unwanted `concatP`
                 depHard
        cnfT = clauses2CNF (maxIndex aiD) rulesT `combineCNF` cnfTrans
        relaxableClauses = clauses2CNF (maxIndex aiD) relaxable

    hPutStrLn stderr $ "Constructed " ++ show (length (build rulesT)) ++ " hard and " ++
        show (length (build relaxable)) ++ " soft clauses, with " ++ show (length (build desired)) ++ " desired and " ++ show (length (build unwanted)) ++ " unwanted atoms."

    mbDo (clausesUnrelaxH config) $ \h -> do
        hPutStrLn stderr $ "Writing unrelaxed SAT problem as literal clauses"
        mapM_ (hPrint h . nest 4 . pp aiD) (build transRules)
        hPutStrLn h ""
        mapM_ (hPrint h . nest 4 . pp aiD) (build rulesT)
        hPutStrLn h ""
        mapM_ (hPrint h . nest 4 . pp aiD) (build relaxable)
        hFlush h
    
    hPutStrLn stderr $ "Relaxing testing to a consistent set..."
    removeClauseE <- relaxer relaxableClauses cnfT
    leftConj <- case removeClauseE of
        Left musCNF -> do
            hPutStrLn stderr $ "The following unrelaxable clauses are conflicting in testing:"
            let mus = cnf2Clauses relaxable musCNF 
            hPrint stderr $ nest 4 (vcat (map (pp aiD) (build mus)))
            exitFailure
        Right (leftConj,removeConjs) -> do
            hPutStrLn stderr $ show (V.length (fst removeConjs)) ++ " clauses are removed to make testing conform"
            mbDo (relaxationH config) $ \h -> do
                let removeClause = cnf2Clauses relaxable removeConjs 
                hPrint h $ nest 4 (vcat (map (pp aiD) (build removeClause)))
                hFlush h
            return leftConj


    let extraRules = maybe [] (\si -> [OneOf [si] "it was requested"]) (migrateThisI config)
        cleanedRules = toProducer extraRules `concatP` depHard
        cnf = clauses2CNF (maxIndex aiD) cleanedRules `combineCNF` leftConj `combineCNF` cnfTrans

    mbDo (dimacsH config) $ \h -> do
        hPutStrLn stderr $ "Writing SAT problem im DIMACS problem"
        L.hPut h $ formatCNF cnf
        hFlush h

    mbDo (clausesH config) $ \h -> do
        hPutStrLn stderr $ "Writing SAT problem as literal clauses"
        mapM_ (hPrint h . nest 4 . pp aiD) (build transRules)
        mapM_ (hPrint h . nest 4 . pp aiD) (build cleanedRules)
        mapM_ (hPrint h . nest 4 . pp aiD) (build (cnf2Clauses relaxable leftConj))
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
        then runClauseMINMAXSAT (maxIndex aiD) (build desired') (build unwanted') cnf
        else fmap (\res -> (res,[res])) <$>
             runClauseSAT (maxIndex aiD) (build desired') (build unwanted') cnf
    case result of 
        Left musCNF -> do
            hPutStrLn stderr $
                "No suitable set of packages could be determined, " ++
                "because the following requirements conflict:"
            let mus = cnf2Clauses (transRules `concatP` cleanedRules `concatP` relaxable) musCNF 
            unless (isJust (migrateThis config)) $ do
                hPutStrLn stderr "(This should not happen, as this is detected earlier)"
            print (nest 4 (vcat (map (pp aiD) (build mus))))
        Right (newAtomIs,smallTransitions) -> do

            let newAtomIis = IxS.fromDistinctAscList (S.toList newAtomIs)
            hPutStrLn stderr $ "Difference between testing and new testing:"
            differenceStats testing unstable newAtomIis

            mbDo (differenceH config) $ \h -> do
                let newAtoms = S.map (aiD `lookupAtom`) newAtomIs
                let (newSource, newBinaries, _) = splitAtoms newAtoms
                hPutStrLn h "Changes of Sources:"
                printDifference h (setMap (aiD `lookupSrc`) $ sources testing) newSource
                hPutStrLn h "Changes of Package:"
                printDifference h (setMap (aiD `lookupBin`) $ binaries testing) newBinaries
                hFlush h

            mbDo (hintsH config) $ \h -> do
                forM_ smallTransitions $ \thisTransitionNewAtomsIs-> 
                    L.hPut h $ generateHints aiD testing unstable builtBy thisTransitionNewAtomsIs
                hFlush h

            mbDo (heidiH config) $ \h -> do
                L.hPut h $ generateHeidi aiD newAtomIs

    hPutStrLn stderr $ "Done"
    
splitAtoms = (\(l1,l2,l3) -> (S.fromList l1, S.fromList l2, S.fromList l3)) .
             S.fold select ([],[],[])
  where select (SrcAtom x) ~(l1,l2,l3) = (x:l1,l2,l3)
        select (BinAtom x) ~(l1,l2,l3) = (l1,x:l2,l3)
        select (BugAtom x) ~(l1,l2,l3) = (l1,l2,x:l3)

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
