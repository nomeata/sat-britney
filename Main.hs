{-# LANGUAGE OverloadedStrings #-}

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
import Control.Monad
import System.IO
import System.Console.GetOpt
import System.Exit
import Data.Functor
import Data.Maybe

import qualified IndexSet as IxS

import ParseSuite
import TransRules
import Types
import PrettyPrint
import ClauseSat
import Picosat
import LitSat
import Hints

minAgeTable = M.fromList [
    (Urgency "low", Age 10), 
    (Urgency "medium", Age 5),
    (Urgency "high", Age 2),
    (Urgency "critical", Age 2),
    (Urgency "emergency", Age 2)
    ]

defaultConfig :: Config
defaultConfig = Config "." allArches allArches i386 minAgeTable (Age 10) False AsLargeAsPossible
                       Nothing False Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  where i386 = Arch "i386"

allArches = map (Arch . BS.pack) $ words
    "i386 sparc powerpc armel ia64 mips mipsel s390 amd64 kfreebsd-i386 kfreebsd-amd64"

openH "-" = return (Just stdout)
openH filename = do
    catch (Just <$> openFile filename WriteMode) $ \e -> do
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
    , Option "" ["full-dependencies"]
      (NoArg (\config -> return (config { fullDependencies = True })))
      "model dependency graph per package"
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
        (_,_,errs) -> usage 

runBritney config = do
    let ai1 = emptyIndex
    (unstableFull, ai2) <- parseSuite config ai1 (dir config </> "unstable")
    (testing, ai3)  <- parseSuite config ai2 (dir config </> "testing")

    config <- case migrateThis config of
        Nothing -> return config
        Just atom -> case ai3 `indexAtom` atom of 
            Nothing -> hPutStrLn stderr ("Package " ++ show atom ++ " not known") >> exitFailure
            Just i -> return $ config { migrateThisI = Just i }

    general <- parseGeneralInfo config ai3

    let unstableThin = thinSuite config ai3 unstableFull general
    hPutStrLn stderr $ "Thinning unstable to " ++ show (IxS.size (sources unstableThin)) ++
        " sources and " ++ show (IxS.size (binaries unstableThin)) ++ " binaries."

    let (rules, relaxable, desired, unwanted, ai) = transitionRules config ai3 unstableThin testing general
        rulesT = map (\i -> Not i "we are investigating testing") desired ++
                 map (\i -> OneOf [i] "we are investigating testing") unwanted ++
                 rules
        cnfT = clauses2CNF (maxIndex ai) rulesT
        relaxableClauses = clauses2CNF (maxIndex ai) relaxable

    hPutStrLn stderr $ "Constructed " ++ show (length rulesT) ++ " hard and " ++
        show (length relaxable) ++ " soft clauses."

    mbDo (clausesUnrelaxH config) $ \h -> do
        hPutStrLn stderr $ "Writing unrelaxed SAT problem as literal clauses"
        hPrint h $ nest 4 (vcat (map (pp ai) rulesT))
        hPutStrLn h ""
        hPrint h $ nest 4 (vcat (map (pp ai) relaxable))
        hFlush h

    
    hPutStrLn stderr $ "Relaxing testing to a consistent set..."
    removeClauseE <- runRelaxer (maxIndex ai) relaxableClauses cnfT
    removeClause <- case removeClauseE of
        Left mus -> do
            hPutStrLn stderr $ "The following unrelaxable clauses are conflicting in testing:"
            hPrint stderr $ nest 4 (vcat (map (pp ai) mus))
            exitFailure
        Right removeClause -> do
            hPutStrLn stderr $ show (length removeClause) ++ " clauses are removed to make testing conform"
            mbDo (relaxationH config) $ \h -> do
                hPrint h $ nest 4 (vcat (map (pp ai) removeClause))
                hFlush h
            return removeClause


    let extraRules = maybe [] (\si -> [OneOf [si] "it was requested"]) (migrateThisI config)
        cleanedRules = extraRules ++ rules ++ (relaxable `removeRelated` removeClause)
        cnf = clauses2CNF (maxIndex ai) cleanedRules

    mbDo (dimacsH config) $ \h -> do
        hPutStrLn stderr $ "Writing SAT problem im DIMACS problem"
        L.hPut h $ formatCNF cnf
        hFlush h

    mbDo (clausesH config) $ \h -> do
        hPutStrLn stderr $ "Writing SAT problem as literal clauses"
        hPrint h $ nest 4 (vcat (map (pp ai) cleanedRules))
        hFlush h

    {-
    hPutStrLn stderr $ "Desired packages:"
    hPrint stderr $ nest 4 (vcat (map (pp ai) desired))
    -}

    let (desired', unwanted') = case transSize config of
            AsLargeAsPossible -> (desired, unwanted)
            ManySmall         -> (desired, unwanted)
            AsSmallAsPossible -> (unwanted, desired)
            AnySize           -> ([], [])

    hPutStrLn stderr $ "Running main picosat run"
    result <- if transSize config == ManySmall
        then runClauseMINMAXSAT (maxIndex ai) desired' unwanted' cnf
        else fmap (\res -> (res,[res])) <$>
             runClauseSAT (maxIndex ai) desired' unwanted' cnf
    case result of 
        Left clauses -> do
            hPutStrLn stderr $
                "No suitable set of packages could be determined, " ++
                "because the following requirements conflict:"
            unless (isJust (migrateThis config)) $ do
                hPutStrLn stderr "(This should not happen, as this is detected earlier)"
            print (nest 4 (vcat (map (pp ai) clauses)))
        Right (newAtomIs,smallTransitions) -> do
            mbDo (differenceH config) $ \h -> do
                let newAtoms = S.map (ai `lookupAtom`) newAtomIs
                let (newSource, newBinaries, _) = splitAtoms newAtoms
                hPutStrLn h "Changes of Sources:"
                printDifference h (setMap (ai `lookupSrc`) $ sources testing) newSource
                hPutStrLn h "Changes of Package:"
                printDifference h (setMap (ai `lookupBin`) $ binaries testing) newBinaries
                hFlush h

            mbDo (hintsH config) $ \h -> do
                forM_ smallTransitions $ \thisTransitionNewAtomsIs-> 
                    L.hPut h $ generateHints ai testing unstableThin thisTransitionNewAtomsIs
                hFlush h

    hPutStrLn stderr $ "Done"
    
splitAtoms = (\(l1,l2,l3) -> (S.fromList l1, S.fromList l2, S.fromList l3)) .
             S.fold select ([],[],[])
  where select (SrcAtom x) ~(l1,l2,l3) = (x:l1,l2,l3)
        select (BinAtom x) ~(l1,l2,l3) = (l1,x:l2,l3)
        select (BugAtom x) ~(l1,l2,l3) = (l1,l2,x:l3)

setMap f = S.fromList . map f . IxS.toList

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
