{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import System.FilePath
import Text.PrettyPrint
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as BS
import Control.Monad
import System.IO
import System.Console.GetOpt
import System.Exit
import Data.Functor

import Debian.Control
import Debian.Control.ByteString
import Debian.Relation
import Debian.Relation.ByteString
import Debian.Version
import Debian.Version.ByteString

import ParseSuite
import TransRules
import Types
import PrettyPrint
import ClauseSat
import Picosat
import LitSat

minAgeTable = M.fromList [
    (Urgency "low", Age 10), 
    (Urgency "medium", Age 5),
    (Urgency "high", Age 2),
    (Urgency "critical", Age 2),
    (Urgency "emergency", Age 2)
    ]

defaultConfig = Config "." allArches allArches i386 minAgeTable (Age 10)
                       Nothing False Nothing Nothing Nothing
  where i386 = Arch "i386"

allArches = map (Arch . BS.pack) $ words
    "i386 sparc powerpc armel ia64 mips mipsel s390 amd64 kfreebsd-i386 kfreebsd-amd64"

openH "-" = return (Just stdout)
openH filename = do
    catch (Just <$> openFile filename WriteMode) $ \e -> do
        hPutStr stderr $
            "Error: Couldn't open " ++ filename ++ " for writing:\n" ++ show e ++ "\n"
        exitFailure
        return undefined

toArchList = map Arch . BS.split ',' . BS.pack

opts =
    [ Option "d" ["dir"]
      (ReqArg (\d config -> return (config { dir = d })) "DIR")
      "directory containing britney data"
    , Option "a" ["arches"]
      (ReqArg (\as config -> return (config { arches = toArchList as })) "ARCH1,ARCH2")
      "comma-separated list of arches to consider at all. Defaults to all"
    , Option "r" ["release-arches"]
      (ReqArg (\as config -> return (config { releaseArches = toArchList as })) "ARCH1,ARCH2")
      "comma-separated list of arches to consider release critical. Defaults to all"
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
    ] 

main = do
    args <- getArgs
    name <- getProgName
    let header = "Usage: " ++ name ++ " -d DIR [OPTION...]"
        footer = "Instead of FILE, \"-\" can be used to print to the standard output.\n"
        usage = hPutStr stderr $ usageInfo header opts ++ footer
    if null args then usage else do
    case getOpt Permute opts args of
        (o,[],[] ) -> do
            config <- foldM (flip id) defaultConfig o
            runBritney config 
        (_,_,errs) -> usage 

runBritney config = do
    let ai1 = emptyIndex
    (unstable, ai2) <- parseSuite config ai1 (dir config </> "unstable")
    (testing, ai)  <- parseSuite config ai2 (dir config </> "testing")
    general <- parseGeneralInfo config ai

    let (rulesT, relaxable) = transitionRules config ai testing testing general
        cnfT = clauses2CNF rulesT
        relaxableClauses = clauses2CNF relaxable
    
    hPutStrLn stderr $ "Relaxing testing to a consistent set..."
    removeClauseE <- runRelaxer relaxableClauses cnfT
    removeClause <- case removeClauseE of
        Left mus -> do
            hPutStrLn stderr $ "The following unrelaxable clauses are conflicting in testing:"
            hPrint stderr $ nest 4 (vcat (map pp mus))
            exitFailure
            return []
        Right removeClause -> do
            mbDo (relaxationH config) $ \h -> do
                hPutStrLn h $ "The following " ++ show (length removeClause) ++ " clauses are removed to make testing conform:"
                hPrint h $ nest 4 (vcat (map pp removeClause))
            return removeClause


    let (rules, _) = transitionRules config ai unstable testing general
        cleanedRules = rules `removeRelated` removeClause
        cnf = clauses2CNF cleanedRules

    mbDo (dimacsH config) $ \h -> do
        hPutStrLn stderr $ "Writing SAT problem im DIMACS problem"
        hPutStr h $ formatCNF (onlyCNF cnf)

    mbDo (clausesH config) $ \h -> do
        hPutStrLn stderr $ "Writing SAT problem as literal clauses"
        hPrint h $ nest 4 (vcat (map pp cleanedRules))

    hPutStrLn stderr $ "Running main picosat run"
    result <- runClauseSAT cnf
    case result of 
        Left clauses -> do
            putStrLn "No suitable set of packages could be determined,"
            putStrLn "because the following requirements conflict:"
            putStrLn "(This should not happen, as this is detected earlier)"
            print (nest 4 (vcat (map pp clauses)))
        Right newAtomIs -> do
            mbDo (differenceH config) $ \h -> do
                let newAtoms = S.map (ai `lookupAtom`) newAtomIs
                let (newSource, newBinaries, _) = splitAtoms newAtoms
                hPutStrLn h "Changes of Sources:"
                printDifference h (S.map (ai `lookupSrc`) $ sources testing) newSource
                hPutStrLn h "Changes of Package:"
                printDifference h (S.map (ai `lookupBin`) $ binaries testing) newBinaries
    hPutStrLn stderr $ "Done"
    
splitAtoms = (\(l1,l2,l3) -> (S.fromList l1, S.fromList l2, S.fromList l3)) .
             S.fold select ([],[],[])
  where select (SrcAtom x) ~(l1,l2,l3) = (x:l1,l2,l3)
        select (BinAtom x) ~(l1,l2,l3) = (l1,x:l2,l3)
        select (BugAtom x) ~(l1,l2,l3) = (l1,l2,x:l3)

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
 where  s = S.fromList [ (atom, reason) | Implies atom _ reason <- l2 ]
        check (Implies atom _ reason) = (atom, reason) `S.notMember` s
        check _ = True
