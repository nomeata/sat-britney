{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import System.FilePath
import Text.PrettyPrint
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad
import System.IO
import System.Console.GetOpt

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
import LitSat

minAgeTable = M.fromList [
    (Urgency "low", Age 10), 
    (Urgency "medium", Age 5),
    (Urgency "high", Age 2),
    (Urgency "critical", Age 2),
    (Urgency "emergency", Age 2)
    ]

defaultConfig = Config "." [i386] [i386] i386 minAgeTable (Age 10)
                       Nothing False Nothing Nothing Nothing
  where i386 = Arch "i386"

opts =
    [ Option "d" ["dir"]
      (ReqArg (\d config -> config { dir = d }) "DIR")
      "directory containing britney data"
    ] 

main = do
    args <- getArgs
    name <- getProgName
    let header = "Usage: " ++ name ++ " [OPTION...]"
    case getOpt Permute opts args of
        (o,[],[] ) -> do
            let config = foldl (flip id) defaultConfig o
            runBritney config 
        (_,_,errs) -> do
            hPutStr stderr $ concat errs ++ usageInfo header opts

runBritney config = do
    unstable <- parseSuite config (dir config </> "unstable")
    testing <- parseSuite config (dir config </> "testing")

    let (rulesT, relaxable) = transitionRules config testing testing
        idxT = allAtoms rulesT
        cnfT = clauses2CNF idxT rulesT
        relaxableClauses = clauses2CNF idxT relaxable
    
    hPutStrLn stderr $ "Relaxing testing to a consistent set..."
    removeClauseE <- runRelaxer relaxableClauses cnfT
    removeClause <- case removeClauseE of
        Left mus -> do
            hPutStrLn stderr $ "The following unrelaxable clauses are conflicting in testing:"
            hPrint stderr $ nest 4 (vcat (map pp mus))
            return []
        Right removeClause -> do
            putStrLn $ "The following " ++ show (length removeClause) ++ " clauses are removed to make testing conform:"
            print (nest 4 (vcat (map pp removeClause)))
            return removeClause


    let (rules, _) = transitionRules config unstable testing
        cleanedRules = rules `removeRelated` removeClause
        idx = allAtoms cleanedRules
        cnf = clauses2CNF idx cleanedRules

    hPutStrLn stderr $ "Running main picosat run"
    result <- runPicosat idx cnf
    case result of 
        Left clauses -> do
            putStrLn "No suitable set of packages could be determined,"
            putStrLn "because the following requirements conflict:"
            print (nest 4 (vcat (map pp removeClause)))
        Right newAtoms -> do
            let (newSource, newBinaries, _) = splitAtoms newAtoms
            putStrLn "Changes of Sources:"
            printDifference (sources testing) newSource
            putStrLn "Changes of Package:"
            printDifference (binaries testing) newBinaries
    hPutStrLn stderr $ "Done"
    
splitAtoms = (\(l1,l2,l3) -> (S.fromList l1, S.fromList l2, S.fromList l3)) .
             S.fold select ([],[],[])
  where select (SrcAtom x) ~(l1,l2,l3) = (x:l1,l2,l3)
        select (BinAtom x) ~(l1,l2,l3) = (l1,x:l2,l3)
        select (BugAtom x) ~(l1,l2,l3) = (l1,l2,x:l3)

printDifference :: (Show a, Ord a) => S.Set a -> S.Set a -> IO ()
printDifference old new = do
    {-
    putStrLn "New state"
    forM_ (S.toList new) $ \x -> putStrLn $ "    " ++ show x
    -}
    let added = new `S.difference` old
    putStrLn "Newly added:"
    forM_ (S.toList added) $ \x -> putStrLn $ "    " ++ show x
    let removed = old `S.difference` new
    putStrLn "Removed:"
    forM_ (S.toList removed) $ \x -> putStrLn $ "    " ++ show x


removeRelated l1 l2 = filter check l1
 where  s = S.fromList [ (atom, reason) | Implies atom _ reason <- l2 ]
        check (Implies atom _ reason) = (atom, reason) `S.notMember` s
        check _ = True
