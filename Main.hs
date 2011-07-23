import System.Environment
import System.FilePath
import Text.PrettyPrint
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad

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
import Picosat
import LitSat

config = Config all all i386
  where all = [ i386 {- , amd64 -} ]
        i386 = Arch (BS.pack "i386")
        amd64 = Arch (BS.pack "amd64")

main = do
    args <- getArgs
    case args of
        [] -> printUsage
        [dir] -> runBritney dir

runBritney dir = do
    unstable <- parseSuite config (dir </> "unstable")
    testing <- parseSuite config (dir </> "testing")

    let (rulesT, relaxable) = transitionRules config testing testing
        idxT = allAtoms rulesT
        cnfT = clauses2CNF idxT rulesT
        relaxableClauses = clauses2CNF idxT relaxable
    
    removeCnf <- relaxer (M.keysSet relaxableClauses) (M.keys cnfT)
    let removeClause = cnf2Clause cnfT removeCnf
    putStrLn $ "The following " ++ show (length removeClause) ++ " clauses are removed to make testing conform:"
    print (nest 4 (vcat (map pp removeClause)))

    let (rules, _) = transitionRules config unstable testing
        cleanedRules = rules `removeRelated` removeClause
        idx = allAtoms cleanedRules
        cnf = clauses2CNF idx cleanedRules

    result <- runPicosat idx cnf
    case result of 
        Left clauses -> do
            putStrLn "No suitable set of packages could be determined,"
            putStrLn "because the following requirements conflict:"
            -- print (vcat (map pp clauses))
            mapM_ print clauses
        Right newAtoms -> do
            let (newSource, newBinaries, _) = splitAtoms newAtoms
            putStrLn "Changes of Sources:"
            printDifference (sources testing) newSource
            putStrLn "Changes of Package:"
            printDifference (binaries testing) newBinaries
    
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

printUsage = do
    name <- getProgName
    putStrLn ("Usage: " ++ name ++ " dir_to_data/")

removeRelated l1 l2 = filter check l1
 where  s = S.fromList [ (atom, reason) | Implies atom _ reason <- l2 ]
        check (Implies atom _ reason) = (atom, reason) `S.notMember` s
        check _ = True
