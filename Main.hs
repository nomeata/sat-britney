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
    let rules = transitionRules config unstable testing
    -- print (vcat (map pp rules))
    let idx = allAtoms rules
    let cnf = clauses2CNF idx rules
    --sequence_ [ print (text (unwords (map show l)) <+> char ':' <+> pp clause ) |
    --    (l, clause) <- M.toList cnf]
    result <- runPicosat idx cnf
    case result of 
        Left clauses -> do
            putStrLn "No suitable set of packages could be determined,"
            putStrLn "because the following requirements conflict:"
            print (vcat (map pp clauses))
        Right newAtoms -> do
            printDifference (atoms testing) newAtoms
    
printDifference old new = do
    putStrLn "New state"
    forM_ (S.toList new) $ \atom -> putStrLn $ "    " ++ show atom
    let added = new `S.difference` old
    putStrLn "Newly added packages"
    forM_ (S.toList added) $ \atom -> putStrLn $ "    " ++ show atom
    let removed = old `S.difference` new
    putStrLn "Removed packages"
    forM_ (S.toList removed) $ \atom -> putStrLn $ "    " ++ show atom

printUsage = do
    name <- getProgName
    putStrLn ("Usage: " ++ name ++ " dir_to_data/")
