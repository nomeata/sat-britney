import System.Environment
import System.FilePath
import Text.PrettyPrint
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M

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
    let rules = transitionRules config unstable testing
    -- print (vcat (map pp rules))
    let idx = allAtoms rules
    let cnf = clauses2CNF idx rules
    sequence_ [ print (text (unwords (map show l)) <+> char ':' <+> pp clause ) |
        (l, clause) <- M.toList cnf]
    Left core <- runPicosat idx cnf
    print (vcat (map pp core))

    
printUsage = do
    name <- getProgName
    putStrLn ("Usage: " ++ name ++ " dir_to_data/")
