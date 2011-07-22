import System.Environment
import System.FilePath
import Text.PrettyPrint
import qualified Data.ByteString.Char8 as BS

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
    print (vcat (map pp rules))

    
printUsage = do
    name <- getProgName
    putStrLn ("Usage: " ++ name ++ " dir_to_data/")
