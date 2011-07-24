import qualified Data.Map as M
import Types
import Data.List
import ParseSuite
import Data.Function
import Data.Functor
import ControlParser
import Debian.Control

main = do
    if False then do
        m <- map (\(Paragraph x) -> map formatField x) <$> myParseControl "realdata/unstable/Packages_i386"
        length (show m) `seq` return ()
        else do
        m <- parseControlFile "realdata/unstable/Packages_i386"
        length (show m) `seq` return ()
