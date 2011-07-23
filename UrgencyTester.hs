import qualified Data.Map as M
import Types
import Data.List
import ParseSuite
import Data.Function

import DebVersionCmp

main = do
    m <- parseUrgencyFile "realdata/testing/Urgency"
    let srcs = M.keys m
    let vers = [ v | Source _ v <- srcs ]
    head (sortBy c  vers) `seq` return ()

-- c = cmpDebianVersion
c = versionCompare `on` unDebianVersion
{- 
c v1 v2 = 
    let a = (versionCompare `on` unDebianVersion) v1 v2
        b = cmpDebianVersion v1 v2
    in if a == b then a else error $ show v1 ++ " " ++ show v2 ++ " " ++ show a ++ " " ++ show b
-}
