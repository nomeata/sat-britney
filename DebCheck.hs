{-# LANGUAGE DoAndIfThenElse #-}

module DebCheck where

import System.Process
import Text.XML.HaXml hiding ((!),when)
import Text.XML.HaXml.Posn (noPos)
import System.FilePath
import Control.Monad
import Data.Maybe
import System.IO
import Data.Char
import Data.Functor
import qualified Data.ByteString.Char8 as BS
import qualified Data.Strict as ST
import qualified System.IO.Strict as ST
import qualified Data.Map as M
import Debug.Trace

import Types
import AtomIndex
import Arches
import qualified IndexSet as IxS

findUninstallablePackages :: Config -> AtomIndex -> SuiteInfo -> FilePath -> Arch -> IO (IxS.Set Binary)
findUninstallablePackages config ai suite dir arch = do
    let file = dir </> "Packages_" ++ show arch
    -- edos-debcheck does not like empty files
    str <- readFile file
    if all isSpace str
    then return IxS.empty
    else do
        uninstallable <- collectEdosOutput file
        return $
            IxS.fromList $
            map (\bin -> 
                case ai `indexBin` bin of
                    Just binI -> binI
                    Nothing -> 
                        -- Work around http://bugs.debian.org/665248
                        case M.lookup (binName bin, arch) (binaryNames suite) of
                            Nothing -> error $ show bin ++ " not found in AtomIndex or suite"
                            Just (binI':_) ->
                                --trace ("edos-debcheck returned " ++ show bin ++ ", using " ++ show (ai `lookupBin` binI')) $
                                binI'
                ) $
            map (\(name,arch,version) ->
                Binary (BinName (BS.pack name))
                       (DebianVersion (BS.pack version))
                       (if arch == "all" then ST.Nothing else ST.Just (read arch))) $
            uninstallable

collectEdosOutput :: FilePath -> IO [(String, String, String)]
collectEdosOutput file = do
    pkgFile <- openFile file ReadMode
    (_, Just edosOutH, _, pid) <- createProcess $ (proc "edos-debcheck" ["-quiet", "-xml","-failures"]) { std_in = UseHandle pkgFile, std_out = CreatePipe }
    edosOut <- ST.hGetContents edosOutH
    waitForProcess pid
    let Document _ _ root  _ = xmlParse "edos output" edosOut
    -- How do you actually use this HaXmL? This can not be the correct way:
    let filter = concatMap ((attributed "package" `x` attributed "architecture" `x` attributed "version" `x` extracted (concat . mapMaybe fst . textlabelled (txt `o` children)) ) keep) . (elm `o` children)
    return $ map (\((((p,a),v),_),_) -> (p, a, v)) (filter (CElem root noPos))

