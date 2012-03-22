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

import Types
import AtomIndex
import Arches
import qualified IndexSet as IxS

findUninstallablePackages :: Config -> AtomIndex -> FilePath -> Arch -> IO (IxS.Set Binary)
findUninstallablePackages config ai dir arch = do
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
                    Nothing -> error $ show bin ++ " not found in AtomIndex"
                ) $
            map (\(name,arch,version) ->
                Binary (BinName (BS.pack name))
                       (DebianVersion (BS.pack version))
                       (if arch == "all" then ST.Nothing else ST.Just (read arch))) $
            uninstallable

collectEdosOutput :: FilePath -> IO [(String, String, String)]
collectEdosOutput file = do
    pkgFile <- openFile file ReadMode
    (_, Just edosOut, _, _) <- createProcess $ (proc "edos-debcheck" ["-quiet", "-xml","-failures"]) { std_in = UseHandle pkgFile, std_out = CreatePipe }
    Document _ _ root  _ <- xmlParse "edos output" <$> hGetContents edosOut
    -- How do you actually use this HaXmL? This can not be the correct way:
    let filter = concatMap ((attributed "package" `x` attributed "architecture" `x` attributed "version" `x` extracted (concat . mapMaybe fst . textlabelled (txt `o` children)) ) keep) . (elm `o` children)
    return $ map (\((((p,a),v),_),_) -> (p, a, v)) (filter (CElem root noPos))

