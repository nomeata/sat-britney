{-# LANGUAGE RecordWildCards, PatternGuards #-}

module ParseHints where

import Types
import Arches
import AtomIndex
import qualified IndexSet as IxS

import Data.List.Split
import System.IO
import Data.Functor
import System.FilePath
import System.Directory
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.Char
import Data.List

{- The format is
	# comment
	hint <pkg1>/<ver1> <pkg2>/<ver2> ...
	easy <pkg1>/<ver1> <pkg2>/<ver2> ...
	force-hint <pkg1>/<ver1> <pkg2>/<ver2> ...
	remove <pkg1>/<ver1> ...
	force <pkg1>/<ver1> ...
	block <pkg1> <pkg2> <pkg3> ...
	block-all source
	approve <pkg1>/<ver1> <pkg2>/<ver2> ...
	unblock <pkg1>/<ver1> <pkg2>/<ver2> ...
	urgent <pkg1>/<ver1> <pkg2>/<ver2> ...
	age-days <days> <pkg1>/<ver1> <pkg2>/<ver2> ...
	finished
-}

-- Copied from /srv/release.debian.org/britney/etc/britney2.conf for now
hintFiles = [
    ("vorlon"      , hints_all),
    ("aba"         , hints_all),
    ("he"          , hints_all),
    ("luk"         , hints_all),
    ("zobel"       , hints_standard ++ ["force"]),
    ("pkern"       , hints_standard ++ ["force"]),
    ("adsb"        , hints_standard ++ ["force","force-hint"]),
    ("neilm"       , hints_standard),
    ("mehdi"       , hints_standard),
    ("jcristau"    , hints_standard),
    ("faw"         , hints_helpers),
    ("nthykier"    , hints_helpers),
    ("freeze"      , ["block","block-all","block-udeb"]),
    ("freeze-exception" , ["unblock","unblock-udeb"]),
    ("test-hints",   hints_all) -- for the test suite
    ]

-- Copied from code/b2/britney.py
hints_helpers = ["easy", "hint", "remove", "block", "block-udeb", "unblock", "unblock-udeb", "approve"]
hints_standard = ["urgent", "age-days"] ++ hints_helpers
hints_all = ["force", "force-hint", "block-all"] ++ hints_standard

data HintSpec = HintSpec SourceName (Maybe DebianVersion) (Maybe Arch)
  deriving (Show, Eq, Ord)

data Hint = Easy [HintSpec]
          | Hint [HintSpec]
          | ForceHint [HintSpec]
          | Remove HintSpec
          | Force HintSpec
          | Block HintSpec
          | BlockUdeb HintSpec
          | BlockAll
          | Approve HintSpec
          | Unblock HintSpec
          | UnblockUdeb HintSpec
          | Urgent HintSpec
          | AgeDays Age [HintSpec]
  deriving (Show, Eq, Ord)

readHintFiles :: Config -> IO [Hint]
readHintFiles config | Nothing <- hintDir config  = return []
readHintFiles config | Just dir <- hintDir config = concat <$> mapM (readHintFile dir) hintFiles

readHintFile :: FilePath -> (String, [String]) -> IO [Hint]
readHintFile dir (file,allowed) =
    do ex <- doesFileExist (dir </> file)
       if ex
         then concatMap (readHintLine allowed) . untilFinished . lines <$> readFile (dir </> file)
         else return []

untilFinished :: [String] -> [String]
untilFinished = takeWhile (\l -> not ("finished" `isPrefixOf` l))

readHintLine :: [String] -> String -> [Hint]
readHintLine allowed line =
    case words line of
        [] -> []
        cmd:args | cmd `notElem` allowed -> []
                 | otherwise -> parseHint cmd args

parseHint "unblock" args = map Unblock $ mapMaybe parseHintSpec args
parseHint "unblock-udeb" args = map UnblockUdeb $ mapMaybe parseHintSpec args
parseHint "block" args = map Block $ mapMaybe parseHintSpec args
parseHint "block-udeb" args = map BlockUdeb $ mapMaybe parseHintSpec args
parseHint "remove" args = map Remove $ mapMaybe parseHintSpec args
parseHint "block-all" ["source"] = [BlockAll]
parseHint _       _    = []

parseHintSpec src = case splitOn "/" src of
    [src] -> Just $ HintSpec (SourceName (BS.pack src)) Nothing Nothing
    [src,version] -> Just $ HintSpec (SourceName (BS.pack src)) (Just (DebianVersion (BS.pack version))) Nothing
    _ -> Nothing

data HintResults = HintResults {
    blockedSources :: IxS.Set Source
    , removedSources :: IxS.Set Source
    }
  deriving (Show)
  

processHints :: Config -> AtomIndex -> SuiteInfo -> SuiteInfo -> GeneralInfo -> [Hint] -> HintResults
processHints config ai unstable testing general hints = HintResults {..}
  where blockedSources = IxS.filter isReallyBlockedSource $ sources unstable `IxS.difference` sources testing
        isReallyBlockedSource srcI =
            ((allBlocked || isBlockedSource srcI) && not (isUnblockedSource srcI))
            || (isBlockedUdebSource srcI && not (isUnblockedUdebSource srcI))

        allBlocked = BlockAll `elem` hints

        isUnblockedSource srcI = foldl' (isUnblockedBy (ai `lookupSrc` srcI)) False hints
        isUnblockedBy src True _ = True
        isUnblockedBy src False (Unblock hintSpec) = hintSpecApplies hintSpec src
        isUnblockedBy src b _ = b

        isBlockedSource srcI = foldl' (isBlockedBy (ai `lookupSrc` srcI)) False hints
        isBlockedBy src True _ = True
        isBlockedBy src False (Block hintSpec) = hintSpecApplies hintSpec src
        isBlockedBy src b _ = b

        isUnblockedUdebSource srcI = foldl' (isUnblockedUdebBy (ai `lookupSrc` srcI)) False hints
        isUnblockedUdebBy src True _ = True
        isUnblockedUdebBy src False (UnblockUdeb hintSpec) = hintSpecApplies hintSpec src
        isUnblockedUdebBy src b _ = b

        isBlockedUdebSource srcI = foldl' (isBlockedUdebBy (ai `lookupSrc` srcI)) False hints
        isBlockedUdebBy src True _ = True
        isBlockedUdebBy src False (BlockUdeb hintSpec) = hintSpecApplies hintSpec src
        isBlockedUdebBy src b _ = b

        removedSources = IxS.filter isRemovedSource $ sources unstable `IxS.union` sources testing
        isRemovedSource srcI = foldl' (isRemovedBy (ai `lookupSrc` srcI)) False hints
        isRemovedBy src True _ = True
        isRemovedBy src False (Remove hintSpec) = hintSpecApplies hintSpec src
        isRemovedBy src b _ = b

-- TODO: binNMU syntax
hintSpecApplies (HintSpec sn1 v1 Nothing) (Source name version) = 
   sn1 == name && maybe True (== version) v1 
hintSpecApplies _ _ = False

    
