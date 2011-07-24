{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Debian.Version.ByteString (parseDebianVersion)

import System.IO

import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)
import qualified Data.Strict as ST
import Control.DeepSeq

import qualified Data.Set as S
import qualified Data.Map as M
import Data.List
import Data.Function
import DebVersionCmp

type Set = S.Set
type Map = M.Map

instance NFData BS.ByteString 

newtype DebianVersion = DebianVersion { unDebianVersion :: ByteString }
    deriving (Ord, Eq)
instance Show DebianVersion where show = BS.unpack . unDebianVersion

data VersionReq
    = SLT !DebianVersion
    | LTE !DebianVersion
    | EEQ !DebianVersion
    | GRE !DebianVersion
    | SGR !DebianVersion
      deriving Eq

instance NFData VersionReq

cmpDebianVersion :: DebianVersion -> DebianVersion -> Ordering
cmpDebianVersion = versionCompare `on` unDebianVersion

newtype Arch = Arch { unArch :: ByteString }
    deriving (Ord, Eq, NFData)

instance Show Arch where show = BS.unpack . unArch

newtype SourceName = SourceName { unSourceName :: ByteString }
    deriving (Ord, Eq, NFData)

instance Show SourceName where show = BS.unpack . unSourceName

newtype BinName = BinName { unBinName :: ByteString }
    deriving (Ord, Eq, NFData)

instance Show BinName where show = BS.unpack . unBinName

data Source = Source !SourceName !DebianVersion
    deriving (Ord, Eq)

instance NFData Source

data Binary = Binary !BinName !DebianVersion !(ST.Maybe Arch)
    deriving (Ord, Eq)

instance NFData Binary

data Atom = SrcAtom !Source | BinAtom !Binary | BugAtom !Bug
    deriving (Ord, Eq)

instance NFData Atom

instance Show Source where
    show (Source sn v)             = show sn ++ "_" ++ show v ++ "_src"

instance Show Binary where
    show (Binary bn v ST.Nothing)  = show bn ++ "_" ++ show v ++ "_all"
    show (Binary bn v (ST.Just a)) = show bn ++ "_" ++ show v ++ "_" ++ show a

instance Show Atom where
    show (SrcAtom src) = show src
    show (BinAtom bin) = show bin
    show (BugAtom bug) = "rc_bug_" ++ show bug

newtype Bug = Bug { unBug :: Int }
    deriving (Ord, Eq, NFData)
instance Show Bug where show = show . unBug

newtype Urgency = Urgency { unUrgency :: ByteString }
    deriving (Ord, Eq, NFData)
instance Show Urgency where show = BS.unpack . unUrgency

newtype Age = Age { unAge :: Int }
    deriving (Ord, Eq, NFData)
instance Show Age where show = show . unAge

data ArchitectureReq
    = ArchOnly [Arch]
    | ArchExcept [Arch]
      deriving Eq

instance Show ArchitectureReq where
    show (ArchOnly arch) = " [" ++ intercalate " " (map show arch) ++ "]"
    show (ArchExcept arch) = " [!" ++ intercalate " !" (map show arch) ++ "]"

instance NFData ArchitectureReq where
    rnf (ArchOnly as) = as `deepseq` ()
    rnf (ArchExcept as) = as `deepseq` ()

data DepRel = DepRel !BinName !(Maybe VersionReq) !(Maybe ArchitectureReq)
		deriving Eq

instance NFData DepRel where
    rnf (DepRel a b c) = a `seq` b `deepseq` c `deepseq` ()

type DepDisj = ([DepRel], ByteString)

type Dependency = [DepDisj]

data SuiteInfo = SuiteInfo {
    sources :: S.Set Source,
    binaries :: S.Set Binary,
    atoms :: S.Set Atom,
    sourceNames :: Map SourceName [Source],
    binaryNames :: Map (BinName, Arch) [Binary],
    builds :: Map Source [Binary],
    builtBy :: Map Binary Source,
    depends :: Map Binary Dependency,
    provides :: Map (BinName, Arch) [Binary],
    newerSources :: Map Source [Source],
    bugs :: Map Atom [Bug]
    }

data GeneralInfo = GeneralInfo {
    urgencies :: Map Source Urgency,
    ages :: Map Source Age
    }

data Config = Config
    { dir :: FilePath
    , arches :: [Arch]
    , releaseArches :: [Arch]
    , archForAll :: Arch
    , minAges :: Map Urgency Age
    , defaultMinAge :: Age

    , relaxationH :: Maybe Handle
    , verboseRelaxation :: Bool
    , clausesH :: Maybe Handle
    , dimacsH :: Maybe Handle
    , differenceH :: Maybe Handle
    }

mbDo Nothing _ = return ()
mbDo (Just x) f = f x
