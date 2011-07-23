module Types where

import Debian.Version
import Debian.Relation (VersionReq)

import System.IO

import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)
import qualified Data.Strict as ST

import qualified Data.Set as S
import qualified Data.Map as M
import Data.List

type Set = S.Set
type Map = M.Map

newtype Arch = Arch { unArch :: ByteString }
    deriving (Ord, Eq)

instance Show Arch where show = BS.unpack . unArch

newtype SourceName = SourceName { unSourceName :: ByteString }
    deriving (Ord, Eq)

instance Show SourceName where show = BS.unpack . unSourceName

newtype BinName = BinName { unBinName :: ByteString }
    deriving (Ord, Eq)

instance Show BinName where show = BS.unpack . unBinName

data Source = Source !SourceName !DebianVersion
    deriving (Ord, Eq)

data Binary = Binary !BinName !DebianVersion !(ST.Maybe Arch)
    deriving (Ord, Eq)

data Atom = SrcAtom !Source | BinAtom !Binary | BugAtom !Bug
    deriving (Ord, Eq)

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
    deriving (Ord, Eq)
instance Show Bug where show = show . unBug

newtype Urgency = Urgency { unUrgency :: ByteString }
    deriving (Ord, Eq)
instance Show Urgency where show = BS.unpack . unUrgency

newtype Age = Age { unAge :: Int }
    deriving (Ord, Eq)
instance Show Age where show = show . unAge

data ArchitectureReq
    = ArchOnly [Arch]
    | ArchExcept [Arch]
      deriving Eq

instance Show ArchitectureReq where
    show (ArchOnly arch) = " [" ++ intercalate " " (map show arch) ++ "]"
    show (ArchExcept arch) = " [!" ++ intercalate " !" (map show arch) ++ "]"

data DepRel = DepRel !BinName !(Maybe VersionReq) !(Maybe ArchitectureReq)
		deriving Eq

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
    bugs :: Map Atom [Bug],
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
