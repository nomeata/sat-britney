module Types where

import Debian.Version
import Debian.Relation

import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)
import qualified Data.Strict as ST

import qualified Data.Set as S
import qualified Data.Map as M

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

data Atom = Source !SourceName !DebianVersion
    | Binary !BinName !DebianVersion !(ST.Maybe Arch)
    deriving (Ord, Eq)

instance Show Atom where
    show (Source sn v)             = show sn ++ "_" ++ show v ++ "_src"
    show (Binary bn v ST.Nothing)  = show bn ++ "_" ++ show v ++ "_all"
    show (Binary bn v (ST.Just a)) = show bn ++ "_" ++ show v ++ "_" ++ show a

data SuiteInfo = SuiteInfo {
    atoms :: [Atom],
    sourceNames :: Map SourceName [Atom],
    binaryNames :: Map (BinName, Arch) [Atom],
    builds :: Map Atom [Atom],
    builtBy :: Map Atom Atom,
    depends :: Map Atom Relations,
    provides :: Map (BinName, Arch) [Atom],
    newerSources :: Map Atom [Atom]
    }

data Config = Config {
    arches :: [Arch],
    releaseArches :: [Arch],
    archForAll :: Arch
    }
