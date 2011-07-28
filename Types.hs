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
import Data.Functor
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
      deriving (Eq, Show)

instance NFData VersionReq

hasUpperBound (SLT _ ) = True
hasUpperBound (LTE _ ) = True
hasUpperBound (EEQ _ ) = True
hasUpperBound _ = False

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

data Binary = Binary
    { binName :: !BinName
    , binVersion :: !DebianVersion
    , binArch :: !(ST.Maybe Arch)
    }
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
		deriving (Show, Eq)

instance NFData DepRel where
    rnf (DepRel a b c) = a `seq` b `deepseq` c `deepseq` ()

type DepDisj = ([DepRel], ByteString)

type Dependency = [DepDisj]

{- Atom index counter stuff -}

newtype Index a = Index { unIndex :: Int }
 deriving (Eq, Ord, NFData)
instance Show (Index a) where show (Index i ) = "I" ++ show i

type Counter = Int

type BinI = Index Binary
type SrcI = Index Source
type BugI = Index Bug
type AtomI = Index Atom

genIndex :: Index a -> Index Atom
genIndex (Index i) = Index i

type AtomIndex = (M.Map Atom Int, M.Map Int Atom, Int)

maxIndex :: AtomIndex -> AtomI
maxIndex (_,_,i) = Index (pred i)

emptyIndex :: AtomIndex
emptyIndex = (M.empty, M.empty, 1)

indexBin :: AtomIndex -> Binary -> Maybe BinI
indexBin (m,_,_) b = Index <$> BinAtom b `M.lookup` m
indexSrc :: AtomIndex -> Source -> Maybe SrcI
indexSrc (m,_,_) s = Index <$> SrcAtom s `M.lookup` m
indexBug :: AtomIndex -> Bug -> Maybe BugI
indexBug (m,_,_) b = Index <$> BugAtom b `M.lookup` m

addBin :: AtomIndex -> Binary -> (AtomIndex, BinI)
addBin a2i@(m,m',c) b = case indexBin a2i b of
                    Just i -> (a2i, i)
                    Nothing -> (((BinAtom b `M.insert` c) m, (c `M.insert` BinAtom b) m', succ c), Index c)
addSrc :: AtomIndex -> Source -> (AtomIndex, SrcI)
addSrc a2i@(m,m',c) b = case indexSrc a2i b of
                    Just i -> (a2i, i)
                    Nothing -> (((SrcAtom b `M.insert` c) m, (c `M.insert` SrcAtom b) m', succ c), Index c)
addBug :: AtomIndex -> Bug -> (AtomIndex, BugI)
addBug a2i@(m,m',c) b = case indexBug a2i b of
                    Just i -> (a2i, i)
                    Nothing -> (((BugAtom b `M.insert` c) m, (c `M.insert` BugAtom b) m', succ c), Index c)

lookupBin :: AtomIndex -> BinI -> Binary
lookupBin (_,m,_) (Index i) = (\(BinAtom b) -> b) (m M.! i)
lookupSrc :: AtomIndex -> SrcI -> Source
lookupSrc (_,m,_) (Index i) = (\(SrcAtom b) -> b) (m M.! i)
lookupBug :: AtomIndex -> BugI -> Bug
lookupBug (_,m,_) (Index i) = (\(BugAtom b) -> b) (m M.! i)
lookupAtom :: AtomIndex -> AtomI -> Atom
lookupAtom (_,m,_) (Index i) = m M.! i

data SuiteInfo = SuiteInfo {
    sources :: S.Set SrcI,
    binaries :: S.Set BinI,
    atoms :: S.Set AtomI,
    sourceNames :: Map SourceName [SrcI],
    binaryNames :: Map (BinName, Arch) [BinI],
    builds :: Map SrcI [BinI],
    builtBy :: Map BinI SrcI,
    depends :: Map BinI Dependency,
    provides :: Map (BinName, Arch) [BinI],
    conflicts :: Map BinI Dependency,
    breaks :: Map BinI Dependency,
    newerSources :: Map SrcI [SrcI],
    bugs :: Map AtomI [BugI]
    }

data GeneralInfo = GeneralInfo {
    urgencies :: Map SrcI Urgency,
    ages :: Map SrcI Age
    }

data TransSize = AsLargeAsPossible | AsSmallAsPossible | ManySmall | AnySize
    deriving (Eq)

data Config = Config
    { dir :: FilePath
    , arches :: [Arch]
    , releaseArches :: [Arch]
    , archForAll :: Arch
    , minAges :: Map Urgency Age
    , defaultMinAge :: Age

    , transSize :: TransSize

    , relaxationH :: Maybe Handle
    , verboseRelaxation :: Bool
    , clausesH :: Maybe Handle
    , dimacsH :: Maybe Handle
    , differenceH :: Maybe Handle
    , hintsH :: Maybe Handle

    , migrateThis :: Maybe Source
    , migrateThisI :: Maybe SrcI
    }

mbDo Nothing _ = return ()
mbDo (Just x) f = f x
