{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module: Types
-- Copyright: (c) 2011 Joachim Breitner
-- License: GPL-2
--
module Types where

import System.IO

import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)
import qualified Data.Strict as ST
import Control.DeepSeq

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.List
import Data.Function
import Data.Functor
import DebVersionCmp
import Indices
import qualified IndexMap as IxM
import qualified IndexSet as IxS

type Set = S.Set
type Map = M.Map

instance NFData BS.ByteString 
instance NFData a => NFData (ST.Maybe a) where
    rnf (ST.Just x) = rnf x
    rnf ST.Nothing = ()

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

data Inst = Inst
    { instFor :: !BinI
    , instBin :: !BinI 
    }
    deriving (Ord, Eq)

instance NFData Inst

data Atom = SrcAtom !Source
          | BinAtom !Binary
          | BugAtom !Bug
          | InstAtom !Inst
    deriving (Ord, Eq)

instance NFData Atom

instance Show Source where
    show (Source sn v)             = show sn ++ "_" ++ show v ++ "_src"

instance Show Binary where
    show (Binary bn v ST.Nothing)  = show bn ++ "_" ++ show v ++ "_all"
    show (Binary bn v (ST.Just a)) = show bn ++ "_" ++ show v ++ "_" ++ show a

instance Show Inst where
    show (Inst for bin) = show bin ++ "@" ++ show for

instance Show Atom where
    show (SrcAtom src) = show src
    show (BinAtom bin) = show bin
    show (InstAtom inst) = show inst
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

data DepRel = DepRel !BinName !(ST.Maybe VersionReq) !(ST.Maybe ArchitectureReq)
		deriving (Show, Eq)

instance NFData DepRel where
    rnf (DepRel a b c) = a `seq` b `deepseq` c `deepseq` ()

type DepDisj = ([DepRel], ByteString)

type Dependency = [DepDisj]

{- Atom index counter stuff -}

type Counter = Int

type BinI = Index Binary
type SrcI = Index Source
type BugI = Index Bug
type InstI = Index Inst
type AtomI = Index Atom

genIndex :: Index a -> Index Atom
genIndex (Index i) = Index i

type AtomIndex = (M.Map Atom Int, IM.IntMap Atom, Counter)

maxIndex :: AtomIndex -> AtomI
maxIndex (_,_,i) = Index (pred i)

emptyIndex :: AtomIndex
emptyIndex = (M.empty, IM.empty, 1)

indexBin :: AtomIndex -> Binary -> Maybe BinI
indexBin (m,_,_) b = Index <$> BinAtom b `M.lookup` m
indexSrc :: AtomIndex -> Source -> Maybe SrcI
indexSrc (m,_,_) s = Index <$> SrcAtom s `M.lookup` m
indexBug :: AtomIndex -> Bug -> Maybe BugI
indexBug (m,_,_) b = Index <$> BugAtom b `M.lookup` m
indexInst :: AtomIndex -> Inst -> Maybe InstI
indexInst (m,_,_) i' = Index <$> InstAtom i' `M.lookup` m
indexAtom :: AtomIndex -> Atom -> Maybe AtomI
indexAtom (m,_,_) a = Index <$> a `M.lookup` m

addBin :: AtomIndex -> Binary -> (AtomIndex, BinI)
addBin a2i@(m,m',c) b = case indexBin a2i b of
                    Just i -> (a2i, i)
                    Nothing -> (((BinAtom b `M.insert` c) m, (c `IM.insert` BinAtom b) m', succ c), Index c)
addSrc :: AtomIndex -> Source -> (AtomIndex, SrcI)
addSrc a2i@(m,m',c) b = case indexSrc a2i b of
                    Just i -> (a2i, i)
                    Nothing -> (((SrcAtom b `M.insert` c) m, (c `IM.insert` SrcAtom b) m', succ c), Index c)
addBug :: AtomIndex -> Bug -> (AtomIndex, BugI)
addBug a2i@(m,m',c) b = case indexBug a2i b of
                    Just i -> (a2i, i)
                    Nothing -> (((BugAtom b `M.insert` c) m, (c `IM.insert` BugAtom b) m', succ c), Index c)

addInst :: AtomIndex -> Inst -> (AtomIndex, InstI)
addInst a2i@(m,m',c) i' = case indexInst a2i i' of
                    Just i -> (a2i, i)
                    Nothing -> (((InstAtom i' `M.insert` c) m, (c `IM.insert` InstAtom i') m', succ c), Index c)

lookupBin :: AtomIndex -> BinI -> Binary
lookupBin (_,m,_) (Index i) = (\(BinAtom b) -> b) (m IM.! i)
lookupSrc :: AtomIndex -> SrcI -> Source
lookupSrc (_,m,_) (Index i) = (\(SrcAtom b) -> b) (m IM.! i)
lookupBug :: AtomIndex -> BugI -> Bug
lookupBug (_,m,_) (Index i) = (\(BugAtom b) -> b) (m IM.! i)
lookupInst :: AtomIndex -> InstI -> Inst
lookupInst (_,m,_) (Index i) = (\(InstAtom b) -> b) (m IM.! i)
lookupAtom :: AtomIndex -> AtomI -> Atom
lookupAtom (_,m,_) (Index i) = m IM.! i

-- | Data specific to one suite
--
-- TODO: could be removed from here
data SuiteInfo = SuiteInfo {
    sources :: IxS.Set Source,
    binaries :: IxS.Set Binary,
    atoms :: IxS.Set Atom,
    sourceNames :: Map SourceName [SrcI],
    binaryNames :: Map (BinName, Arch) [BinI],
    builds :: IxM.Map Source [BinI],
    newerSources :: IxM.Map Source [SrcI],
    bugs :: IxM.Map Atom [BugI]
    }
    deriving (Show)

-- | Data that comes from one suite, but really ought to apply to all of them,
-- e.g. because it is actually properties of binaries.
-- 
-- Raw variant, e.g. before resolving dependencies and conflicts.
data RawPackageInfo = RawPackageInfo {
    binaryNamesR :: Map (BinName, Arch) [BinI], -- Duplicate from SuiteInfo
    builtByR :: IxM.Map Binary SrcI,
    dependsR :: IxM.Map Binary Dependency,
    providesR :: Map (BinName, Arch) [BinI],
    conflictsR :: IxM.Map Binary Dependency,
    breaksR :: IxM.Map Binary Dependency
}
    deriving (Show)
    
data PackageInfo = PackageInfo {
    builtBy :: IxM.Map Binary SrcI,
    depends :: IxM.Map Binary [([BinI], ByteString)],
    dependsRel :: IxM.Map Binary (IxS.Set Binary),
    dependsHull :: IxM.Map Binary (IxS.Set Binary),
    conflicts :: IxM.Map Binary [([BinI], ByteString)],
    conflictsRel :: IxM.Map Binary (IxS.Set Binary),
    hasConflict :: IxS.Set Binary,
    hasConflictInDeps :: IxS.Set Binary,
    hasBadConflictInDeps :: IxS.Set Binary
}
    deriving (Show)

data GeneralInfo = GeneralInfo {
    urgencies :: Map SrcI Urgency,
    ages :: Map SrcI Age
    }
    deriving (Show)

data TransSize = AsLargeAsPossible | AsSmallAsPossible | ManySmall | AnySize
    deriving (Eq)

data Config = Config
    { dir :: FilePath
    , arches :: [Arch]
    , releaseArches :: [Arch]
    , archForAll :: Arch
    , minAges :: Map Urgency Age
    , defaultMinAge :: Age

    , fullDependencies :: Bool

    , transSize :: TransSize

    , relaxationH :: Maybe Handle
    , verboseRelaxation :: Bool
    , clausesH :: Maybe Handle
    , clausesUnrelaxH :: Maybe Handle
    , dimacsH :: Maybe Handle
    , differenceH :: Maybe Handle
    , hintsH :: Maybe Handle

    , migrateThis :: Maybe Atom
    , migrateThisI :: Maybe AtomI
    }

mbDo Nothing _ = return ()
mbDo (Just x) f = f x
