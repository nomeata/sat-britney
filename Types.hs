{-# LANGUAGE GeneralizedNewtypeDeriving, ImpredicativeTypes #-}

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
import Data.List
import Data.Function
import DebVersionCmp
import Indices
import Arches
import qualified ArchMap as AM
import qualified IndexMap as IxM
import qualified IndexSet as IxS

import GHC.Exts

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
    , instOn :: !Arch
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
    show (Inst for bin arch) = show bin ++ "@" ++ show for ++ "@" ++ show arch

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
    dependsR :: [(BinI, Dependency)],
    providesR :: Map (BinName, Arch) [BinI],
    conflictsR :: [(BinI, Dependency)],
    breaksR :: [(BinI, Dependency)]
}
    deriving (Show)
    
type BuiltBy = IxM.Map Binary SrcI

data PackageInfo =
    PackageInfoOut {
        depends :: AM.Map (IxM.Map Binary [([BinI], ByteString)]),
        dependsRel :: AM.Map (IxM.Map Binary (IxS.Set Binary)),
        -- dependsHull :: IxM.Map Binary (IxS.Set Binary),
        dependsBadHull :: AM.Map (IxM.Map Binary (IxS.Pred Binary)),
        conflicts :: AM.Map (IxM.Map Binary [([BinI], ByteString)]),
        conflictsRel :: AM.Map (IxM.Map Binary (IxS.Set Binary)),
        hasConflict :: IxS.Set Binary,
        hasConflictInDeps :: IxS.Set Binary,
        affected :: IxS.Set Binary,
        conflictHistogram :: [((BinI,BinI),Int)],
        relevantDepHistogram :: [(BinI,Int)]
        }
    | PackageInfoIn {
        depends :: AM.Map (IxM.Map Binary [([BinI], ByteString)]),
        dependsBadHull :: AM.Map (IxM.Map Binary (IxS.Pred Binary)),
        conflicts :: AM.Map (IxM.Map Binary [([BinI], ByteString)]),
        affected :: IxS.Set Binary
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
    , hintDir :: Maybe FilePath
    , arches :: [Arch]
    , releaseArches :: [Arch]
    , archForAll :: Arch
    , minAges :: Map Urgency Age
    , defaultMinAge :: Age

    , showStats :: Bool
    , offset :: Integer

    , transSize :: TransSize

    , relaxationH :: Maybe Handle
    , verboseRelaxation :: Bool
    , clausesH :: Maybe Handle
    , clausesUnrelaxH :: Maybe Handle
    , dimacsH :: Maybe Handle
    , differenceH :: Maybe Handle
    , hintsH :: Maybe Handle
    , heidiH :: Maybe Handle

    , migrateThis :: Maybe Atom
    , migrateThisI :: Maybe AtomI
    }

-- List fusion helper
newtype Producer a = Producer { applyProducer :: (forall b. (a -> b -> b) -> b -> b) }

toProducer l = Producer $ \f x -> foldr f x l
{-# INLINE toProducer #-}

fromProducer (Producer p) = build p
{-# INLINE fromProducer #-}

augmentProducer (Producer p) = augment p
{-# INLINE augmentProducer #-}

mapP :: (a -> b) -> Producer a -> Producer b
mapP f (Producer p) = Producer $ \c n -> p (\x ys -> c (f x) ys) n
{-# INLINE mapP #-}

concatP :: Producer a -> Producer a -> Producer a
concatP (Producer p1) (Producer p2) = Producer $ \c n -> p1 c (p2 c n)
{-# INLINE concatP #-}

unionP :: [Producer a] -> Producer a
unionP [] = Producer $ \c n -> n
unionP (Producer p:ps) = Producer $ \c n -> p c (applyProducer (unionP ps) c n)

mbDo Nothing _ = return ()
mbDo (Just x) f = f x
