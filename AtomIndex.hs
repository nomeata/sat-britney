module AtomIndex where

import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Functor

import Types
import Indices

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

