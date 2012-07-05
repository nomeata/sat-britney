{-# LANGUAGE TypeOperators #-}

module AtomIndex where

import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Functor
import Data.Strict.Tuple

import Types
import Arches
import Indices

type AtomIndex = (M.Map Atom Int :!: IM.IntMap (IM.IntMap (IM.IntMap Int)) :!: IM.IntMap Atom :!: Counter)

maxIndex :: AtomIndex -> AtomI
maxIndex (_ :!: _ :!: _ :!: i) = Index (pred i)

emptyIndex :: AtomIndex
emptyIndex = (M.empty :!: IM.empty :!: IM.empty :!: 1)

indexBin :: AtomIndex -> Binary -> Maybe BinI
indexBin (m :!: _ :!: _ :!: _) b = Index <$> BinAtom b `M.lookup` m
indexSrc :: AtomIndex -> Source -> Maybe SrcI
indexSrc (m :!: _ :!: _ :!: _) s = Index <$> SrcAtom s `M.lookup` m
indexBug :: AtomIndex -> Bug -> Maybe BugI
indexBug (m :!: _ :!: _ :!: _) b = Index <$> BugAtom b `M.lookup` m
indexInst :: AtomIndex -> Inst -> Maybe InstI
indexInst (m :!: im :!: _ :!: _) (Inst (Index b1) (Index b2) (Arch arch)) = do
    im' <- arch `IM.lookup` im
    im'' <- b1 `IM.lookup` im'
    i <- b2 `IM.lookup` im''
    return $ Index i
indexAtom :: AtomIndex -> Atom -> Maybe AtomI
indexAtom ai@(m :!: _ :!: _ :!: _) (InstAtom i) = genIndex <$> indexInst ai i
indexAtom (m :!: _ :!: _ :!: _) a = Index <$> a `M.lookup` m

addBin :: AtomIndex -> Binary -> (AtomIndex, BinI)
addBin a2i@(m :!: im :!: m' :!: c) b = case indexBin a2i b of
                    Just i -> (a2i, i)
                    Nothing -> (((BinAtom b `M.insert` c) m :!:
                                 im :!:
                                 (c `IM.insert` BinAtom b) m' :!:
                                 succ c),
                                Index c)
addSrc :: AtomIndex -> Source -> (AtomIndex, SrcI)
addSrc a2i@(m :!: im :!: m' :!: c) b = case indexSrc a2i b of
                    Just i -> (a2i, i)
                    Nothing -> (((SrcAtom b `M.insert` c) m :!:
                                 im :!:
                                 (c `IM.insert` SrcAtom b) m' :!:
                                 succ c),
                                Index c)
addBug :: AtomIndex -> Bug -> (AtomIndex, BugI)
addBug a2i@(m :!: im :!: m' :!: c) b = case indexBug a2i b of
                    Just i -> (a2i, i)
                    Nothing -> (((BugAtom b `M.insert` c) m :!:
                                 im :!:
                                 (c `IM.insert` BugAtom b) m':!:
                                 succ c),
                                Index c)

addInst :: AtomIndex -> Inst -> (AtomIndex, InstI)
addInst a2i@(m :!: im :!: m' :!: c) i' = case indexInst a2i i' of
                    Just i -> (a2i, i)
                    Nothing -> (( m :!:
                                 addInst2IM i' c im :!:
                                 (c `IM.insert` InstAtom i') m' :!:
                                 succ c
                                ), Index c)

addInst2IM (Inst (Index b1) (Index b2) (Arch a)) c = 
    IM.insertWith (IM.unionWith IM.union) a $
    IM.singleton b1 $ IM.singleton b2 c

lookupBin :: AtomIndex -> BinI -> Binary
lookupBin (_ :!: _ :!: m :!: _) (Index i) = (\(BinAtom b) -> b) (m IM.! i)
lookupSrc :: AtomIndex -> SrcI -> Source
lookupSrc (_ :!: _ :!: m :!: _) (Index i) = (\(SrcAtom b) -> b) (m IM.! i)
lookupBug :: AtomIndex -> BugI -> Bug
lookupBug (_ :!: _ :!: m :!: _) (Index i) = (\(BugAtom b) -> b) (m IM.! i)
lookupInst :: AtomIndex -> InstI -> Inst
lookupInst (_ :!: _ :!: m :!: _) (Index i) = (\(InstAtom b) -> b) (m IM.! i)
lookupAtom :: AtomIndex -> AtomI -> Atom
lookupAtom (_ :!: _ :!: m :!: _) (Index i) = m IM.! i

lookupAny :: AtomIndex -> Index a -> Atom
lookupAny (_ :!: _ :!: m :!: _) (Index i) = m IM.! i

