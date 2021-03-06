{-# LANGUAGE Rank2Types #-}

-- |
-- Module: ClauseSat
-- Copyright: (c) 2011 Joachim Breitner
-- License: GPL-2
--
module ClauseSat where

import qualified Data.Set as S

import LitSat
import Picosat
import Types
import Indices
import GHC.Exts ( augment, build ) 
import qualified Data.Vector as V
import Control.Seq
import qualified ZArray as Z

import Data.Vector.Unboxed (modify)
import qualified Data.Vector.Algorithms.Intro as Intro

{-
allAtoms :: Ord a => [Clause a] -> M.Map a ()
allAtoms = foldl go M.empty
  where go m (OneOf as _)     = foldl (\m c -> M.insert c () m) m as
        go m (AtMostOne as _) = foldl (\m c -> M.insert c () m) m as
        go m (Implies a as _) = M.insert a () $ foldl (\m c -> M.insert c () m) m as
        go m (Not a _)        = M.insert a () m

Is a slightly optimized version of:
allAtoms :: Ord a => [Clause a] -> M.Map a ()
allAtoms = M.fromList . map (\x -> (x,())) . concatMap atoms
-}

clauses2CNF :: Producer (Clause AtomI) -> CNF
clauses2CNF clauses = V.fromList conjs
    where conjs = clauses (\c -> augment (clause2CNF c)) [] `using` seqList rseq

clause2CNF :: Clause AtomI -> Producer Conj
clause2CNF c@(OneOf as _) = toProducer
                                [ atoms2Conj ais ]
    where ais = [ unIndex a | a <- as ]

clause2CNF c@(AtMostOne as _) = toProducer
                                [ atoms2Conj [-ai1, -ai2]
                                | ai1 <- ais , ai2 <- ais , ai1 /= ai2 ]
    where ais = [ unIndex a | a <- as ]

clause2CNF c@(AllOrNone as _) = toProducer
                                [ atoms2Conj [-unIndex a1, unIndex a2]
                                | (a1,a2) <- zip as (tail (cycle as))]

clause2CNF c@(Implies a as _) = toProducer
                                [ atoms2Conj (-ai: ais) ]
    where ai = unIndex a
          ais = [ unIndex a | a <- as ]

clause2CNF c@(NotBoth a1 a2 _) = toProducer
                                [ atoms2Conj [- unIndex a1, - unIndex a2 ] ]

clause2CNF c@(Not a _) = toProducer
                                [ atom2Conj (-ai) ]
    where ai = unIndex a

-- TODO clauses are unsorted ATM
cnf2Clauses :: Producer (Clause AtomI) -> CNF -> Producer (Clause AtomI)
cnf2Clauses clauses conj = toProducer $ filter check $ build clauses
  where check c = any (`S.member` conjS) $ map Z.sort $ build (clause2CNF c)
        conjS = S.fromList $ map Z.sort $ V.toList conj

runClauseSAT :: AtomI -> Producer AtomI -> Producer AtomI -> SATProb -> IO (Either CNF (S.Set AtomI))
runClauseSAT mi desired unwanted sp = do
    result <- runPicosatPMAX (build $ mapP unIndex desired `concatP` mapP (negate . unIndex) unwanted) sp
    case result of
        Left core -> return  $ Left core
        Right vars -> return $ Right $ varsToSet vars

runClauseMINMAXSAT :: AtomI -> Producer AtomI -> Producer AtomI -> SATProb -> IO (Either CNF (S.Set AtomI, [S.Set AtomI]))
runClauseMINMAXSAT mi desired unwanted sp = do
    result <- runPicosatPMINMAX (build $ mapP unIndex desired `concatP` mapP (negate . unIndex) unwanted) sp
    case result of
        Left core -> return  $ Left core
        Right (vars,varss) -> return $ Right $ (varsToSet vars, map varsToSet varss)

varsToSet :: [Int] -> S.Set AtomI
varsToSet vars = S.fromList [ Index i | i <- vars, i > 0] 
