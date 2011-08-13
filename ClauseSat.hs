-- |
-- Module: ClauseSat
-- Copyright: (c) 2011 Joachim Breitner
-- License: GPL-2
--
module ClauseSat where
import System.IO
import System.Posix.IO
import System.Process
import Data.Functor
import Data.List
import Data.Ord
import Data.Function

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map ((!))

import PrettyPrint
import LitSat
import Picosat
import Types
import Indices

type CNF2Clause a = CNF (Clause a)

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

clauses2CNF :: AtomI -> [Clause AtomI] -> CNF2Clause AtomI
clauses2CNF (Index mv) clauses = (concatMap clause2CNF clauses, mv)

clause2CNF :: Clause AtomI -> [Conj (Clause AtomI)]
clause2CNF c@(OneOf as _) = [ atoms2Conj c ais ]
    where ais = [ unIndex a | a <- as ]
clause2CNF c@(AtMostOne as _) = [ atoms2Conj c [-ai1, -ai2]
                                | ai1 <- ais , ai2 <- ais , ai1 /= ai2 ]
    where ais = [ unIndex a | a <- as ]
clause2CNF c@(AllOrNone as _) = [ atoms2Conj c [-unIndex a1, unIndex a2]
                                | (a1,a2) <- zip as (tail (cycle as))]
clause2CNF c@(Implies a as _) = [ atoms2Conj c (-ai: ais) ]
    where ai = unIndex a
          ais = [ unIndex a | a <- as ]
clause2CNF c@(NotBoth a1 a2 _) = [ atoms2Conj c [- unIndex a1, - unIndex a2 ] ]
clause2CNF c@(Not a _) = [ atom2Conj c (-ai) ]
    where ai = unIndex a

cnf2Clause = fastNub . map snd . fst
    where fastNub = S.toList . S.fromList

runClauseSAT :: AtomI -> [AtomI] -> [AtomI] -> CNF2Clause AtomI -> IO (Either [Clause AtomI] (S.Set AtomI))
runClauseSAT mi desired unwanted cnf = do
    result <- runPicosatPMAX (map unIndex desired ++ map (negate . unIndex) unwanted) cnf
    case result of
        Left core -> return  $ Left $ cnf2Clause core
        Right vars -> return $ Right $ varsToSet vars

runClauseMINMAXSAT :: AtomI -> [AtomI] -> [AtomI] -> CNF2Clause AtomI -> IO (Either [Clause AtomI] (S.Set AtomI, [S.Set AtomI]))
runClauseMINMAXSAT mi desired unwanted cnf = do
    result <- runPicosatPMINMAX (map unIndex desired ++ map (negate . unIndex) unwanted) cnf
    case result of
        Left core -> return  $ Left $ cnf2Clause core
        Right (vars,varss) -> return $ Right $ (varsToSet vars, map varsToSet varss)

varsToSet :: [Int] -> S.Set AtomI
varsToSet vars = S.fromList [ Index i | i <- vars, i > 0] 

runRelaxer :: AtomI -> CNF2Clause AtomI -> CNF2Clause AtomI -> IO (Either [Clause AtomI] [Clause AtomI])
runRelaxer mi relaxable cnf = do
    removeCNF <- relaxer relaxable cnf
    return $ either (Left . cnf2Clause) (Right . cnf2Clause) removeCNF
