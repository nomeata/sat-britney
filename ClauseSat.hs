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
import qualified Data.HashMap.Lazy as HM

import PrettyPrint
import LitSat
import Picosat

type CNF2Clause a = HM.HashMap Conj [Clause a]

allAtoms :: Ord a => [Clause a] -> M.Map a ()
allAtoms = foldl go M.empty
  where go m (OneOf as _)     = foldl (\m c -> M.insert c () m) m as
        go m (AtMostOne as _) = foldl (\m c -> M.insert c () m) m as
        go m (Implies a as _) = M.insert a () $ foldl (\m c -> M.insert c () m) m as
        go m (Not a _)        = M.insert a () m
{-
Is a slightly optimized version of:
allAtoms :: Ord a => [Clause a] -> M.Map a ()
allAtoms = M.fromList . map (\x -> (x,())) . concatMap atoms
-}


onlyCNF :: CNF2Clause a -> CNF
onlyCNF = HM.keys

clauses2CNF :: Ord a => M.Map a () -> [Clause a] -> CNF2Clause a
clauses2CNF idx clauses = HM.fromListWith (++)
    [ (conj, [clause]) | clause <- clauses , conj <- clause2CNF idx clause ]


clause2CNF :: Ord a => M.Map a () -> Clause a -> CNF
clause2CNF idx (OneOf as _) = [ reorder ais ]
    where ais = [ M.findIndex a idx + 1 | a <- as ]
clause2CNF idx (AtMostOne as _) = [ reorder [-ai1, -ai2] | ai1 <- ais , ai2 <- ais , ai1 /= ai2 ]
    where ais = [ M.findIndex a idx + 1 | a <- as ]
clause2CNF idx (Implies a as _) = [ reorder (-ai: ais) ]
    where ai = M.findIndex a idx + 1
          ais = [ M.findIndex a idx + 1 | a <- as ]
clause2CNF idx (Not a _) = [ [-ai] ]
    where ai = M.findIndex a idx + 1

cnf2Clause :: CNF2Clause a -> CNF -> [Clause a]
cnf2Clause cnf = concatMap (\disj -> HM.lookupDefault undefined disj cnf) 

runPicosat :: (Show a, Ord a) => M.Map a () -> CNF2Clause a -> IO (Either [Clause a] (S.Set a))
runPicosat idx cnf = do
    result <- runPicosatCNF (onlyCNF cnf)
    case result of
        Left core -> do
            return (Left (cnf2Clause cnf core))
        Right vars -> do
            let atoms = [ atom | i <- vars, i > 0, -- We only return the true variables
                                 let (atom,_) = M.elemAt (i-1) idx
                        ]
            return (Right (S.fromList atoms))

runRelaxer :: CNF2Clause a -> CNF2Clause a -> IO (Either [Clause a] [Clause a])
runRelaxer relaxable cnf = do
    removeCNF <- relaxer (S.fromList (HM.keys relaxable)) (onlyCNF cnf)
    return $ either (Left . cnf2Clause cnf) (Right . cnf2Clause cnf) removeCNF
