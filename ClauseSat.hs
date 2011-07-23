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

type CNF2Clause a = M.Map Conj [Clause a]

allAtoms :: Ord a => [Clause a] -> M.Map a ()
allAtoms = M.fromList . map (\x -> (x,())) . concatMap atoms
  where atoms (OneOf as _) = as
        atoms (AtMostOne as _) = as
        atoms (Implies a as _) = a:as
        atoms (Not a _) = [a]


clauses2CNF :: Ord a => M.Map a () -> [Clause a] -> CNF2Clause a
clauses2CNF idx clauses = M.fromListWith (++)
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
cnf2Clause cnf = concatMap (\disj -> cnf ! disj) 

runPicosat :: (Show a, Ord a) => M.Map a () -> CNF2Clause a -> IO (Either [Clause a] (S.Set a))
runPicosat idx cnf = do
    result <- runPicosatCNF (M.keys cnf)
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
    removeCNF <- relaxer (M.keysSet relaxable) (M.keys cnf)
    return $ either (Left . cnf2Clause cnf) (Right . cnf2Clause cnf) removeCNF
