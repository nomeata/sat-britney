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
import Types

type CNF2Clause a = HM.HashMap Conj [Clause a]

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


onlyCNF :: CNF2Clause a -> CNF
onlyCNF = HM.keys

clauses2CNF :: [Clause AtomI] -> CNF2Clause AtomI
clauses2CNF clauses = HM.fromListWith (++)
    [ (conj, [clause]) | clause <- clauses , conj <- clause2CNF clause ]


clause2CNF :: Clause AtomI -> CNF
clause2CNF (OneOf as _) = [ reorder ais ]
    where ais = [ unIndex a | a <- as ]
clause2CNF (AtMostOne as _) = [ reorder [-ai1, -ai2]
                              | ai1 <- ais , ai2 <- ais , ai1 /= ai2 ]
    where ais = [ unIndex a | a <- as ]
clause2CNF (AllOrNone as _) = [ reorder [-unIndex a1, unIndex a2]
                              | (a1,a2) <- zip as (tail (cycle as))]
clause2CNF (Implies a as _) = [ reorder (-ai: ais) ]
    where ai = unIndex a
          ais = [ unIndex a | a <- as ]
clause2CNF (NotBoth a1 a2 _) = [ reorder [- unIndex a1, - unIndex a2 ] ]
clause2CNF (Not a _) = [ atom2Conj (-ai) ]
    where ai = unIndex a

cnf2Clause :: CNF2Clause a -> CNF -> [Clause a]
cnf2Clause cnf = concatMap (\disj -> HM.lookupDefault (err disj) disj cnf) 
    where err cl = error $ "A tool returned the clause " ++ show cl ++ " which is not known to me"

runClauseSAT :: [AtomI] -> [AtomI] -> CNF2Clause AtomI -> IO (Either [Clause AtomI] (S.Set AtomI))
runClauseSAT desired unwanted cnf = do
    result <- runPicosatPMAX (map unIndex desired ++ map (negate . unIndex) unwanted) (onlyCNF cnf)
    case result of
        Left core -> return  $ Left $ cnf2Clause cnf core
        Right vars -> return $ Right $ varsToSet vars

runClauseMINMAXSAT :: [AtomI] -> [AtomI] -> CNF2Clause AtomI -> IO (Either [Clause AtomI] (S.Set AtomI, [S.Set AtomI]))
runClauseMINMAXSAT desired unwanted cnf = do
    result <- runPicosatPMINMAX (map unIndex desired ++ map (negate . unIndex) unwanted) (onlyCNF cnf)
    case result of
        Left core -> return  $ Left $ cnf2Clause cnf core
        Right (vars,varss) -> return $ Right $ (varsToSet vars, map varsToSet varss)

varsToSet :: [Int] -> S.Set AtomI
varsToSet vars = S.fromList [ Index i | i <- vars, i > 0] 

runRelaxer :: CNF2Clause a -> CNF2Clause a -> IO (Either [Clause a] [Clause a])
runRelaxer relaxable cnf = do
    removeCNF <- relaxer (onlyCNF relaxable) (onlyCNF cnf)
    return $ either (Left . cnf2Clause cnf) (Right . cnf2Clause relaxable) removeCNF
