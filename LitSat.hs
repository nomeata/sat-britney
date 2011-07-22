module LitSat where

import System.IO
import System.Posix.IO
import System.Process
import Data.Functor
import Data.List
import Data.Ord
import Data.Function

import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.Set as S


data Clause a 
    = OneOf [a] String
    | AtMostOne [a] String
    | Implies a [a] String
    | Not a String
    deriving (Show)


allAtoms :: Ord a => [Clause a] -> M.Map a ()
allAtoms = M.fromList . map (\x -> (x,())) . concatMap atoms
  where atoms (OneOf as _) = as
        atoms (AtMostOne as _) = as
        atoms (Implies a as _) = a:as
        atoms (Not a _) = [a]

type CNF = [Conj]
type Conj = [Int]
type CNF2Clause a = M.Map Conj (Clause a)

clauses2CNF :: Ord a => M.Map a () -> [Clause a] -> CNF2Clause a
clauses2CNF idx clauses = M.fromList
    [ (conj, clause) | clause <- clauses , conj <- clause2CNF idx clause ]

reorder = sortBy (compare `on` abs)

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

formatCNF :: M.Map a () -> CNF -> String
formatCNF idx cnf =
    "c LitSat CNF generator\n" ++
    "p cnf " ++ show (M.size idx) ++ " " ++ show (length cnf) ++ "\n" ++
    concatMap (\l -> unwords (map show l) ++ " 0\n") cnf

parseCNF :: String -> CNF
parseCNF str =
    map (init . map read . words) $
    dropWhile (\l -> null l || head l `elem` "cp") $
    lines str

runPicosat :: Show a => M.Map a () -> CNF2Clause a -> IO (Either [Clause a] [a])
runPicosat idx cnf = do
    (coreInFd, coreOutFd) <- createPipe
    coreIn <- fdToHandle coreInFd

    putStr cnfString

    (Just hint, _, _, _) <- createProcess $
        (proc "picosat.trace" ["-c", "/proc/self/fd/" ++ show coreOutFd])
        { std_in = CreatePipe }

    closeFd coreOutFd
    hPutStr hint cnfString
    hClose hint
    core <- parseCNF <$> hGetContents coreIn
    print core
    let coreClauses = map (\disj -> cnf ! disj) $ core
    return (Left coreClauses)
  where cnfString = formatCNF idx (M.keys cnf)
    

