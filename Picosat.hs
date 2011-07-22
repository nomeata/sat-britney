module Picosat where

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

allAtoms :: Ord a => [Clause a] -> M.Map a ()
allAtoms = M.fromList . map (\x -> (x,())) . concatMap atoms
  where atoms (OneOf as _) = as
        atoms (AtMostOne as _) = as
        atoms (Implies a as _) = a:as
        atoms (Not a _) = [a]

type CNF = [Conj]
type Conj = [Int]
type CNF2Clause a = M.Map Conj [Clause a]

clauses2CNF :: Ord a => M.Map a () -> [Clause a] -> CNF2Clause a
clauses2CNF idx clauses = M.fromListWith (++)
    [ (conj, [clause]) | clause <- clauses , conj <- clause2CNF idx clause ]

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

formatCNF :: CNF -> String
formatCNF cnf =
    "c LitSat CNF generator\n" ++
    "p cnf " ++ show (maximum (map abs (concat cnf))) ++ " " ++ show (length cnf) ++ "\n" ++
    concatMap (\l -> unwords (map show l) ++ " 0\n") cnf

parseCNF :: String -> CNF
parseCNF str =
    map (init . map read . words) $
    dropWhile (\l -> null l || head l `elem` "cp") $
    lines str

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

runPicosatCNF :: CNF -> IO (Either CNF [Int])
runPicosatCNF cnf = do
    let cnfString = formatCNF cnf
 
    (coreInFd, coreOutFd) <- createPipe
    coreIn <- fdToHandle coreInFd

    (Just hint, Just hout, _, _) <- createProcess $
        (proc "picosat.trace" ["-c", "/proc/self/fd/" ++ show coreOutFd])
        { std_in = CreatePipe
        , std_out = CreatePipe
        }

    closeFd coreOutFd
    hPutStr hint cnfString
    hClose hint
    
    result <- hGetLine hout
    case result of
        "s UNSATISFIABLE" -> do
            core <- parseCNF <$> hGetContents coreIn
            return (Left core)
        "s SATISFIABLE" -> do
            satvarsS <- hGetContents hout
            let ls = map (\l ->
                        if not (null l) && head l == 'v'
                        then drop 2 l
                        else error "Cannot parse picosat SAT output: " ++ show l
                    ) $ lines satvarsS
            let vars = case concatMap words ls of 
                 ints@(_:_) | last ints == "0" -> read <$> init ints
                 _ -> error $ "Cannot parse picosat SAT output: " ++ satvarsS
            return (Right vars)
        s -> do
            error $ "Cannot parse picostat status output: " ++ s

-- Takes a CNF and removes clauses (and returns them) until it becomes
-- satisfiable. The first argument gives the CNFs to relax
relaxer :: S.Set Conj -> CNF -> IO CNF
relaxer relaxable = go 
  where go cnf = do
            ret <- runPicosatCNF cnf
            case ret of
                Left mus -> do
                    case find (`S.member` relaxable) mus of
                        Just remove -> (remove:) <$> go (remove `delete` cnf)
                        Nothing -> do
                            putStrLn "No relaxable clause in MUS:"
                            putStr (formatCNF mus)
                            return []
                Right _ -> do
                    return []
