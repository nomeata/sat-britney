module Picosat where

import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Nums.Careless

import System.IO
import System.Posix.IO
import System.Process
import Data.Functor
import Data.List
import Data.Ord
import Data.Maybe
import Data.Function

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map ((!))

type CNF = [Conj]
type Conj = [Int]

reorder :: Conj -> Conj
reorder = sortBy (compare `on` abs)

formatCNF :: CNF -> String
formatCNF cnf =
    "c LitSat CNF generator\n" ++
    "p cnf " ++ show maxVer ++ " " ++ show (length cnf) ++ "\n" ++
    concatMap (\l -> unwords (map show l) ++ " 0\n") cnf
  where maxVer = {-# SCC "maxVer" #-} foldl' (\n c -> foldl' (max `on` abs) n c) 0 cnf


parseCNF :: BS.ByteString -> CNF
parseCNF str =
    map (init . map int . BS.words) $
    dropWhile (\l -> BS.null l || BS.head l `elem` "cp") $
    BS.lines str

runPicosat :: CNF -> IO (Either CNF [Int])
runPicosat cnf = do
    let cnfString = formatCNF cnf

    (coreInFd, coreOutFd) <- createPipe
    coreIn <- fdToHandle coreInFd

    (Just hint, Just hout, _, procHandle) <- createProcess $
        (proc "picosat.trace" ["-c", "/proc/self/fd/" ++ show coreOutFd])
        { std_in = CreatePipe
        , std_out = CreatePipe
        }

    closeFd coreOutFd
    hPutStr hint cnfString
    hClose hint
    
    result <- fix $ \next -> do
        line <- hGetLine hout
        if null line || head line == 'c' then next else return line
    case result of
        "s UNSATISFIABLE" -> do
            hClose hout
            core <- parseCNF <$> BS.hGetContents coreIn
            waitForProcess procHandle
            return (Left core)
        "s SATISFIABLE" -> do
            hClose coreIn
            satvarsS <- BS.hGetContents hout
            let ls = mapMaybe (\l ->
                        if BS.null l then Nothing
                        else if BS.head l == 'c' then Nothing
                        else if BS.head l == 'v' then Just (BS.drop 2 l)
                        else error $ "Cannot parse picosat SAT output: " ++ BS.unpack l
                    ) $ BS.lines satvarsS
            let vars = case concatMap BS.words ls of 
                 ints@(_:_) | last ints == BS.pack "0" -> int <$> init ints
                 _ -> error $ "Cannot parse picosat SAT output: " ++ BS.unpack satvarsS
            waitForProcess procHandle
            return (Right vars)
        s -> do
            error $ "Cannot parse picostat status output: " ++ s

-- Takes a CNF and removes clauses (and returns them) until it becomes
-- satisfiable. The first argument gives the CNFs to relax
relaxer :: S.Set Conj -> CNF -> IO (Either CNF CNF)
relaxer relaxable = go 
  where go cnf = do
            ret <- runPicosat cnf
            case ret of
                Left mus -> do
                    case find (`S.member` relaxable) mus of
                        Just remove -> fmap (remove:) <$> go (remove `delete` cnf)
                        Nothing -> do
                            hPutStrLn stderr $ "No relaxable clause in MUS:"
                            hPutStr stderr $ (formatCNF mus)
                            return (Left mus)
                Right _ -> do
                    return (Right [])

-- Takes a CNF and a list of desired atoms (positive or negative), and it finds
-- a solution that is set-inclusion maximal with regard to these atoms.
runPicosatPMAX :: [Int] -> CNF -> IO (Either CNF [Int])
runPicosatPMAX desired cnf = do
    -- Initial run, to ensure satisfiability
    ret <- runPicosat cnf
    case ret of
        Left mus -> return (Left mus)
        Right solution -> Right <$> whatsLeft cnf solution desired 
  where whatsLeft cnf solution desired = tryForce (map (:[]) done ++ cnf) solution todo
          where solSet = S.fromList solution
                (done,todo) = partition (`S.member` solSet) desired
        tryForce cnf lastSol [] = return lastSol
        tryForce cnf lastSol (force:desired) = do
            let cnf' = [force] : cnf
            ret <- runPicosat cnf'
            case ret of
                Left _ -> tryForce ([-force] : cnf) lastSol desired
                Right solution -> whatsLeft cnf' solution desired
