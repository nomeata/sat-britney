{-# LANGUAGE OverloadedStrings #-}
module Picosat where

import qualified Data.ByteString.Lazy.Char8 as L
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
import Data.Either
import System.Directory
import Distribution.Simple.Utils (withTempFile)
import Control.Arrow ((***))
import Debug.Trace
import Control.Monad
import Data.BitArray

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map ((!))

type CNF = [Conj]
-- Conj is in DIMACS format, e.g. list of digits, followed by "0\n"
-- Also, remember largest variable
type Conj = (BS.ByteString, Int)

safeMaximum [] = 0
safeMaximum l = maximum l

reorder :: [Int] -> Conj
reorder ls = m `seq` (BS.pack $ unwords (map show (sortBy (compare `on` abs) ls)) ++ " 0\n" , m)
  where m = maximum (map abs ls)

atom2Conj :: Int -> Conj
atom2Conj i = (BS.pack $ show i ++ " 0\n", i)

formatCNF :: CNF -> L.ByteString
formatCNF cnf = L.concat
    [ L.pack "c LitSat CNF generator\n"
    , L.pack $unwords ["p", "cnf", show maxVar, show numClauses] ++ "\n"
    , body
    ]
  where numClauses = length cnf
        (body, maxVar) = (L.fromChunks *** safeMaximum) (unzip cnf)

formatCNFPMAX :: CNF -> CNF -> L.ByteString
formatCNFPMAX cnf relaxable = L.concat $
    [ L.pack "c LitSat CNF generator\n"
    , L.pack $ unwords
        ["p", "wcnf", show maxVar, show (numClauses + numRelaxable), topN ] ++ "\n"
    , body1
    , body2 ]
  where numClauses = length cnf
        numRelaxable = length relaxable
        topN = show (numRelaxable + 2)
        top = BS.pack (topN ++ " ")
        soft = BS.pack ("1 ")
        (body1, maxVar1) = (L.fromChunks . prependEach top  *** safeMaximum) (unzip cnf)
        (body2, maxVar2) = (L.fromChunks . prependEach soft *** safeMaximum) (unzip relaxable)
        maxVar = maxVar1 `max` maxVar2
        prependEach a [] = []
        prependEach a l = (a:) . intersperse a $ l


parseConj :: BS.ByteString -> Conj
parseConj = reorder . filter (/=0) . map int . BS.words
                    
parseCNF :: BS.ByteString -> CNF
parseCNF = map parseConj . dropWhile (\l -> BS.null l || BS.head l `elem` "cp") . BS.lines

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
    L.hPut hint cnfString
    hClose hint
    
    result <- fix $ \next -> do
        line <- hGetLine hout
        if null line || head line == 'c' then next else return line
    case result of
        "s UNSATISFIABLE" -> do
            hClose hout
            musString <- BS.hGetContents coreIn
            waitForProcess procHandle
            return (Left (parseCNF musString))
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
relaxer :: CNF -> CNF -> IO (Either CNF CNF)
relaxer relaxable cnf = do
    ret <- runPicosat cnf
    case ret of
        Left mus -> do
            hPutStrLn stderr $ "Non-relaxable clauses are not satisfiable"
            L.hPut stderr $ formatCNF mus
            return (Left mus)
        Right _ -> do
            vars <- runPMAXSolver cnf relaxable
            let (satisfied, remove) = partitionSatClauses relaxable vars
            let s = S.fromList remove
            let (removed,leftOver) = partition (`S.member`s) relaxable
            --let s2 = S.fromList relaxable
            --case find (`S.notMember`s2) remove of { Nothing -> return () ; Just clause -> 
            --    hPutStrLn stderr $ "Removed clause " ++ show clause ++ " not found in relaxable" }
            ret <- runPicosat (cnf ++ leftOver) 
            case ret of 
                Left mus -> do
                    hPutStrLn stderr $ "Relaxed CNF still unsatisfiable after removing " ++
                        show (length removed) ++ " clauses, retrying..."
                    fmap (removed ++) <$> relaxer leftOver cnf
                Right _ -> return (Right remove)


-- Takes a CNF and a list of desired atoms (positive or negative), and it finds
-- a solution that is set-inclusion maximal with regard to these atoms.
runPicosatPMAX :: [Int] -> CNF -> IO (Either CNF [Int])
runPicosatPMAX desired cnf = do
    -- Initial run, to ensure satisfiability
    ret <- runPicosat cnf
    case (ret, desired) of
        (Left mus,_) -> return (Left mus)
        (Right solution, []) -> return (Right solution)
        (Right solution, _)  -> Right <$> runPMAXSolver cnf relaxable 
    where relaxable = map (\i -> (BS.pack $ show i ++ " 0\n", i)) desired
{-
    where whatsLeft cnf solution desired = tryForce (map atom2Conj done ++ cnf) solution todo
          where solSet = S.fromList solution
                (done,todo) = partition (`S.member` solSet) desired
        tryForce cnf lastSol [] = return lastSol
        tryForce cnf lastSol (force:desired) = do
            let cnf' = atom2Conj force : cnf
            hPutStr stderr $ "Forcing one, " ++ show (length desired) ++ " left to do."
            ret <- runPicosat cnf'
            case ret of
                Left _ -> do
                    hPutStrLn stderr $ "failed"
                    tryForce (atom2Conj (-force) : cnf) lastSol desired
                Right solution -> do
                    hPutStrLn stderr $ "failed"
                    whatsLeft cnf' solution desired
-}

partitionSatClauses :: CNF -> [Int] -> (CNF,CNF)
partitionSatClauses cnf vars = partition check cnf
  where maxVar = maximum (0:map snd cnf)
        array = listBitArray (1,maxVar) $ map (>0) vars
--        array = bitArray (0,maxVar) [ (abs i, i > 0) | i <- vars]
        check = any (\i -> (i > 0) == lookupBit array (abs i)) . map int . init . BS.words . fst

runPMAXSolver :: CNF -> CNF -> IO [Int]
runPMAXSolver = runMSUnCore

runMSUnCore :: CNF -> CNF -> IO [Int]
runMSUnCore = runAPMAXSolver $ \filename ->  proc "./msuncore" $ ["-v","0",filename]

runMiniMaxSat :: CNF -> CNF -> IO [Int]
runMiniMaxSat cnf desired = do
    ret <- runAPMAXSolver (\filename ->  proc "./minimaxsat" $ ["-F=2",filename]) cnf desired
    removeFile "none"
    return ret

runAPMAXSolver :: (FilePath -> CreateProcess) -> CNF -> CNF -> IO [Int]
runAPMAXSolver cmd cnf desired = getTemporaryDirectory  >>= \tmpdir ->
    withTempFile tmpdir "sat-britney.dimacs" $ \tmpfile handle -> do
    let cnfString = formatCNFPMAX cnf desired

    L.hPut handle cnfString
    hClose handle

    (_, Just hout, _, procHandle) <- createProcess $ (cmd tmpfile) { std_out = CreatePipe }
    
    result <- fix $ \next -> do
        line <- hGetLine hout
        if null line || head line `elem` "co" then next else return line
    case result of
        "s UNSATISFIABLE" -> do
            hClose hout
            waitForProcess procHandle
            error "runMSUnCore should not be called with unsatisfiable instances"
        "s OPTIMUM FOUND" -> do
            satvarsS <- BS.hGetContents hout
            let vLines = mapMaybe (\l ->
                        if BS.null l then Nothing
                        else if BS.head l == 'v' then Just (BS.drop 2 l)
                        else Nothing
                    ) $ BS.lines satvarsS
            let vars = case concatMap BS.words vLines of 
                 ints@(_:_) -> filter (/= 0) . fmap int $ ints
                 _ -> error $ "Cannot parse msuncore SAT output: " ++ BS.unpack satvarsS
            waitForProcess procHandle
            return vars
        s -> do
            error $ "Cannot parse msuncore status output: " ++ s

