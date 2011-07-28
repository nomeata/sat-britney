{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Picosat
    ( Conj
    , CNF
    , relaxer 
    , conjs2Cnf
    , atom2Conj
    , atoms2Conj
    , formatCNF
    , runPicosatPMAX
    , runPicosatPMINMAX
    )
    where

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
import Control.Arrow 
import Debug.Trace
import Control.Monad
import Data.BitArray
import Control.Exception.Base (try)

import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map ((!))

type CNF = ([Conj], Int)
-- Conj is in DIMACS format, e.g. list of digits, followed by "0\n"
-- Also, remember largest variable
type Conj = [Int]

atoms2Conj :: [Int] -> Conj
atoms2Conj = sortBy (compare `on` abs)

conj2Line :: Conj -> BS.ByteString
conj2Line ls = BS.pack $ unwords (map show ls) ++ " 0\n"

conjs2Cnf :: Int -> [Conj] -> CNF
conjs2Cnf m conjs = m `seq` (conjs, m)

atom2Conj :: Int -> Conj
atom2Conj i = [i]

formatCNF :: CNF -> L.ByteString
formatCNF (conjs,maxVar) = L.concat
    [ L.pack "c LitSat CNF generator\n"
    , L.pack $ unwords ["p", "cnf", show maxVar, show (length conjs)] ++ "\n"
    , L.fromChunks $ map conj2Line conjs
    ]

formatCNFPMAX :: CNF -> CNF -> L.ByteString
formatCNFPMAX (conjs, maxVar1) (relaxable, maxVar2) = L.concat $
    [ L.pack "c LitSat CNF generator\n"
    , L.pack $ unwords
        ["p", "wcnf", show maxVar, show (numClauses + numRelaxable), topN ] ++ "\n"
    , L.fromChunks $ prependEach top  $ map conj2Line conjs
    , L.fromChunks $ prependEach soft $ map conj2Line relaxable
    ]
  where numClauses = length conjs
        numRelaxable = length relaxable
        topN = show (numRelaxable + 2)
        top = BS.pack (topN ++ " ")
        soft = BS.pack ("1 ")
        maxVar = maxVar1 `max` maxVar2
        prependEach a [] = []
        prependEach a l = (a:) . intersperse a $ l


parseConj :: BS.ByteString -> Conj
parseConj = atoms2Conj . filter (/=0) . map int . BS.words
                    
parseCNF :: Int -> BS.ByteString -> CNF
parseCNF m bs = 
    ( map parseConj . dropWhile (\l -> BS.null l || BS.head l `elem` "cp") . BS.lines $ bs
    , m )

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
            return (Left (parseCNF (snd cnf) musString))
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
    ret <- runPMAXSolver cnf relaxable
    case ret of
        Nothing -> do
            ret <- runPicosat cnf
            case ret of
                Left mus -> do
                    hPutStrLn stderr $ "Non-relaxable clauses are not satisfiable"
                    L.hPut stderr $ formatCNF mus
                    return (Left mus)
                Right _ -> do
                    error $ "The MAX-SAT solver found the problem to be unsatisfiable, " ++
                            "yet the SAT solver found a problem. Possible bug in the solvers?"
        Just vars -> do
            let (satisfied, remove) = partitionSatClauses relaxable vars
            {- Code to check the PMAX-SAT solver if we do not trust it: 
            let s = S.fromList remove
            let (removed,leftOver) = partition (`S.member`s) relaxable

            ret <- runPicosat (cnf ++ leftOver) 
            case ret of 
                Left _ -> do
                    hPutStrLn stderr $ "Relaxed CNF still unsatisfiable after removing " ++
                        show (length removed) ++ " clauses, retrying..."
                    fmap (removed ++) <$> relaxer leftOver cnf
                Right _ -> return (Right remove)
            -}
            return (Right remove)


-- Takes a CNF and a list of desired atoms (positive or negative), and it finds
-- a solution that is set-inclusion maximal with regard to these atoms.
runPicosatPMAX :: [Int] -> CNF -> IO (Either CNF [Int])
runPicosatPMAX [] cnf = runPicosat cnf
runPicosatPMAX desired cnf = do
    ret <- runPMAXSolver cnf relaxable
    case ret of
        Nothing -> do
            ret <- runPicosat cnf
            case ret of
                Left mus ->
                    return $ Left mus
                Right _ -> do
                    error $ "The MAX-SAT solver found the problem to be unsatisfiable, " ++
                            "yet the SAT solver found a problem. Possible bug in the solvers?"
        Just solution -> return (Right solution)
    where relaxable = (map atom2Conj desired, snd cnf)

-- Takes a CNF and a list of desired atoms (positive or negative), and it finds
-- a solution that is set-inclusion minimal with regard to these atoms, but
-- includes at least one.
runPicosatPMIN1 :: [Int] -> CNF -> IO (Maybe [Int])
runPicosatPMIN1 [] cnf = error $ "Cannot call runPicosatPMIN1 with an empty set of desired clauses"
runPicosatPMIN1 desired cnf = runPMAXSolver cnf relaxable
    where relaxable = (disj : map atom2Conj desired, snd cnf)
          disj = atoms2Conj desired

-- Takes a CNF and a list of desired atoms (positive or negative), and it finds
-- a set-inclusion minimal solutions that covers the set-inclusion maximal
-- solution, both are returned.
runPicosatPMINMAX :: [Int] -> CNF -> IO (Either CNF ([Int],[[Int]]))
runPicosatPMINMAX [] cnf = do 
    ret <- runPicosat cnf
    case ret of
        Left mus -> return (Left mus)
        Right solution -> return (Right (solution, [solution]))
runPicosatPMINMAX desired cnf = do
    ret <- runPicosatPMAX desired cnf
    case ret of 
        Left mus -> return (Left mus)
        Right maxSol -> do
            Right . (maxSol,) <$> step (filter (`IS.member` desiredS) maxSol)
  where desiredS    = IS.fromList desired
        step []     = return []
        step (x:xs) = do
            hPutStrLn stderr $ show (length xs + 1) ++ " clauses left while finding a small solution..."
            aMinSol <- either (\_ -> error "Solvable problem turned unsolveable") id <$>
                runPicosatPMAX (map negate desired) (first (atom2Conj x :) cnf)
            let aMinSolS = IS.fromList aMinSol
                todo = filter (`IS.notMember` aMinSolS) xs
            when (x `IS.notMember` aMinSolS) $
                hPutStr stderr $ "Solution does not contain forced variable."
            (aMinSol :) <$> step todo

partitionSatClauses :: CNF -> [Int] -> (CNF,CNF)
partitionSatClauses (conjs,maxVar) vars = ( (,maxVar) *** (,maxVar)) $ partition check conjs
  where array = listBitArray (1,maxVar) $ map (>0) vars
--        array = bitArray (0,maxVar) [ (abs i, i > 0) | i <- vars]
        check = any (\i -> (i > 0) == lookupBit array (abs i))


runPMAXSolver :: CNF -> CNF -> IO (Maybe [Int])
runPMAXSolver cnf desired = do
    hPrint stderr (length (fst cnf), length (fst desired), length (fst cnf'), length (fst desired'))
    fmap fixSol <$> runClasp cnf desired
  where (cnf',desired', fixSol) = simplifyCNF cnf desired

runMSUnCore :: CNF -> CNF -> IO (Maybe [Int])
runMSUnCore = runAPMAXSolver $ \filename ->  proc "./msuncore" $ ["-v","0",filename]

runMiniMaxSat :: CNF -> CNF -> IO (Maybe [Int])
runMiniMaxSat = runAPMAXSolver (\filename ->  proc "./minimaxsat" $ ["-F=2",filename])

runClasp :: CNF -> CNF -> IO (Maybe [Int])
runClasp = runAPMAXSolver (\filename ->  proc "clasp" $ ["--quiet=1,2",filename])

runAPMAXSolver :: (FilePath -> CreateProcess) -> CNF -> CNF -> IO (Maybe [Int])
runAPMAXSolver cmd cnf desired =
    if null (fst cnf) && null (fst desired) then return (Just [1..snd cnf]) else do
    getTemporaryDirectory  >>= \tmpdir -> do
    withTempFile tmpdir "sat-britney-.dimacs" $ \tmpfile handle -> do
    let cnfString = formatCNFPMAX cnf desired

    L.hPut handle cnfString
    hClose handle

    L.writeFile "/tmp/input.wcnf" cnfString

    (_, Just hout, _, procHandle) <- createProcess $ (cmd tmpfile) { std_out = CreatePipe }
    
    lines <- filter (not . BS.null) . BS.lines <$> BS.hGetContents hout
    let (slines,(vlines,rest)) =
            second (partition ((=='v') . BS.head)) .
            partition ((=='s') . BS.head) $
            lines
    case slines of 
        []      -> error $ "PMAX-SAT solver returned no \"s\"-line."
        (_:_:_) -> do
            error $ "PMAX-SAT solver returned more than one \"s\"-line." ++ 
                    BS.unpack (BS.unlines slines)
        [sline] | sline == "s UNSATISFIABLE" -> do
            hClose hout
            waitForProcess procHandle
            return Nothing
                | sline == "s OPTIMUM FOUND" -> do
            let vars = case concatMap (BS.words . BS.drop 2) vlines of 
                 ints@(_:_) -> filter (/= 0) . fmap int $ ints
                 _ -> error $ "Cannot parse pmaxsatsolver assignment output: " ++
                              BS.unpack (BS.unlines slines)
            waitForProcess procHandle
            return (Just vars)
                | otherwise -> do
            error $ "Cannot parse pmaxsatsolver status output: " ++ BS.unpack sline

simplifyCNF :: CNF -> CNF -> (CNF, CNF, [Int] -> [Int])
simplifyCNF (hard,maxVar) (soft,_) = case singletons of
            [] -> ((hard,maxVar), (soft,maxVar), id)
            _ -> (\(h,s,f) -> (h,s,map fixAtom . f)) $ simplifyCNF (hard',maxVar) (soft',maxVar)
  where (singletons, others) = partitionEithers $ 
            map(\l -> case l of [s] -> Left s ; _ -> Right l ) hard
        (trueAtoms,falseAtoms) = partition (>0) singletons
        trueAtomsA  = bitArray (1,maxVar) [ (i, True) | i <- trueAtoms]
        falseAtomsA = bitArray (1,maxVar) [ (-i, True) | i <- falseAtoms]
        surelyTrueAtom i = if i > 0 then lookupBit trueAtomsA    i
                                    else lookupBit falseAtomsA (-i)
        surelyTrueConjs = any surelyTrueAtom
        known i =  lookupBit trueAtomsA a || lookupBit falseAtomsA a
            where a = abs i
        hard' = map (filter (not . known)) .
                filter (not . surelyTrueConjs) $ others 
        soft' = filter (not . null) .
                map (filter (not . known)) .
                filter (not . surelyTrueConjs) $ soft 
        fixAtom i | lookupBit trueAtomsA a  = a
                  | lookupBit falseAtomsA a = -a 
                  | otherwise               = i
          where a = abs i
