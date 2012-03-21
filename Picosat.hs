{-# LANGUAGE OverloadedStrings, TupleSections, ViewPatterns #-}
-- |
-- Module: Picosat
-- Copyright: (c) 2011 Joachim Breitner
-- License: GPL-2
--
module Picosat
    ( Conj
    , CNF
    , relaxer 
    , conjs2Cnf
    , atom2Conj
    , atoms2Conj
    , combineCNF
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
import System.Exit
import Data.Functor
import Data.List
import Data.Maybe
import Data.Function
import Data.Either
import Data.Int
import System.Directory
import Distribution.Simple.Utils (withTempFile)
import Control.Arrow 
import Control.Monad
import Data.BitArray
import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed
import Data.Array.ST
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import qualified Data.IntSet as IS
import qualified Data.Set as S

-- Remember largest variable
type CNF = (V.Vector Conj, Int)
-- Conj is guaranteed to be ordered by absolute value
type Conj = U.Vector Int32

-- Known to have rage (-maxVar,maxVar)
type AssignmentMask = UArray Int Bool

combineCNF :: CNF -> CNF -> CNF
combineCNF (conj1,mi1) (conj2,mi2) = (conj1 V.++ conj2 , mi1 `max` mi2)
{-# INLINE combineCNF #-}

atoms2Conj :: [Int] -> Conj
atoms2Conj = U.fromList . map fromIntegral

conj2Line :: Conj -> BS.ByteString
conj2Line ls = BS.pack $ unwords (map show (U.toList ls)) ++ " 0\n"

conjs2Cnf :: Int -> V.Vector Conj -> CNF
conjs2Cnf m conjs = m `seq` (conjs, m)

atom2Conj :: Int -> Conj
{-# INLINE atom2Conj #-}
atom2Conj i = U.singleton (fromIntegral i)

atom2Conj' = atom2Conj invalidExtra

formatCNF :: CNF -> L.ByteString
formatCNF (conjs,maxVar) = L.concat
    [ L.pack "c LitSat CNF generator\n"
    , L.pack $ unwords ["p", "cnf", show maxVar, show (V.length conjs)] ++ "\n"
    , L.fromChunks $ map conj2Line $ V.toList conjs
    ]

formatCNFPMAX :: CNF -> CNF -> L.ByteString
formatCNFPMAX (conjs, maxVar1) (relaxable, maxVar2) = L.concat $
    [ L.pack "c LitSat CNF generator\n"
    , L.pack $ unwords
        ["p", "wcnf", show maxVar, show (numClauses + numRelaxable), topN ] ++ "\n"
    , L.fromChunks $ prependEach top  $ map conj2Line $ V.toList conjs
    , L.fromChunks $ prependEach soft $ map conj2Line $ V.toList relaxable
    ]
  where numClauses = V.length conjs
        numRelaxable = V.length relaxable
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
    ( V.fromList . map parseConj . dropWhile (\l -> BS.null l || BS.head l `elem` "cp") . BS.lines $ bs
    , m )

runPicosat :: CNF -> IO (Either (CNF) [Int])
runPicosat cnf = do
    let cnfString = formatCNF cnf

    (coreInFd, coreOutFd) <- createPipe
    coreIn <- fdToHandle coreInFd

    let picoProc = proc "picosat.trace" ["-c", "/proc/self/fd/" ++ show coreOutFd]

    (Just hint, Just hout, _, procHandle) <- createProcess $ picoProc
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
            ensureSuccess [10,20] picoProc procHandle
            let mus = parseCNF (snd cnf) musString
            --let annotatedMus = findConj mus cnf
            return (Left mus)
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
            ensureSuccess [10,20] picoProc procHandle
            return (Right vars)
        s -> do
            error $ "Cannot parse picostat status output: " ++ s

findConj :: CNF -> CNF -> CNF
findConj (mus,_) = first (V.filter (`S.member` set))
  where set = S.fromList (V.toList mus)

-- Takes a CNF and removes clauses until it becomes satisfiable.
-- The first argument gives the CNFs to relax
-- If successful, the first CNF returned contains the non-removed clauses,
-- while the second element in the tuple contains the removed clauses.
relaxer :: CNF -> CNF -> IO (Either CNF (CNF,CNF))
relaxer relaxable cnf = do
    ret <- runPMAXSolver cnf relaxable
    case ret of
        Nothing -> do
            ret <- runPicosat cnf
            case ret of
                Left mus -> do
                    hPutStrLn stderr $ "Non-relaxable clauses are not satisfiable"
                    return (Left mus)
                Right _ -> do
                    error $ "The MAX-SAT solver found the problem to be unsatisfiable, " ++
                            "yet the SAT solver found a solution. Possible bug in the solvers?"
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
            return $ Right (satisfied,remove)


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
    where relaxable = (V.fromList (map atom2Conj desired), snd cnf)

-- Takes a CNF and a list of desired atoms (positive or negative), and it finds
-- a solution that is set-inclusion minimal with regard to these atoms, but
-- includes at least one.
runPicosatPMIN1 :: [Int] -> CNF -> IO (Maybe [Int])
runPicosatPMIN1 [] cnf = error $ "Cannot call runPicosatPMIN1 with an empty set of desired clauses"
runPicosatPMIN1 desired cnf = runPMAXSolver cnf relaxable
    where relaxable = (V.fromList (disj : map atom2Conj desired), snd cnf)
          disj = atoms2Conj desired

-- Takes a CNF and a list of desired atoms (positive or negative), and it finds
-- a set-inclusion minimal solutions that covers the set-inclusion maximal
-- solution, both are returned.
runPicosatPMINMAX :: [Int] -> CNF -> IO (Either (CNF) ([Int],[[Int]]))
runPicosatPMINMAX [] cnf = do 
    ret <- runPicosat cnf
    case ret of
        Left mus -> return (Left mus)
        Right solution -> return (Right (solution, [solution]))
runPicosatPMINMAX desired cnf = do
    ret <- if isJust sret
           then runPMAXSolver cnf' (V.fromList (map atom2Conj desired), snd cnf)
           else return Nothing
    case ret of 
        Nothing -> do
            ret <- runPicosat cnf
            case ret of
                Left mus ->
                    return $ Left mus
                Right _ -> do
                    error $ "The MAX-SAT solver found the problem to be unsatisfiable, " ++
                            "yet the SAT solver found a problem. Possible bug in the solvers?"
        Just maxSol -> do
            let maxSol' = applyMask known maxSol
            Right . (maxSol',) <$> step 0 (filter (`IS.member` desiredS) maxSol')
  where sret@(~(Just (cnf', _, known))) = simplifyCNF cnf (V.empty, snd cnf)
        desiredS    = IS.fromList desired
        step 5 _   = return []
        step n []   = return []
        step n todo = do
            hPutStrLn stderr $ show (length todo) ++ " clauses left while finding next small solution..."
            aMinSol <- either (\_ -> error "Solvable problem turned unsolveable")
                              (applyMask known) <$>
                runPicosatPMAX (map negate desired) (first (atoms2Conj todo `V.cons`) cnf')
            let aMinSolS = IS.fromList aMinSol
                todo' = filter (`IS.notMember` aMinSolS) todo
            when (length todo == length todo') $
                hPutStr stderr $ "Solution does not contain any variable."
            (aMinSol :) <$> step (succ n) todo'

partitionSatClauses :: CNF -> [Int] -> (CNF,CNF)
partitionSatClauses (conjs,maxVar) vars = ( (,maxVar) *** (,maxVar)) $ V.unstablePartition check conjs
  where --array = listBitArray (1,maxVar) $ map (>0) vars
        array = array' (1,maxVar) [ (i, True) | i <- vars, i > 0]
        check = U.any (\i -> (i > 0) == unsafeAt' array (abs (fromIntegral i)))


runPMAXSolver :: CNF -> CNF -> IO (Maybe [Int])
runPMAXSolver cnf desired = do
    -- hPrint stderr (length (fst cnf), length (fst desired), length (fst cnf'), length (fst desired'))
    case simplifyCNF cnf desired of
        Just (cnf',desired', known) -> fmap (applyMask known) <$> runSat4j cnf' desired'
        Nothing -> return Nothing

runMSUnCore :: CNF -> CNF -> IO (Maybe [Int])
runMSUnCore = runAPMAXSolver $ \filename ->  proc "./msuncore" ["-v","0",filename]

runMiniMaxSat :: CNF -> CNF -> IO (Maybe [Int])
runMiniMaxSat = runAPMAXSolver $ \filename ->  proc "./minimaxsat" ["-F=2",filename]

runSat4j :: CNF -> CNF -> IO (Maybe [Int])
runSat4j = runAPMAXSolver $  \filename -> proc "java" ["-jar","solvers/sat4j-maxsat.jar",filename]

runClasp :: CNF -> CNF -> IO (Maybe [Int])
runClasp = runAPMAXSolver (\filename ->  proc "clasp" $ ["--quiet=1,2",filename])

runAPMAXSolver :: (FilePath -> CreateProcess) -> CNF -> CNF -> IO (Maybe [Int])
runAPMAXSolver cmd cnf desired =
    if V.null (fst cnf) && V.null (fst desired) then return (Just [1..snd cnf]) else do
    getTemporaryDirectory  >>= \tmpdir -> do
    withTempFile tmpdir "sat-britney-.wcnf" $ \tmpfile handle -> do
    let cnfString = formatCNFPMAX cnf desired

    L.hPut handle cnfString
    hClose handle

    let proc = cmd tmpfile
    (_, Just hout, _, procHandle) <- createProcess $ proc { std_out = CreatePipe }
    
    lines <- filter (not . BS.null) . BS.lines <$> BS.hGetContents hout
    let (slines,(vlines,rest)) =
            second (partition ((=='v') . BS.head)) .
            partition ((=='s') . BS.head) $
            lines
    case slines of 
        []      -> do
            ensureSuccess [] proc procHandle
            error $ "PMAX-SAT solver returned no \"s\"-line."
        (_:_:_) -> do
            error $ "PMAX-SAT solver returned more than one \"s\"-line." ++ 
                    BS.unpack (BS.unlines slines)
        [sline] | sline == "s UNSATISFIABLE" -> do
            hClose hout
            ensureSuccess [] proc procHandle
            return Nothing
                | sline == "s OPTIMUM FOUND" -> do
            let vars = case concatMap (BS.words . BS.drop 2) vlines of 
                 ints@(_:_) -> filter (/= 0) . fmap int $ ints
                 _ -> error $ "Cannot parse pmaxsatsolver assignment output: " ++
                              BS.unpack (BS.unlines slines)
            ensureSuccess [] proc procHandle
            return (Just vars)
                | otherwise -> do
            error $ "Cannot parse pmaxsatsolver status output: " ++ BS.unpack sline
  
ensureSuccess alsoOk proc procHandle = do
    ec <- waitForProcess procHandle
    case ec of
        ExitSuccess -> return ()
        ExitFailure c | c `elem` alsoOk -> return ()
        ExitFailure c -> error $ "Command \"" ++ showCmdSpec (cmdspec proc) ++
         
            "\" failed with error code " ++ show c

showCmdSpec (ShellCommand cmd) = cmd
showCmdSpec (RawCommand cmd args) = concat $ intersperse " " (cmd:args)

-- | This takes hard and soft clauses and propagats constants (e.g. variables
-- fixed by a top level clause), removing variables from clauses or clauses
-- that are known.
simplifyCNF :: CNF -> CNF -> Maybe (CNF, CNF, AssignmentMask)
--simplifyCNF (hard,maxVar) (soft,_)  = Just ((hard,maxVar), (soft,maxVar), emptyMask)
--  where emptyMask = array' (-maxVar, maxVar) []
simplifyCNF (hard,maxVar) (soft,_)  = go [emptyMask] (hard,maxVar) (soft,maxVar)
  where go ms (hard,maxVar) (soft,_) = if null singletons
            then if isValidMask finalMask
                 then Just ((hard,maxVar), (soft,maxVar), finalMask)
                 else Nothing
            else if V.any (U.null) hard'
                 then Nothing
                 else go (knownAtomsA:ms) (hard',maxVar) (soft',maxVar)
          where 
            singletons = {-# SCC "singletons" #-} V.toList $ V.filter (\c -> U.length c == 1) hard
            others = hard
            knownAtomsA = {-# SCC "knownAtomsA" #-}
                array' (-maxVar, maxVar)
                         [ (fromIntegral i, True) | (U.toList -> [i]) <- singletons] 
            surelyTrueAtom i = {-# SCC "surelyTrueAtom" #-}
                unsafeAt' knownAtomsA (fromIntegral i)
            surelyTrueConjs = {-# SCC "surelyTrueConjs" #-}
                U.any surelyTrueAtom
            knownFalse i = {-# SCC "knownFalse" #-}
                unsafeAt' knownAtomsA (fromIntegral (-i))
            hard' = {-# SCC "hard'" #-}
                V.map (U.filter (not . knownFalse)) .
                V.filter (not . surelyTrueConjs) $ others 
            soft' = {-# SCC "soft'" #-}
                V.filter (not . U.null) .
                V.map (U.filter (not . knownFalse)) .
                V.filter (not . surelyTrueConjs) $ soft 
            finalMask = unionsMask ms
        emptyMask = array' (-maxVar, maxVar) [] :: AssignmentMask

array' :: (Int, Int) -> [(Int, Bool)] -> AssignmentMask
array' (l,u) assoc = runSTUArray $ do
    arr <- newArray (l,u) False
    mapM_ (uncurry (writeArray arr)) assoc
    return arr

unsafeAt' arr i = unsafeAt arr $ index (bounds arr) i
{-# INLINE unsafeAt' #-}

partitionEithersV :: V.Vector (Either a b) -> (V.Vector a, V.Vector b)
partitionEithersV = 
  (V.map (either id undefined) *** V.map (either undefined id)) . V.unstablePartition (either (const True) (const False))

isValidMask mask = not $ any (\i -> unsafeAt' mask i && unsafeAt' mask (-i) ) [1..u]
  where (l,u) = bounds mask

applyMask :: AssignmentMask -> [Int] -> [Int]
applyMask mask = map fixAtom 
  where fixAtom i | unsafeAt' mask i    = i
                  | unsafeAt' mask (-i) = -i
                  | otherwise           = i

unionsMask [] = error "Cannot union empty set of masks"
unionsMask ms@(m1:_) =  array' (l,u) [ (i, True) | i <- [l..u] , any (`unsafeAt'` i) ms ]
  where (l,u) = bounds m1

invalidExtra = error "PicoSat.hs: Internally created CNF clause leaked to caller"
