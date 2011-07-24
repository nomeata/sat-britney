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
import System.Directory
import Distribution.Simple.Utils (withTempFile)
import Control.Arrow ((***))

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map ((!))

type CNF = [Conj]
-- Conj is in DIMACS format, e.g. list of digits, followed by "0\n"
-- Also, remember largest variable
type Conj = (BS.ByteString, Int)

reorder :: [Int] -> Conj
reorder ls = m `seq` (BS.pack $ unwords (map show (sortBy (compare `on` abs) ls)) ++ " 0\n" , m)
  where m = maximum (map abs ls)

atom2Conj :: Int -> Conj
atom2Conj i = (BS.pack $ show i ++ " 0\n", i)

formatCNF :: CNF -> L.ByteString
formatCNF cnf = L.concat
    [ L.pack "c LitSat CNF generator\n"
    , L.pack $unwords ["p", "cnf", show maxVer, show numClauses] ++ "\n"
    , body
    ]
  where numClauses = length cnf
        (body, maxVer) = (L.fromChunks *** maximum) (unzip cnf)

formatCNFPMAX :: CNF -> [Int] -> L.ByteString
formatCNFPMAX cnf desired = L.concat $
    [ L.pack "c LitSat CNF generator\n"
    , L.pack $ unwords ["p", "wcnf", show maxVer, show (numClauses + numDesired), topN ] ++ "\n"
    , body ] ++
    map (\i -> L.pack $ "1 " ++ show i ++ " 0\n") desired
  where numClauses = length cnf
        numDesired = length desired
        topN = show (numDesired + 1)
        top = BS.pack (topN ++ " ")
        (body, maxVer) = (L.fromChunks . (top:) . intersperse top *** maximum) (unzip cnf)


parseCNF :: BS.ByteString -> CNF
parseCNF str =
    map (reorder . init . map int . BS.words) $
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
    L.hPut hint cnfString
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
                            L.hPut stderr $ formatCNF mus
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
        Right solution -> Right <$> runMSUnCore cnf desired -- whatsLeft cnf solution desired 
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

runMSUnCore :: CNF -> [Int] -> IO [Int]
runMSUnCore cnf desired = getTemporaryDirectory  >>= \tmpdir ->
    withTempFile tmpdir "sat-britney.dimacs" $ \tmpfile handle -> do
    let cnfString = formatCNFPMAX cnf desired

    L.hPut handle cnfString
    hClose handle

    (_, Just hout, _, procHandle) <- createProcess $
        (proc "./msuncore" ["-v", "0", tmpfile]) { std_out = CreatePipe }
    
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
            let ls = mapMaybe (\l ->
                        if BS.null l then Nothing
                        else if BS.head l == 'c' then Nothing
                        else if BS.head l == 'o' then Nothing
                        else if BS.head l == 'v' then Just (BS.drop 2 l)
                        else error $ "Cannot parse msuncore SAT output: " ++ BS.unpack l
                    ) $ BS.lines satvarsS
            let vars = case concatMap BS.words ls of 
                 ints@(_:_) -> filter (/= 0) . fmap int $ init ints
                 _ -> error $ "Cannot parse msuncore SAT output: " ++ BS.unpack satvarsS
            waitForProcess procHandle
            return vars
        s -> do
            error $ "Cannot parse msuncore status output: " ++ s

