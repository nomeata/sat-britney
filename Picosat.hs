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
    "p cnf " ++ show (safeMax (map abs (concat cnf))) ++ " " ++ show (length cnf) ++ "\n" ++
    concatMap (\l -> unwords (map show l) ++ " 0\n") cnf
  where safeMax [] = 0
        safeMax l = maximum l

parseCNF :: BS.ByteString -> CNF
parseCNF str =
    map (init . map int . BS.words) $
    dropWhile (\l -> BS.null l || BS.head l `elem` "cp") $
    BS.lines str

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
    
    result <- fix $ \next -> do
        line <- hGetLine hout
        if null line || head line == 'c' then next else return line
    case result of
        "s UNSATISFIABLE" -> do
            hClose hout
            core <- parseCNF <$> BS.hGetContents coreIn
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
            return (Right vars)
        s -> do
            error $ "Cannot parse picostat status output: " ++ s

-- Takes a CNF and removes clauses (and returns them) until it becomes
-- satisfiable. The first argument gives the CNFs to relax
relaxer :: S.Set Conj -> CNF -> IO (Either CNF CNF)
relaxer relaxable = go 
  where go cnf = do
            ret <- runPicosatCNF cnf
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

