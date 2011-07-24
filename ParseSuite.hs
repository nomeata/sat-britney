{-# LANGUAGE OverloadedStrings #-}
module ParseSuite where

import System.FilePath
import Data.Functor
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Parsec
import Text.Parsec.ByteString
import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Nums.Careless
import qualified Data.Strict as ST
import System.IO
import System.Directory
import Data.Time
import Data.Char
import Data.Function
import Control.DeepSeq
import Control.Arrow (first)

import ControlParser
import Types

myParseControl file = do
    hPutStrLn stderr $ "Reading file " ++ file
    parseControlFile file

parseSuite :: Config -> AtomIndex -> FilePath -> IO (SuiteInfo, AtomIndex)
parseSuite config ai dir = do
    {-
    sources <- myParseControl (dir </>"Sources")

    let sourceAtoms = [
            Source (SourceName pkg) (DebianVersion version) |
            para <- sources,
            let Just pkg = fieldValue "Package" para,
            let Just version = fieldValue "Version" para ]
    -}

    binaries <- concat <$>
        mapM (\arch -> myParseControl $ dir </>"Packages_" ++ show arch) (arches config)

    let (binaryAtoms, binaryDepends, binaryProvides, builtByList, ai') = readPara ai binaries
        readPara ai [] = ([], [], [], [], ai)
        readPara ai (para:ps) = (binI:bins, (binI,depends):deps, provides ++ provs, (binI,srcI): bb,finalAi)
          where (bins, deps, provs, bb, finalAi) = readPara ai'' ps
                pkg = packageField para
                version = DebianVersion (versionField para)
                archS = architectureField para
                arch = if archS == BS.pack "all" then ST.Nothing else ST.Just (Arch archS)
                atom = Binary (BinName pkg) version arch
                (ai',binI) = addBin ai atom
                depends = parseDependency $ dependsField para
                provides = [
                    ((BinName (BS.pack provide), providedArch), [binI]) |
                    provide <- either (error.show) id . parseProvides $ providesField para,
                    providedArch <- case arch of 
                        ST.Just arch -> [arch]
                        ST.Nothing -> arches config
                    ]
                (source,sourceVersion) = case BS.words (sourceField para) of
                    []     -> (pkg, version)
                    [s,sv] | BS.head sv == '(' && BS.last sv == ')' 
                           -> (s, DebianVersion (BS.init (BS.tail sv)))
                    [s]    -> (s, version)
                sourceAtom = Source (SourceName source) sourceVersion
                (ai'', srcI) = addSrc ai' sourceAtom

    hPutStrLn stderr $ "Calculating builtBy"
    let builtBy = {-# SCC "builtBy" #-} M.fromList builtByList
    builtBy `deepseq` return ()

    hPutStrLn stderr $ "Calculating builds"
    let builds = {-# SCC "builds" #-}  M.fromListWith (++) [ (src,[bin]) | (bin,src) <- builtByList ]
    builds `deepseq` return ()

    hPutStrLn stderr $ "Calculating depends"
    let depends = {-# SCC "depends" #-} M.fromList binaryDepends
    depends `deepseq` return ()

    hPutStrLn stderr $ "Calculating provides"
    let provides = {-# SCC "provides" #-} M.fromList binaryProvides
    provides `deepseq` return ()

    hPutStrLn stderr $ "Calculating binaryNames"
    let binaryNames = {-# SCC "binaryNames" #-} M.fromListWith (++) 
            [ ((pkg,arch), [binI]) |
                binI <- binaryAtoms,
                let Binary pkg _ mbArch = ai' `lookupBin` binI,
                arch <- case mbArch of { ST.Just arch -> [arch] ; ST.Nothing -> arches config }
                ]
    binaryNames `deepseq` return ()

    -- We use the sources found in the Packages file as well, because they
    -- are not always in SOURCES
    hPutStrLn stderr $ "Calculating sourceAtoms"
    let sourceAtoms = {-# SCC "sourceAtoms" #-} S.fromList (M.elems builtBy)
    sourceAtoms `deepseq` return ()

    hPutStrLn stderr $ "Calculating sourceNames"
    let sourceNames = {-# SCC "sourceNames" #-} M.fromListWith (++)
            [ (pkg, [srcI]) | srcI <- S.toList sourceAtoms,
                              let Source pkg _  = ai' `lookupSrc` srcI ]
    sourceNames `deepseq` return ()

    hPutStrLn stderr $ "Calculating newerSources"
    let newerSources = {-# SCC "newerSource" #-} M.fromListWith (++) [ (source, newer) |
            srcIs <- M.elems sourceNames, 
            let sources = [ (v, srcI) | srcI <- srcIs , let Source _ v  = ai' `lookupSrc` srcI ],
            let sorted = map snd $ sortBy (cmpDebianVersion `on` fst) sources,
            source:newer <- tails sorted
            ]
    newerSources `deepseq` return ()

    hPutStrLn stderr $ "Calculating binaries"
    let binaries = {-# SCC "binaries" #-} S.fromList binaryAtoms
    binaries `deepseq` return ()

    hPutStrLn stderr $ "Calculating atoms"
    let atoms = {-# SCC "atoms" #-} S.mapMonotonic genIndex sourceAtoms `S.union` S.mapMonotonic genIndex binaries
    atoms `deepseq` return ()

    -- Now to the bug file
    hPutStrLn stderr "Reading and parsing bugs file"
    bugS <- BS.readFile (dir </> "BugsV")

    let (rawBugs, ai'') = {-# SCC "rawBugs" #-} first M.fromList $ addBugList ai' (BS.lines bugS)
        addBugList finalAi [] = ([], finalAi)
        addBugList ai (l:ls) = if BS.null l 
                               then addBugList ai ls
                               else first ((pkg, bugIs):) (addBugList ai' ls)
          where [pkg,buglist] = BS.words l
                bugs = Bug . int <$> BS.split ',' buglist
                (ai',bugIs) = mapAccumL addBug ai bugs
    
    let bugs = {-# SCC "bugs" #-} set2MapNonEmpty (\i -> case ai'' `lookupAtom` i of
            SrcAtom (Source sn' _) ->
                let sn = unSourceName sn'
                in  M.findWithDefault [] sn rawBugs ++
                    M.findWithDefault [] (BS.pack "src:" `BS.append` sn) rawBugs
            BinAtom (Binary bn' _ _) -> 
                let bn = unBinName bn'
                in  M.findWithDefault [] bn rawBugs 
            ) atoms
    bugs `deepseq` return ()

    hPutStrLn stderr $ "Done reading input files, " ++ show (S.size sourceAtoms) ++
                       " sources, " ++ show (S.size binaries) ++ " binaries."
    return $ (SuiteInfo
        sourceAtoms
        binaries
        atoms
        sourceNames
        binaryNames
        builds
        builtBy
        depends
        provides
        newerSources
        bugs
        , ai'')

parseGeneralInfo :: Config -> AtomIndex -> IO GeneralInfo
parseGeneralInfo config ai = do 
    -- Now the URGENCY file (may not exist)
    hPutStrLn stderr "Reading and parsing urgency file"
    urgencies <- parseUrgencyFile (dir config </> "testing" </> "Urgency") ai
    urgencies `deepseq` return ()

    -- Now the Dates file (may not exist)
    hPutStrLn stderr "Reading and parsing dates file"
    ages <- parseAgeFile (dir config </> "teting" </> "Dates") ai
    ages `deepseq` return ()

    hPutStrLn stderr $ "Done reading general input files"
    return $ GeneralInfo urgencies ages

parseUrgencyFile :: FilePath -> AtomIndex -> IO (M.Map SrcI Urgency)
parseUrgencyFile file ai = do
    ex <- doesFileExist file
    urgencyS <- if ex then BS.readFile file else return BS.empty

    return $ M.fromList [ (srcI, urgency) | 
            line <- BS.lines urgencyS,
            not (BS.null line),
            let [pkg,version,urgencyS] = BS.words line,
            not (isAlpha (BS.head version)),
            let src = Source (SourceName pkg) (DebianVersion version),
            Just srcI <- [ai `indexSrc` src],
            let urgency = Urgency urgencyS
            ]

parseAgeFile :: FilePath -> AtomIndex -> IO (M.Map SrcI Age)
parseAgeFile file ai = do
    ex <- doesFileExist file
    dateS <- if ex then BS.readFile file else return BS.empty

    -- Timeszone?
    now <- utctDay <$> getCurrentTime
    let epochDay = fromGregorian 1970 1 1
    return $ M.fromList [ (srcI, Age age) | 
            line <- BS.lines dateS,
            not (BS.null line),
            let [pkg,version,dayS] = BS.words line,
            not (BS.pack "upl" `BS.isPrefixOf` version),
            let src = Source (SourceName pkg) (DebianVersion version),
            Just srcI <- [ai `indexSrc` src],
            let age = {-# SCC "ageCalc" #-} fromIntegral $ now `diffDays` (int dayS `addDays` epochDay)
            ]

parseDependency :: BS.ByteString -> Dependency
parseDependency str = parseDisj <$> BS.split ',' (BS.dropWhile isSpace str)

parseDisj :: BS.ByteString -> DepDisj
parseDisj str' = (parseRel <$> BS.split '|' str, str)
  where str = BS.dropWhile isSpace str'

parseRel :: BS.ByteString -> DepRel
parseRel str' = DepRel (BinName pkg) verReq archReq
  where str = BS.dropWhile isSpace str'
        (pkg, rest1') = BS.break isSpace str
        rest1 = BS.dropWhile isSpace rest1'
        (verReq, rest2')  | BS.null rest1        = (Nothing, rest1)
                          | BS.head rest1 == '(' = first (Just . parseVerReq) $
                                                   BS.break (==')') (BS.tail rest1)
                          | otherwise            = (Nothing, rest1)
        rest2 = BS.dropWhile isSpace . BS.dropWhile (==')') $ rest2'
        (archReq, rest3') | BS.null rest2        = (Nothing, rest2)
                          | BS.head rest2 == '[' = first (Just . parseArchReq) $
                                                   BS.break (==']') (BS.tail rest2)
                          | otherwise            = (Nothing, rest2)

parseVerReq :: BS.ByteString -> VersionReq
parseVerReq str = case BS.words str of
    [rel, ver] -> parseVerReqRel rel (DebianVersion ver)
    [] -> error $ "Cannot parse Version requirement " ++ show str

parseVerReqRel :: BS.ByteString -> DebianVersion -> VersionReq
parseVerReqRel str v =
    if str == "<" || str == "<<" then SLT v else
    if str == "<="               then LTE v else
    if str == "="                then EEQ v else
    if str == ">="               then GRE v else
    if str == ">" || str == ">>" then SGR v else
    error $ "Cannot parse version relation " ++ show str
      
parseArchReq :: BS.ByteString -> ArchitectureReq
parseArchReq str | BS.null str = error "Empty Architecture requirement"
parseArchReq str = t arches
 where t = if BS.head str == '!' then ArchExcept else ArchOnly
       arches = fmap Arch $
                filter (not . BS.null) $
                BS.splitWith (\c -> isSpace c || c `elem` ",!") $
                str

parseProvides str = parse pProvides (BS.unpack str) str

pProvides :: Parser [String]
pProvides = do rel <- many pPkgName
               eof
               return rel

pPkgName = do skipMany (char ',' <|> whiteChar)
              pkgName <- many1 (noneOf [' ',',','|','\t','\n','('])
              skipMany (char ',' <|> whiteChar)
              return pkgName

whiteChar = oneOf [' ','\t','\n']

set2MapNonEmpty :: (a -> [b]) -> S.Set a -> M.Map a [b]
set2MapNonEmpty f s = M.fromDistinctAscList [ (k, v) | k <- S.toAscList s, let v = f k, not (null v) ]

set2Map :: (a -> b) -> S.Set a -> M.Map a b
set2Map f s = M.fromDistinctAscList [ (k, f k) | k <- S.toAscList s ]
