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
                depends = either (error.show) id . parseDependency $ dependsField para
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

parseDependency :: BS.ByteString -> Either ParseError Dependency
parseDependency str = parse pRelations (BS.unpack str) str

type RelParser a = Text.Parsec.ByteString.Parser a

-- "Correct" dependency lists are separated by commas, but sometimes they
-- are omitted and it is possible to parse relations without them.
pRelations :: RelParser Dependency
pRelations = do -- rel <- sepBy pOrRelation (char ',')
		rel <- many pOrRelation
                eof
                return rel

withInput :: RelParser a -> RelParser (a, BS.ByteString)
withInput p = do
    input1 <- getInput
    ret <- p
    input2 <- getInput
    return (ret, BS.take (BS.length input1 - BS.length input2) input1)

pOrRelation :: RelParser DepDisj
pOrRelation = do skipMany (char ',' <|> whiteChar)
                 rel <- withInput (sepBy1 pRelation (char '|'))
                 skipMany (char ',' <|> whiteChar)
                 return rel

whiteChar = oneOf [' ','\t','\n']

pRelation :: RelParser DepRel
pRelation =
    do skipMany whiteChar
       pkgName <- many1 (noneOf [' ',',','|','\t','\n','('])
       skipMany whiteChar
       mVerReq <- pMaybeVerReq
       skipMany whiteChar
       mArch <- pMaybeArch
       return $ DepRel (BinName (BS.pack pkgName)) mVerReq mArch

pMaybeVerReq :: RelParser (Maybe VersionReq)
pMaybeVerReq =
    do char '('
       skipMany whiteChar
       op <- pVerReq
       skipMany whiteChar
       version <- many1 (noneOf [' ',')','\t','\n'])
       skipMany whiteChar
       char ')'
       return $ Just (op (DebianVersion (BS.pack version)))
    <|>
    do return $ Nothing

pVerReq =
    do char '<'
       (do char '<' <|> char ' ' <|> char '\t'
	   return $ SLT
        <|>
        do char '='
	   return $ LTE)
    <|>
    do string "="
       return $ EEQ
    <|>
    do char '>'
       (do char '='
 	   return $ GRE
        <|>
        do char '>' <|> char ' ' <|> char '\t'
	   return $ SGR)

pMaybeArch :: RelParser (Maybe ArchitectureReq)
pMaybeArch =
    do char '['
       (do archs <- pArchExcept
	   char ']'
           skipMany whiteChar
	   return (Just (ArchExcept archs))
	<|>
	do archs <- pArchOnly
	   char ']'
           skipMany whiteChar
	   return (Just (ArchOnly archs))
	)
    <|>
    return Nothing

-- Some packages (e.g. coreutils) have architecture specs like [!i386
-- !hppa], even though this doesn't really make sense: once you have
-- one !, anything else you include must also be (implicitly) a !.
pArchExcept :: RelParser [Arch]
pArchExcept = map (Arch . BS.pack) <$> sepBy (char '!' >> many1 (noneOf [']',' '])) (skipMany1 whiteChar)

pArchOnly :: RelParser [Arch]
pArchOnly = map (Arch . BS.pack) <$> sepBy (many1 (noneOf [']',' '])) (skipMany1 whiteChar)


parseProvides str = parse pProvides (BS.unpack str) str

pProvides :: Parser [String]
pProvides = do rel <- many pPkgName
               eof
               return rel

pPkgName = do skipMany (char ',' <|> whiteChar)
              pkgName <- many1 (noneOf [' ',',','|','\t','\n','('])
              skipMany (char ',' <|> whiteChar)
              return pkgName

set2MapNonEmpty :: (a -> [b]) -> S.Set a -> M.Map a [b]
set2MapNonEmpty f s = M.fromDistinctAscList [ (k, v) | k <- S.toAscList s, let v = f k, not (null v) ]

set2Map :: (a -> b) -> S.Set a -> M.Map a b
set2Map f s = M.fromDistinctAscList [ (k, f k) | k <- S.toAscList s ]
