{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
-- |
-- Module: ParseSuite
-- Copyright: (c) 2011 Joachim Breitner
-- License: GPL-2
--
module ParseSuite where

import System.FilePath
import System.Directory
import System.Time
import Data.Functor
import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.ByteString
import Data.List
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Nums.Careless
import qualified Data.Strict as ST
import System.IO
import Data.Time
import Data.DateTime ( fromClockTime )
import Data.Char
import Data.Function
import Control.DeepSeq
import Control.Arrow (first)

import ControlParser
import Types
import AtomIndex
import Indices
import qualified IndexMap as IxM
import qualified IndexSet as IxS

myParseControl file = do
    hPutStrLn stderr $ "Reading file " ++ file
    parseControlFile file

parseSuite :: Config -> AtomIndex -> FilePath -> IO (SuiteInfo, RawPackageInfo, AtomIndex)
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

    let ( binaryAtoms
         ,binaryNamesList
         ,binaryDepends
         ,binaryProvides
         ,binaryConflicts
         ,binaryBreaks
         ,builtByList
         ,ai') = readPara ai binaries
        readPara ai [] = ([], [], [], [], [], [], [], ai)
        readPara ai (Para {..}:ps) =
                    ( binI:bins
                    , binNamesEntries ++ binNames
                    , (binI,depends++preDepends):deps
                    , provides ++ provs
                    , (binI,conflicts):confls
                    , (binI,breaks):brks
                    , (binI,srcI): bb
                    ,finalAi)
          where (bins, binNames, deps, provs, confls, brks, bb, finalAi) = readPara ai'' ps
                pkg = packageField
                version = DebianVersion versionField
                archS = architectureField
                (arch,onArches) = if archS == "all"
                                  then (ST.Nothing, arches config)
                                  else (ST.Just (Arch archS), [Arch archS])
                binNamesEntries = [ ((BinName pkg,a),[binI]) | a <- onArches ]
                atom = Binary (BinName pkg) version arch
                (ai',binI) = addBin ai atom
                depends = parseDependency $ dependsField
                preDepends = parseDependency $ preDependsField
                conflicts = parseDependency $ conflictsField
                breaks = parseDependency $ breaksField
                provides = [
                    ((BinName (BS.pack provide), providedArch), [binI]) |
                    provide <- either (error.show) id . parseProvides $ providesField,
                    providedArch <- case arch of 
                        ST.Just arch -> [arch]
                        ST.Nothing -> arches config
                    ]
                (source,sourceVersion) = case BS.words sourceField of
                    []     -> (pkg, version)
                    [s,sv] | BS.head sv == '(' && BS.last sv == ')' 
                           -> (s, DebianVersion (BS.init (BS.tail sv)))
                    [s]    -> (s, version)
                sourceAtom = Source (SourceName source) sourceVersion
                (ai'', srcI) = addSrc ai' sourceAtom

    let builtBy = {-# SCC "builtBy" #-} IxM.fromList builtByList

    let builds = {-# SCC "builds" #-}  IxM.fromListWith (++) [ (src,[bin]) | (bin,src) <- builtByList ]

    let depends = {-# SCC "depends" #-} IxM.fromList binaryDepends

    let provides = {-# SCC "provides" #-} M.fromListWith (++) binaryProvides

    let conflicts = {-# SCC "conflicts" #-} IxM.fromList binaryConflicts

    let breaks = {-# SCC "breaks" #-} IxM.fromList binaryBreaks

    let binaryNames = {-# SCC "binaryNames" #-} M.fromListWith (++) binaryNamesList

    -- We use the sources found in the Packages file as well, because they
    -- are not always in SOURCES
    let sourceAtoms = {-# SCC "sourceAtoms" #-} IxS.fromList (IxM.keys builds)

    let sourceNames = {-# SCC "sourceNames" #-} M.fromListWith (++)
            [ (pkg, [srcI]) | srcI <- IxS.toList sourceAtoms,
                              let Source pkg _  = ai' `lookupSrc` srcI ]

    let newerSources = {-# SCC "newerSource" #-} IxM.fromListWith (++) [ (source, newer) |
            srcIs <- M.elems sourceNames, 
            let sources = [ (v, srcI) | srcI <- srcIs , let Source _ v  = ai' `lookupSrc` srcI ],
            let sorted = map snd $ sortBy (cmpDebianVersion `on` fst) sources,
            source:newer <- tails sorted
            ]

    let binaries = {-# SCC "binaries" #-} IxS.fromList binaryAtoms

    let atoms = {-# SCC "atoms" #-} IxS.generalize sourceAtoms `IxS.union` IxS.generalize binaries

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
                    M.findWithDefault [] ("src:" `BS.append` sn) rawBugs
            BinAtom (Binary bn' _ _) -> 
                let bn = unBinName bn'
                in  M.findWithDefault [] bn rawBugs 
            ) atoms

    hPutStrLn stderr $ "Done reading input files, " ++ show (IxS.size sourceAtoms) ++
                       " sources, " ++ show (IxS.size binaries) ++ " binaries."
    return
        ( SuiteInfo
            sourceAtoms
            binaries
            atoms
            sourceNames
            binaryNames
            builds
            newerSources
            bugs
        , RawPackageInfo
            binaryNames
            builtBy
            depends
            provides
            conflicts
            breaks
        , ai'')

parseGeneralInfo :: Config -> AtomIndex -> IO GeneralInfo
parseGeneralInfo config ai = do 
    -- Now the URGENCY file (may not exist)
    hPutStrLn stderr "Reading and parsing urgency file"
    urgencies <- parseUrgencyFile (dir config </> "testing" </> "Urgency") ai
--    urgencies `deepseq` return ()

    -- Now the Dates file (may not exist)
    hPutStrLn stderr "Reading and parsing dates file"
    -- If a package has been given at the command line, and it happens to be the index
    -- of a source package, we remove it. Removing the index of something that
    -- is not a source pacackge is a noop.
    ages <- maybe id (M.delete . (\(Index i) -> Index i)) (migrateThisI config) <$>
        parseAgeFile config  ai
--    ages `deepseq` return ()



    hPutStrLn stderr $ "Done reading general input files"
    return $ GeneralInfo urgencies ages

parseUrgencyFile :: FilePath -> AtomIndex -> IO (M.Map SrcI Urgency)
parseUrgencyFile file ai = do
    urgencyS <- BS.readFile file

    return $ M.fromList [ (srcI, urgency) | 
            line <- BS.lines urgencyS,
            not (BS.null line),
            let [pkg,version,urgencyS] = BS.words line,
            not (isAlpha (BS.head version)),
            let src = Source (SourceName pkg) (DebianVersion version),
            Just srcI <- [ai `indexSrc` src],
            let urgency = Urgency urgencyS
            ]

parseAgeFile :: Config -> AtomIndex -> IO (M.Map SrcI Age)
parseAgeFile config ai = do
    let filename = dir config </> "testing" </> "Dates"
    dateS <- BS.readFile filename

    -- Timeszone?
    now <- case True of -- whether to use file timestamp for "now"
        True -> utctDay . fromClockTime <$> getModificationTime filename
        False -> utctDay <$> getCurrentTime
    let now' = offset config `addDays`now
    let epochDay = fromGregorian 1970 1 1
    return $ M.fromList [ (srcI, Age age) | 
            line <- BS.lines dateS,
            not (BS.null line),
            let [pkg,version,dayS] = BS.words line,
            not ("upl" `BS.isPrefixOf` version),
            let src = Source (SourceName pkg) (DebianVersion version),
            Just srcI <- [ai `indexSrc` src],
            let age = {-# SCC "ageCalc" #-} fromIntegral $ now' `diffDays` (int dayS `addDays` epochDay)
            ]

parseDependency :: BS.ByteString -> Dependency
parseDependency str = parseDisj <$> BS.split ',' (BS.dropWhile isSpace str)

parseDisj :: BS.ByteString -> DepDisj
parseDisj str' = rel `seq` str `seq` (rel,str)
  where rel = parseRel <$> BS.split '|' str
        str = BS.dropWhile isSpace str'

parseRel :: BS.ByteString -> DepRel
parseRel str' = DepRel (BinName pkg) verReq archReq
  where str = BS.dropWhile isSpace str'
        (pkg, rest1') = BS.break isSpace str
        rest1 = BS.dropWhile isSpace rest1'
        (verReq, rest2')  | BS.null rest1        = (ST.Nothing, rest1)
                          | BS.head rest1 == '(' = first (ST.Just . parseVerReq) $
                                                   BS.break (==')') (BS.tail rest1)
                          | otherwise            = (ST.Nothing, rest1)
        rest2 = BS.dropWhile isSpace . BS.dropWhile (==')') $ rest2'
        (archReq, rest3') | BS.null rest2        = (ST.Nothing, rest2)
                          | BS.head rest2 == '[' = first (ST.Just . parseArchReq) $
                                                   BS.break (==']') (BS.tail rest2)
                          | otherwise            = (ST.Nothing, rest2)

parseVerReq :: BS.ByteString -> VersionReq
parseVerReq str =
    let (rel, ver) = BS.span (`elem` "<=>") str
    in  parseVerReqRel rel (DebianVersion (BS.dropWhile isSpace ver))

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
parseArchReq str = arches `deepseq` t arches
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

set2MapNonEmpty :: (Index a -> [b]) -> IxS.Set a -> IxM.Map a [b]
set2MapNonEmpty f s = IxM.fromDistinctAscList [ (k, v) | k <- IxS.toAscList s, let v = f k, not (null v) ]

set2Map :: (Index a -> b) -> IxS.Set a -> IxM.Map a b
set2Map f s = IxM.fromDistinctAscList [ (k, f k) | k <- IxS.toAscList s ]
