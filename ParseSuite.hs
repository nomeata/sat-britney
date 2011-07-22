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
import qualified Data.Strict as ST
import System.IO

import Debian.Version.ByteString
import Debian.Control.ByteString
import Debian.Relation.Common

import Types

myParseControl file = do
    hPutStrLn stderr $ "Reading file " ++ file
    control <- parseControlFromFile file >>=
        either (error . show) (return . unControl)
    return control

parseSuite config dir = do
    {-
    sources <- myParseControl (dir </>"Sources")

    let sourceAtoms = [
            Source (SourceName pkg) (parseDebianVersion version) |
            para <- sources,
            let Just pkg = fieldValue "Package" para,
            let Just version = fieldValue "Version" para ]
    -}

    binaries <- concat <$>
        mapM (\arch -> myParseControl $ dir </>"Packages_" ++ show arch) (arches config)
    hPutStrLn stderr "Done reading input files."

    let (binaryAtoms, binaryDepends, binaryProvides, builtByList) = unzip4 [
            (atom, (atom, depends), provides, (atom, sourceAtom)) |
            para <- binaries,
            let Just pkg = fieldValue "Package" para,
            let Just versionS = fieldValue "Version" para,
            let version = parseDebianVersion versionS,
            let Just archS = fieldValue "Architecture" para,
            let arch = if archS == BS.pack "all" then ST.Nothing else ST.Just (Arch archS),
            let atom = Binary (BinName pkg) version arch,
            let depends = maybe [] (either (error.show) id . parseDependency) $
                                fieldValue "Depends" para,
            let provides = [
                    ((BinName (BS.pack provide), providedArch), [atom]) |
                    provide <- maybe [] (either (error.show) id . parseProvides) $
                                fieldValue "Provides" para,
                    providedArch <- case arch of 
                        ST.Just arch -> [arch]
                        ST.Nothing -> arches config
                    ],

            let sourceS = fromMaybe pkg (fieldValue "Source" para),
            let (source,sourceVersion) = case BS.words sourceS of
                    [s,sv] | BS.head sv == '(' && BS.last sv == ')' 
                           -> (s, parseDebianVersion (BS.init (BS.tail sv)))
                    [s]    -> (s, version),
            let sourceAtom = Source (SourceName source) sourceVersion
            ]

    let builtBy = M.fromList builtByList
    let builds = M.fromListWith (++) [ (src,[bin]) | (bin,src) <- builtByList ]

    let binaryNames = M.fromListWith (++) 
            [ ((pkg,arch), [atom]) |
                atom <- binaryAtoms,
                let Binary pkg _ mbArch = atom,
                arch <- case mbArch of { ST.Just arch -> [arch] ; ST.Nothing -> arches config }
                ]

    -- We use the sources found in the Packages file as well, because they
    -- are not always in SOURCES
    let sourceAtoms = S.fromList (M.elems builtBy)

    let sourceNames =  M.fromListWith (++)
            [ (pkg, [atom]) | atom@(Source pkg _) <- S.toList sourceAtoms ]

    let newerSources = M.fromListWith (++) [ (source, newer) |
            sources <- M.elems sourceNames, 
            let sorted = sort sources,
            source:newer <- tails sorted
            ]

    return $ SuiteInfo
        (sourceAtoms `S.union` S.fromList binaryAtoms)
        sourceNames
        binaryNames
        builds
        builtBy
        (M.fromList binaryDepends)
        (M.fromList (concat binaryProvides))
        newerSources


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
       return $ Just (op (parseDebianVersion version))
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
pArchExcept :: RelParser [String]
pArchExcept = sepBy (char '!' >> many1 (noneOf [']',' '])) (skipMany1 whiteChar)

pArchOnly :: RelParser [String]
pArchOnly = sepBy (many1 (noneOf [']',' '])) (skipMany1 whiteChar)


parseProvides str = parse pProvides (BS.unpack str) str

pProvides :: Parser [String]
pProvides = do rel <- many pPkgName
               eof
               return rel

pPkgName = do skipMany (char ',' <|> whiteChar)
              pkgName <- many1 (noneOf [' ',',','|','\t','\n','('])
              skipMany (char ',' <|> whiteChar)
              return pkgName
