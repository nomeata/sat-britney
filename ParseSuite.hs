module ParseSuite where

import System.FilePath
import Data.Functor
import qualified Data.Map as M
import qualified Data.Set as S
import Text.ParserCombinators.Parsec hiding (SourceName)
import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified Data.Strict as ST
import System.IO

import Debian.Version.ByteString
import Debian.Control.ByteString
import Debian.Relation.ByteString

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
            let depends = maybe [] (either (error.show) id . parseRelations) $
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
        (S.toList sourceAtoms ++ binaryAtoms)
        sourceNames
        binaryNames
        builds
        builtBy
        (M.fromList binaryDepends)
        (M.fromList (concat binaryProvides))
        newerSources


parseProvides str = parse pProvides (BS.unpack str) (BS.unpack str)

pProvides :: CharParser () [String]
pProvides = do rel <- many pPkgName
               eof
               return rel

pPkgName = do skipMany (char ',' <|> whiteChar)
              pkgName <- many1 (noneOf [' ',',','|','\t','\n','('])
              skipMany (char ',' <|> whiteChar)
              return pkgName

whiteChar = oneOf [' ','\t','\n']
