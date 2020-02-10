module Version (searchVersions, filePath) where

import Control.Applicative ((<|>))
import Data.Foldable (fold)
import Data.List (intersperse)
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Data.Bifunctor (first)
import Data.Text (Text, pack, unpack)
import System.Process (shell, readCreateProcess, CreateProcess(..))
import Text.Parsec (ParseError, many, many1, sepBy, parse, oneOf, between)
import Text.Parsec.Char (alphaNum, char, spaces, string, digit)
import Text.Parsec.String (Parser)

searchVersions :: FilePath -> IO [PackageVersion]
searchVersions path = do
    let command =
            "git rev-list master -- "
            <> path
            <> " | parallel -q git grep -E '^\\s+version\\s?=\\s?\"[^\"]+\"\\s*;\\s*$' {} -- "
            <> path
            <> " || true"

    out <- runInNixPkgs command
    return $ either (error . show) id $ sequence $ fmap parsePackageVersion $ lines out

searchVersions' :: FilePath -> IO [PackageVersion]
searchVersions' path = do
    commits <- pathCommits path
    fold <$> traverse (versionMention path) commits


pathCommits :: FilePath -> IO [Hash]
pathCommits path = do
    let command = "git rev-list master -- " <> path
    out <- runInNixPkgs command
    return
        $ mapMaybe (either (const Nothing) Just)
        $ fmap parseCommitHash
        $ lines out

versionMention :: FilePath -> Hash -> IO [PackageVersion]
versionMention path (Hash hash) = do
    let command = "git grep -E '^\\s+version\\s?=\\s?\"[^\"]+\"\\s*;\\s*$' "
                <> hash
                <> " -- "
                <> path
                <> " || true" -- Prevent the command from failing by not finding a match
    out <- runInNixPkgs command
    return
        $ fromEitherList
        $ fmap parsePackageVersion
        $ lines out

runInNixPkgs :: String -> IO String
runInNixPkgs command =
    readCreateProcess ((shell command) { cwd = Just nixpkgsRepoPath }) ""
    where
        nixpkgsRepoPath = "/Users/marcelo/Projects/nixpkgs"

fromEitherList :: [Either b a] -> [a]
fromEitherList = mapMaybe (either (const Nothing) Just)

parsePackageVersion :: String -> Either ParseError PackageVersion
parsePackageVersion = parse packageVersionParser "Package Version"

parseCommitHash :: String -> Either ParseError Hash
parseCommitHash = parse hash "Commit Hash"


data PackageVersion = PackageVersion
    --  Name of package. e.g. nodejs
    { packageName :: Maybe String
    -- package version. e.g. 10.12.0
    , packageVersion :: Version
    -- path of file where the version was found.
    -- e.g. pkgs/development/web/nodejs/default.nix
    , packagePath :: FilePath
    -- commit hash of nixpkgs with that version.
    , nixpkgsHash :: Hash
    } deriving (Show, Eq)

packageVersionParser :: Parser PackageVersion
packageVersionParser = do
    h <- hash
    char ':'
    p <- filePath
    char ':'
    spaces *> string "version" *> spaces *> char '=' *> spaces
    v <- inQuotes version
    return $ PackageVersion
        { packageName = Nothing
        , packageVersion = v
        , packagePath = p
        , nixpkgsHash = h
        }

newtype Hash = Hash String
    deriving (Show, Eq)

hash :: Parser Hash
hash = Hash <$> many alphaNum

newtype Version = Version Text
    deriving (Show, Eq)

-- | A package version number.
-- May look like one of these:
--      5
--      5.0.0.2
--      v13.5
--      8.2.1-rc3
version :: Parser Version
version = Version . pack <$> aName

inQuotes :: Parser a -> Parser a
inQuotes = between (char '"') (char '"')

filePath :: Parser FilePath
filePath = aName `separatedBy` '/'

separatedBy :: Parser String -> Char -> Parser String
separatedBy p separator =
    fold . intersperse [separator] <$> p `sepBy` char separator

-- | A valid directory/file name
aName :: Parser String
aName = many1 (alphaNum <|> char '-' <|> char '_' <|> char ' ' <|> char '.')

