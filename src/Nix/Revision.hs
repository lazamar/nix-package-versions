{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{-| This module retrieves and parses information about Nix revisions.
   Revisions are available at https://nixos.org/nixos/packages
-}

module Nix.Revision
    ( downloadFromNix
    , nixpkgs
    , headAt
    , downloadVersionsInPeriod
    , load
    , gheadAt
    , gnixpkgs

    , commitToFileName
    , fileNameToCommit
    , Revision(..)
    , Package(..)
    ) where

import Control.Exception (Exception(..), SomeException(..), throw, catch, tryJust)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (unless, (<=<), join)
import Control.Monad.Except (runExceptT, liftEither, MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (ToJSON, FromJSON, eitherDecodeFileStrict, parseJSON, withObject, (.:), (.:?), parseJSON)
import Data.Bifunctor (bimap, first)
import Data.Either (isRight)
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe)
import Data.Text (pack, unpack, Text)
import Data.Time.Calendar (Day, fromGregorian, toGregorian, showGregorian)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Foldable (asum, find)
import Data.Typeable (Typeable, cast)
import GHC.Generics (Generic)
import Nix.Cache (Cache(..))
import Nix.Versions.Types (NixConfig(..), Config(..), Channel(..), Hash(..), Name, Version(..), Commit(..), Repo(..))
import System.Directory (listDirectory)
import System.Exit (ExitCode(..))
import System.Posix.Files (fileExist, removeLink)
import System.Process (readCreateProcessWithExitCode, shell, CreateProcess(..))
import Text.Parsec (parse, spaces)
import Text.Read (readMaybe)

import qualified Network.HTTP.Req as Req
import qualified Nix.Versions.Parsers as Parsers

-- | The contents of a json file with package information
data Revision = Revision
    { commit :: Commit
    , packages :: HashMap Name Package
    } deriving (Generic)

-- | The information we have about a nix package in one revision
data Package = Package
    { description :: Maybe Text
    , version :: Version
    , nixpkgsPath :: Maybe FilePath
    } deriving (Show, Generic, Eq)

instance FromJSON Package where
    parseJSON = withObject "Package" $ \v -> Package
       <$> (v .: "meta" >>= (.:? "description"))
       <*>  v .:  "version"
       <*> (v .: "meta" >>= (.:? "position"))
       where
        -- | take some/path:123 and return some/path
        removeLineNumber :: FilePath -> FilePath
        removeLineNumber rawPath =
            case parse Parsers.filePath "Remove line number" rawPath of
                Left _  -> rawPath
                Right f -> f

--

-- | Download lists of packages and their versions
-- for commits between from date and to date.
-- Downloads one package list for each month in period.
downloadVersionsInPeriod :: FilePath -> Day -> Day -> IO [Either (Day, String) (Commit, FilePath)]
downloadVersionsInPeriod dir from to = do
    commits <- mapMaybe id <$> mapConcurrently (headAt nixpkgs) dates
    mapConcurrently (download <=< announce) commits
    where
        (fromYear, _, _) = toGregorian from

        (toYear, _, _) = toGregorian to

        dates
            = takeWhile (<= to)
            $ dropWhile (< from)
            $ [fromGregorian year month 1
              | year <- [fromYear .. toYear]
              , month <- [1..12]
              ]

        announce (Commit hash date) = do
            putStrLn $ "Downloading files for " <> showGregorian date
            return (Commit hash date)

        download commit@(Commit _ date) = do
            result <- downloadFromNix dir commit
            return $ bimap (date,) (commit,) result


-------------------------------------------
-- Loading packages


data FileType
    = RawNixVersions
    | Preprocessed

nixpkgs :: Repo
nixpkgs = Repo "../nixpkgs"

-- Constants
c_DOWNLOADS_DIRECTORY = "./saved-versions"
--
-- | Get the place where a JSON file with package versions should be saved.
filePath :: FilePath -> FileType -> Commit -> FilePath
filePath dir fileType (Commit (Hash hash) day) = dir <> "/" <> name
    where
        name = showGregorian day <> "-" <> unpack hash <> "-" <> suffix fileType <> ".json"

        suffix RawNixVersions = "raw"
        suffix Preprocessed = "preprocessed"


type Url = String

revisionUrl :: Hash -> Url
revisionUrl (Hash hash) = "https://github.com/NixOS/nixpkgs/archive/" <> unpack hash <> ".tar.gz"


-- Exceptions

-- All possible exceptions that may happen in our program
data Error
    = Uncaught Text
    | UnableToParseCommitDate Hash
    deriving (Typeable, Show)

instance Exception Error where
    toException = SomeException
    fromException e = fromMaybe (Just $ Uncaught $ pack $ show e) (cast e)
    displayException = show


--------------------------------------------------------------
-- Git

-- | Returns the commit that was the head on that date at 00:00 UTC
headAt :: Repo -> Day -> IO (Maybe Commit)
headAt repo day = do
    output <- runInRepo repo $ "git log --format='%cs %H' --max-count=1 --date=unix --until " <> show posix
    return $ eitherToMaybe $ first show . parse commitParser "Commit" =<< output
    where
        posix = floor $ utcTimeToPOSIXSeconds $ UTCTime day $ fromInteger 0

        -- Parses something that looks like this
        --  2015-02-12 a43db5fa2025c998ce0d72dc7dd425152d26ad59
        commitParser = do
            day <- Parsers.day
            spaces
            hash <- Parsers.hash
            return $ Commit hash day

runInRepo :: Repo -> String -> IO (Either String String)
runInRepo (Repo path) command = run $ (shell command) { cwd = Just path }

run :: CreateProcess -> IO (Either String String)
run cmd = do
    (exitCode, stdOut, stdErr) <- readCreateProcessWithExitCode cmd ""
    return $ case exitCode of
      ExitSuccess   -> Right stdOut
      ExitFailure _ -> Left stdErr


eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

----------------------------
-- GitHub

gnixpkgs :: GitHubRepo
gnixpkgs = GitHubRepo
    { g_user = "NixOS"
    , g_repo = "nixpkgs"
    }

data GitHubRepo = GitHubRepo
    { g_user :: Text
    , g_repo :: Text
    }

newtype GitHubCommit = GitHubCommit
    { unGitHubCommit :: Commit
    }

instance FromJSON GitHubCommit where
    parseJSON = withObject "GitHubCommit " $ \v ->
        handle
        <$> (v .: "sha")
        <*> (v .: "commit" >>= (.: "committer") >>= (.: "date"))
       where
           handle sha date = GitHubCommit $ Commit (Hash sha) (readGregorian date)

           readGregorian :: String -> Day
           readGregorian = read . take 10

-- | Fetch the commit that was the HEAD at 0 hours on the specified day
gheadAt :: GitHubRepo -> Day -> IO (Either String Commit)
gheadAt repo day = runExceptT $ do
    response <- catching  isHttpException
        $ Req.req Req.GET url Req.NoReqBody Req.jsonResponse options

    liftEither
        $ maybe (Left "Received empty commit list from remote") Right
        $ fmap unGitHubCommit
        $ listToMaybe
        $ Req.responseBody response
    where
        options =
            Req.header "User-Agent" "lazamar"
            <>
            Req.queryParam "until" (Just $ pack $ showGregorian day <> "T00:00:00Z")

        url = Req.https "api.github.com"
            Req./: "repos"
            Req./: g_user repo
            Req./: g_repo repo
            Req./: "commits"

        a </> b = a <> "/" <> b

        isHttpException :: Req.HttpException -> Maybe String
        isHttpException = Just . show

        catching exc
            = join
            . liftIO
            . fmap liftEither
            . tryJust exc
            . Req.runReq Req.defaultHttpConfig

--

type CommitCache m = Cache m Day Commit

instance (MonadIO m, NixConfig m) => Cache m Day Commit where
    getCached day = do
        config <- getConfig
        files  <- liftIO $ listDirectory $ config_revisionsDir config
        return $ find ((day ==) . commitDate) $ mapMaybe fileNameToCommit files
        where
            commitDate (Commit hash date) = date

    getUncached day = liftIO $ gheadAt gnixpkgs day

    cachedKeys = return []


ext :: String
ext = ".json"

commitToFileName :: Commit -> String
commitToFileName  (Commit (Hash hash) date) =
    showGregorian date <> "-" <> unpack hash <> ext

fileNameToCommit :: String -> Maybe (Commit)
fileNameToCommit  fname = Commit hash <$> readMaybe (take 10 fname)
    where
        hash = Hash $ pack $ dropTail (length ext) $ drop 11 fname

        dropTail n s = reverse $ drop n $ reverse s

type RevisionCache m = Cache m Commit Revision

instance (MonadIO m, NixConfig m) => Cache m Commit Revision where
    getCached commit = do
        Config { config_revisionsDir } <- getConfig
        keys <- cachedKeys
        if elem commit keys
           then liftIO $ either (const Nothing) Just <$> load config_revisionsDir commit
           else return Nothing

    getUncached commit = do
        Config { config_revisionsDir } <- getConfig
        res <- liftIO $ downloadFromNix config_revisionsDir commit
        case res of
            Left err -> return $ Left err
            Right _  -> liftIO $ load config_revisionsDir commit

    cachedKeys = do
        config <- getConfig
        files  <- liftIO $ listDirectory $ config_revisionsDir config
        return $ mapMaybe fileNameToCommit files

-- | Load package info from a commit that is already saved locally
load :: FilePath -> Commit -> IO (Either String Revision)
load dir commit = do
    packages <-
        catch (eitherDecodeFileStrict $ filePath dir RawNixVersions commit)
            (\(SomeException err) -> return $ Left $ show err)
    return $ Revision commit <$> packages

-- | Get JSON version information from nixos.org
downloadFromNix :: FilePath -> Commit -> IO (Either String FilePath)
downloadFromNix dir (Commit hash day) = do
    fileAlreadyThere <- fileExist path
    if fileAlreadyThere
       then return (return path)
       else do
           result <- downloadNixVersionsTo path
           unless (isRight result) (delete path)
           return result
    where
        path = filePath dir RawNixVersions (Commit hash day)

        delete = removeLink

        downloadNixVersionsTo = run . shell . command

        -- | download package versions as JSON and save
        -- them at destination
        command destination =
                "nix-env -qaP --json -f "
                <> revisionUrl hash
                <> " --arg config '" <> config <> "'"
                <> " >" <> destination

        -- | Configuration to make sure that all packages show up in the JSON
        config = mconcat
            [ "{"
               --Ensures no aliases are in the results.
            , "  allowAliases = false;"
            --  Enable recursion into attribute sets that nix-env normally
            --  doesn't look into so that we can get a more complete picture
            --  of the available packages for the purposes of the index.
            , "  packageOverrides = super: {"
            , "    haskellPackages = super.recurseIntoAttrs super.haskellPackages;"
            , "    rPackages = super.recurseIntoAttrs super.rPackages;"
            , "  };"
            , "}"
            ]
