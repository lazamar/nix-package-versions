{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}

{-| This module retrieves and parses information about Nix revisions.
   Revisions are available at https://nixos.org/nixos/packages
-}

module Nix.Revision
    ( downloadFromNix
    , nixpkgs
    , headAt
    , downloadVersionsInPeriod
    , load
    , PackagesJSON(..)
    , InfoJSON(..)
    ) where

import Control.Exception (Exception(..), SomeException(..), throw, catch)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (unless, (<=<))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (ToJSON, FromJSON, eitherDecodeFileStrict, parseJSON, withObject, (.:), (.:?), parseJSON)
import Data.Bifunctor (bimap, first)
import Data.Either (isRight)
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (pack, unpack, Text)
import Data.Time.Calendar (Day, fromGregorian, toGregorian, showGregorian)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Foldable (asum)
import Data.Typeable (Typeable, cast)
import GHC.Generics (Generic)
import Nix.Versions.Types (Channel(..), Hash(..), Name, Version(..), Commit(..), Repo(..))
import System.Exit (ExitCode(..))
import System.Posix.Files (fileExist, removeLink)
import System.Process (readCreateProcessWithExitCode, shell, CreateProcess(..))
import Text.Parsec (parse, spaces)
import Text.Read (readMaybe)

import qualified Nix.Versions.Parsers as Parsers


-- | Download lists of packages and their versions
-- for commits between from date and to date.
-- Downloads one package list for each month in period.
downloadVersionsInPeriod :: Day -> Day -> IO [Either (Day, String) (Commit, FilePath)]
downloadVersionsInPeriod from to = do
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
            result <- downloadFromNix commit
            return $ bimap (date,) (commit,) result

-- | Get JSON version information from nixos.org
downloadFromNix :: Commit -> IO (Either String FilePath)
downloadFromNix (Commit hash day) = do
    fileAlreadyThere <- fileExist path
    if fileAlreadyThere
       then return (return path)
       else do
           result <- downloadNixVersionsTo path
           unless (isRight result) (delete path)
           return result
    where
        path = filePath RawNixVersions (Commit hash day)

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

-------------------------------------------
-- Loading packages

-- | Load package info from a commit that is already saved locally
load :: Commit -> IO (Either String PackagesJSON)
load commit = do
    packages <-
        catch
            (eitherDecodeFileStrict $ filePath RawNixVersions commit)
            (\(SomeException err) -> return $ Left $ show err)
    return $ PackagesJSON commit <$> packages

data FileType
    = RawNixVersions
    | Preprocessed

nixpkgs :: Repo
nixpkgs = Repo "../nixpkgs"

-- Constants
c_DOWNLOADS_DIRECTORY = "./saved-versions"
--
-- | Get the place where a JSON file with package versions should be saved.
filePath :: FileType -> Commit -> FilePath
filePath fileType (Commit (Hash hash) day) = c_DOWNLOADS_DIRECTORY <> "/" <> name
    where
        name = showGregorian day <> "-" <> unpack hash <> "-" <> suffix fileType <> ".json"

        suffix RawNixVersions = "raw"
        suffix Preprocessed = "preprocessed"


type Url = String

revisionUrl :: Hash -> Url
revisionUrl (Hash hash) = "https://github.com/NixOS/nixpkgs/archive/" <> unpack hash <> ".tar.gz"

-- | The contents of a json file with package information
data PackagesJSON = PackagesJSON
    { commit :: Commit
    , packages :: HashMap Name InfoJSON
    } deriving (Generic)

data InfoJSON = InfoJSON
    { description :: Maybe Text
    , version :: Version
    , nixpkgsPath :: Maybe FilePath
    } deriving (Show, Generic)

instance FromJSON InfoJSON where
    parseJSON = withObject "InfoJSON" $ \v -> InfoJSON
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
