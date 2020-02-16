{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-| This module is responsible handles downloading and parsing the package
   versioning information available at https://nixos.org/nixos/packages
-}

module Nix.Versions.Json
    ( downloadFromNix
    , nixpkgs
    , headAt
    , PackagesJSON(..)
    , InfoJSON(..)
    , Commit(..)
    ) where

import Control.Exception (Exception(..), SomeException(..), throw)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (ToJSON, FromJSON, eitherDecodeFileStrict, parseJSON, withObject, (.:), (.:?), parseJSON)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)
import Data.Text (pack, unpack, Text)
import Data.Time.Calendar (Day)
import Data.Typeable (Typeable, cast)
import GHC.Generics (Generic)
import Nix.Versions.Types (Channel(..), Hash(..), Name, Version(..))
import System.Posix.Files (fileExist)
import System.Process (callCommand, readCreateProcess, shell, CreateProcess(..))
import Text.Parsec (parse, spaces)
import Text.Read (readMaybe)

import qualified Nix.Versions.Parsers as Parsers

data FileType
    = RawNixVersions
    | Preprocessed

nixpkgs :: Repo
nixpkgs = Repo "../nixpkgs"

-- Constants
c_DOWNLOADS_DIRECTORY = "./saved-versions"
--
-- | Get the place where a JSON file with package versions should be saved.
fileName :: FileType -> Hash -> Day -> String
fileName fileType (Hash hash) day = show day <> "-" <> unpack hash <> "-" <> suffix fileType <> ".json"
    where
        suffix RawNixVersions = "raw"
        suffix Preprocessed = "preprocessed"

-- | Get JSON version information from nixos.org
downloadFromNix :: Commit -> IO FilePath
downloadFromNix (Commit hash day) = do
    fileAlreadyThere <- fileExist path
    unless fileAlreadyThere (downloadNixVersionsTo path)
    return path
    where
        path = c_DOWNLOADS_DIRECTORY <> "/" <> fileName RawNixVersions hash day

        downloadNixVersionsTo = callCommand . command

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



type Url = String

revisionUrl :: Hash -> Url
revisionUrl (Hash hash) = "https://github.com/NixOS/nixpkgs/archive/" <> unpack hash <> ".tar.gz"

-- | The contents of a json file with package information
data PackagesJSON = PackagesJSON
    { commit :: Hash
    , packages :: HashMap Name InfoJSON
    } deriving (Generic)

instance FromJSON PackagesJSON
instance ToJSON PackagesJSON

data InfoJSON = InfoJSON
    { description :: Maybe String
    , version :: Version
    , nixpkgsPath :: Maybe FilePath
    } deriving (Show, Generic)

instance ToJSON InfoJSON
instance FromJSON InfoJSON where
    parseJSON = withObject "InfoJSON" $ \v -> InfoJSON
       <$> (v .: "meta" >>= (.:? "description"))
       <*>  v .:  "version"
       <*> (fmap removeLineNumber <$> (v .: "meta" >>= (.:? "position")))
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
-- Commits

-- A local clone of a git repository
data Repo = Repo FilePath

data Commit = Commit Hash Day
    deriving (Show)

-- | Returns the commit that was the head on that date
headAt :: Repo -> Day -> IO (Maybe Commit)
headAt repo day = do
    output <- runInRepo repo $ "git log --format='%cs %H' --max-count=1 --until " <> show day
    return $ either (const Nothing) Just $ parse commitParser "Commit" output
    where
        -- Parses something that looks like this
        --  2015-02-12 a43db5fa2025c998ce0d72dc7dd425152d26ad59
        commitParser = do
            day <- Parsers.day
            spaces
            hash <- Parsers.hash
            return $ Commit hash day

commitDay :: Hash -> IO Day
commitDay (Hash hash) = do
    output <- liftIO $ runInRepo nixpkgs command
    case readMaybe output of
        Just day -> return day
        Nothing  -> throw $ UnableToParseCommitDate (Hash hash)
    where
        command = "git show -s --format=%cs " <> unpack hash

runInRepo :: Repo -> String -> IO String
runInRepo (Repo path) command =
    readCreateProcess ((shell command) { cwd = Just path }) ""
