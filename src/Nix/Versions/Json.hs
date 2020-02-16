{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-| This module is responsible handles downloading and parsing the package
   versioning information available at https://nixos.org/nixos/packages
-}

module Nix.Versions.Json
    ( downloadFromNix
    , PackagesJSON(..)
    , InfoJSON(..)
    ) where

import Control.Exception (Exception(..), SomeException(..))
import Control.Monad (unless)
import Control.Monad.Catch (MonadThrow, throwM)
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
import Text.Parsec (parse)
import Text.Read (readMaybe)

import qualified Nix.Versions.Parsers as Parsers

data FileType
    = RawNixVersions
    | Preprocessed

-- | Get the place where a JSON file with package versions should be saved.
filePath :: FileType -> Hash -> Day -> FilePath
filePath fileType (Hash hash) day = downloadsDirectory <> "/" <> show day <> "-" <> unpack hash <> "-" <> suffix fileType <> ".json"
    where
        downloadsDirectory = "./saved-versions"

        suffix RawNixVersions = "raw"
        suffix Preprocessed = "preprocessed"

-- | Get JSON version information from nixos.org
downloadFromNix :: Hash -> IO FilePath
downloadFromNix hash = do
    day <- commitDay hash
    let file = filePath RawNixVersions hash day
    fileAlreadyThere <- fileExist file
    unless fileAlreadyThere (downloadNixVersionsTo file)
    return file
    where
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

nixpkgsLocalRepo = "../nixpkgs"

commitDay :: (MonadThrow m, MonadIO m) => Hash -> m Day
commitDay (Hash hash) = do
    output <- liftIO $ readCreateProcess ((shell command) { cwd = Just nixpkgsLocalRepo }) ""
    case readMaybe output of
        Just day -> return day
        Nothing  -> throwM $ UnableToParseCommitDate (Hash hash)
    where
        command = "git show -s --format=%cs " <> unpack hash

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

