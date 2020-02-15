{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

{-| This module is responsible handles downloading and parsing the package
   versioning information available at https://nixos.org/nixos/packages
-}

module Nix.Versions.Json
    ( fetch
    , PackagesJSON(..)
    , InfoJSON(..)
    ) where

import Data.Aeson (ToJSON, FromJSON, eitherDecodeFileStrict, parseJSON, withObject, (.:), (.:?), parseJSON)
import Data.HashMap.Strict (HashMap)
import Data.Text (unpack)
import Text.Parsec (parse)
import Nix.Versions.Types (Channel(..), Hash(..), Name, Version(..))
import GHC.Generics (Generic)
import System.Process (callCommand)

import System.Posix.Temp (mkdtemp)

import qualified Nix.Versions.Parsers as Parsers

-- | Get JSON version information from nixos.org
fetch :: Hash -> IO PackagesJSON
fetch hash = do
    -- Create temporary directory
    let dirPrefix = "nix-package-versions"
    tempDir <- mkdtemp dirPrefix

    -- Get packages json info and save it in the temporary file
    let tempFileAddress = tempDir <> "/nix-packages.json"
    callCommand (command tempFileAddress)

    packagesMap <- either error id <$> eitherDecodeFileStrict tempFileAddress
    return $ PackagesJSON hash packagesMap
        where

            command destination = "nix-env -qaP --json -f "
                    <> revisionUrl hash
                    <> " --arg config '" <> config <> "'"
                    <> " >" <> destination

            config = mconcat
                [ "{"
                   --Ensures no aliases are in the results.
                , "  allowAliases = false;"
                --  Enable recursion into attribute sets that nix-env normally doesn't look into
                --  so that we can get a more complete picture of the available packages for the
                --  purposes of the index.
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



