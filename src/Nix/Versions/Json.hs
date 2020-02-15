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

import Data.Aeson (FromJSON, eitherDecode, parseJSON, withObject, (.:), (.:?))
import Data.HashMap.Strict (HashMap)
import Nix.Versions.Types (Channel(..), Hash, Name, Version(..))
import GHC.Generics (Generic)
import Network.HTTP.Req (defaultHttpConfig, lbsResponse, runReq, req, GET(..), (/:), Url, Scheme(..)
                        , https, NoReqBody(..), responseBody)

-- | Get JSON version information from nixos.org
fetch :: Channel -> IO PackagesJSON
fetch channel = do
    response <- runReq defaultHttpConfig $ req GET (packageInfoUrl channel) NoReqBody lbsResponse mempty
    return $ either error id $ eitherDecode $  responseBody response


packageInfoUrl :: Channel -> Url Https
packageInfoUrl (NixOS (Version version)) = https "nixos.org" /: "nixpkgs" /: "packages-nixos-" <> version <> ".json.gz"
packageInfoUrl UnstableNixOS = https "nixos.org" /: "nixpkgs" /: "packages-nixos-unstable.json.gz"
packageInfoUrl UnstableNixPkgs = https "nixos.org" /: "nixpkgs" /: "packages-nixpkgs-unstable.json.gz"

-- | The contents of a json file with package information
data PackagesJSON = PackagesJSON
    { commit :: Hash
    , packages :: HashMap Name InfoJSON
    } deriving (Generic)

instance FromJSON PackagesJSON

data InfoJSON = InfoJSON
    { description :: Maybe String
    , version :: Version
    , nixpkgsPath :: Maybe FilePath
    } deriving (Show, Generic)

instance FromJSON InfoJSON where
    parseJSON = withObject "InfoJSON" $ \v -> InfoJSON
       <$> v .:? "description"
       <*> v .:  "version"
       <*> v .:? "location"



