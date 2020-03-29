{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

{-| This module defines the main types used to convey package version information
-}
module Nix.Versions.Types
    ( Name(..)
    , Version(..)
    , Hash(..)
    , Channel(..)
    , Repo(..)
    , Commit(..)
    , NixConfig(..)
    , Config(..)
    , CachePath(..)
    , GitHubUser(..)
    , DBFile(..)
    ) where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.Monoid (Monoid)
import GHC.Generics (Generic)

data Config = Config
    { config_databaseFile   :: DBFile
    , config_cacheDirectory :: CachePath
    , config_gitHubUser     :: GitHubUser
    }

newtype DBFile = DBFile String

newtype GitHubUser = GitHubUser Text

newtype CachePath = CachePath FilePath

class NixConfig m where
    getConfig :: m Config

data Channel
    = NixOS Version
    | UnstableNixOS
    | UnstableNixPkgs

--  | The name of package. e.g. nodejs
newtype Name = Name { fromName :: Text }
    deriving (Show, Eq, Generic)
    deriving newtype (Hashable, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

-- | A package version. e.g. v8.10-rc2
newtype Version = Version { fromVersion :: Text }
    deriving (Show, Eq, Generic)
    deriving newtype (Monoid, Semigroup, FromJSON, FromJSONKey, ToJSON, ToJSONKey, Hashable)

-- | A commit hash
newtype Hash = Hash { fromHash :: Text }
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- A local clone of a git repository
data Repo = Repo FilePath

data Commit = Commit Hash Day
    deriving (Show, Eq)
