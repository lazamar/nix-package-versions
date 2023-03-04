{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

{-| This module defines the main types used to convey package version information
-}
module Nix.Versions.Types
    ( Name(..)
    , FullName(..)
    , KeyName(..)
    , Version(..)
    , Hash(..)
    , Commit(..)
    , Config(..)
    , CachePath(..)
    , GitHubUser(..)
    , DBFile(..)
    , Task(..)
    ) where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)

data Config = Config
    { config_databaseFile   :: DBFile
    , config_cacheDirectory :: CachePath
    , config_gitHubUser     :: GitHubUser
    }
    deriving (Show)

newtype DBFile = DBFile String
    deriving (Show)

data GitHubUser = GitHubUser
    { g_username :: ByteString
    , g_authToken :: ByteString
    }
    deriving (Show)

newtype CachePath = CachePath FilePath
    deriving (Show)

-- | The name of the key in the nixpkgs expression that identifies the package.
newtype KeyName = KeyName { fromKeyName :: Text }
    deriving (Show, Eq, Generic)
    deriving newtype (Hashable, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

-- | Usually name + version. Used to install a package with "nix-env -i"
newtype FullName = FullName { fromFullName :: Text }
    deriving (Show, Eq, Generic)
    deriving newtype (Hashable, FromJSON, ToJSON)

--  | The name of package. e.g. nodejs
newtype Name = Name { fromName :: Text }
    deriving (Show, Eq, Generic)
    deriving newtype (Hashable, FromJSON, ToJSON)

-- | A package version. e.g. v8.10-rc2
newtype Version = Version { fromVersion :: Text }
    deriving (Show, Eq, Generic)
    deriving newtype (Monoid, Semigroup, FromJSON, ToJSON, Hashable)

-- | A commit hash
newtype Hash = Hash { fromHash :: Text }
    deriving (Eq, Show, Generic, FromJSON, ToJSON, Ord)

data Commit = Commit Hash Day
    deriving (Show, Eq, Ord)


-- | Asynchronous tasks
data Task
    = BuildNixRevision
    deriving (Show, Eq, Ord)

