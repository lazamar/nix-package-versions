{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

{-| This module defines the main types used to convey package version information
-}
module Nix.Versions.Types
    ( Name(..)
    , Version(..)
    , Hash(..)
    , Package(..)
    , Channel(..)
    , Repo(..)
    , Commit(..)
    ) where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.Monoid (Monoid)
import GHC.Generics (Generic)

data Channel
    = NixOS Version
    | UnstableNixOS
    | UnstableNixPkgs

--  | The name of package. e.g. nodejs
newtype Name = Name Text
    deriving (Show, Eq, Generic)
    deriving newtype (Hashable, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

-- | A package version. e.g. v8.10-rc2
newtype Version = Version Text
    deriving (Show, Eq, Generic)
    deriving newtype (Monoid, Semigroup, FromJSON, FromJSONKey, ToJSON, Hashable)

-- | A commit hash
newtype Hash = Hash Text
    deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | A package with its version information
data Package = Package
    { name :: Name
    -- path of file where the version was found.
    -- e.g. pkgs/development/web/nodejs/default.nix
    , nixpkgsPath :: FilePath
    -- A map from version number to NixPkgs commit
    -- that contains this package in that version.
    , versions :: HashMap Version Hash
    }

-- A local clone of a git repository
data Repo = Repo FilePath

data Commit = Commit Hash Day
    deriving (Show)
