{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module Nix
  ( Name(..)
  , FullName(..)
  , KeyName(..)
  , Version(..)
  , Task(..)
  ) where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)

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
    deriving newtype (Hashable, FromJSON, ToJSON, FromJSONKey, ToJSONKey)

-- | A package version. e.g. v8.10-rc2
newtype Version = Version { fromVersion :: Text }
    deriving (Show, Eq, Generic)
    deriving newtype (Monoid, Semigroup, FromJSON, ToJSON, Hashable)

-- | Asynchronous tasks
data Task
    = BuildNixRevision
    deriving (Show, Eq, Ord)

