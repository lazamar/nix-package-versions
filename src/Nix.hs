{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module Nix
  ( Package(..)
  , PackageWithVersion(..)
  , KeyName(..)
  , Version(..)
  , RevisionPackages
  , PackageDetails(..)
  , Channel(..)
  , Revision(..)
  , channelBranch
  , nixpkgsRepo
  , packagesAt
  ) where

import Control.Monad.Catch (SomeException(..), handle)
import Data.Aeson
  ( FromJSON
  , ToJSON
  , ToJSONKey
  , FromJSONKey
  , eitherDecodeFileStrict
  , parseJSON
  , withObject
  , (.:)
  , (.:?)
  , parseJSON )
import Data.Bifunctor (first)
import Data.Hashable (Hashable)
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMap
import Data.Text (Text, unpack, pack)
import GHC.Generics (Generic)
import Data.Git (Commit(..), Hash(..))
import qualified Data.Git as Git
import qualified GitHub
import Prettyprinter (Pretty(..))
import System.Exit (ExitCode(..))
import System.Process (readCreateProcessWithExitCode, shell, CreateProcess(..))
import System.IO.Temp (emptyTempFile, withSystemTempDirectory)

-- | The name of the key in the nixpkgs expression that identifies the package.
newtype KeyName = KeyName { fromKeyName :: Text }
    deriving (Show, Eq, Generic)
    deriving newtype (Hashable, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

-- | Usually name + version. Used to install a package with "nix-env -i"
newtype PackageWithVersion = PackageWithVersion { unPackageWithVersion :: Text }
    deriving (Show, Eq, Generic)
    deriving newtype (Hashable, FromJSON, ToJSON, Ord)

--  | The name of package. e.g. nodejs
newtype Package = Package { unPackage :: Text }
    deriving (Show, Eq, Generic)
    deriving newtype (Hashable, FromJSON, ToJSON, FromJSONKey, ToJSONKey)

-- | A package version. e.g. v8.10-rc2
newtype Version = Version { unVersion :: Text }
    deriving (Show, Eq, Generic)
    deriving newtype (Monoid, Semigroup, FromJSON, ToJSON, Hashable)

-- | A Nix distribution channel.
-- These are the channels we care about. There are many other channels that
-- are not worth keeping track of
data Channel
    = Nixpkgs_unstable
    | Nixpkgs_20_03_darwin
    | Nixpkgs_19_09_darwin
    | Nixpkgs_19_03_darwin
    | Nixpkgs_18_09_darwin
    | Nixpkgs_18_03_darwin
    | Nixpkgs_17_09_darwin
    | Nixos_unstable
    | Nixos_23_05
    | Nixos_22_11
    | Nixos_22_05
    | Nixos_21_11
    | Nixos_21_05
    | Nixos_20_09
    | Nixos_20_03
    | Nixos_19_09
    | Nixos_19_03
    | Nixos_18_09
    | Nixos_18_03
    | Nixos_17_09
    | Nixos_17_03
    deriving (Show, Read, Eq, Bounded, Enum, Ord, Generic)
    deriving anyclass (Hashable, FromJSON, FromJSONKey, ToJSON, ToJSONKey)

instance Pretty Channel where
  pretty channel = pretty branch
    where (Git.Branch branch) = channelBranch channel

channelBranch :: Channel -> Git.Branch
channelBranch = Git.Branch . \case
    Nixpkgs_unstable     -> "nixpkgs-unstable"
    Nixpkgs_20_03_darwin -> "nixpkgs-20.03-darwin"
    Nixpkgs_19_09_darwin -> "nixpkgs-19.09-darwin"
    Nixpkgs_19_03_darwin -> "nixpkgs-19.03-darwin"
    Nixpkgs_18_09_darwin -> "nixpkgs-18.09-darwin"
    Nixpkgs_18_03_darwin -> "nixpkgs-18.03-darwin"
    Nixpkgs_17_09_darwin -> "nixpkgs-17.09-darwin"
    Nixos_unstable       -> "nixos-unstable"
    Nixos_23_05          -> "nixos-23.05"
    Nixos_22_11          -> "nixos-22.11"
    Nixos_22_05          -> "nixos-22.05"
    Nixos_21_11          -> "nixos-21.11"
    Nixos_21_05          -> "nixos-21.05"
    Nixos_20_09          -> "nixos-20.09"
    Nixos_20_03          -> "nixos-20.03"
    Nixos_19_09          -> "nixos-19.09"
    Nixos_19_03          -> "nixos-19.03"
    Nixos_18_09          -> "nixos-18.09"
    Nixos_18_03          -> "nixos-18.03"
    Nixos_17_09          -> "nixos-17.09"
    Nixos_17_03          -> "nixos-17.03"

-- | The contents of a json file with package information
data Revision = Revision
    { r_channel  :: Channel
    , r_commit   :: Commit
    } deriving (Show, Generic, Ord, Eq)

type RevisionPackages = [PackageDetails]

-- | The information we have about a nix package in one revision
data PackageDetails = PackageDetails
    { name :: Package
    , version :: Version
    , keyName :: KeyName
    , fullName :: PackageWithVersion
    , description :: Maybe Text
    }
    deriving (Show, Generic, Eq)
    deriving anyclass (FromJSON, ToJSON)

nixpkgsRepo :: GitHub.Repository
nixpkgsRepo = GitHub.Repository
    { g_user = "NixOS"
    , g_repo = "nixpkgs"
    }

--------------------------------------------------------------

-- | An error trying to build a revision
data BuildError
    = BuildError Text
    | JsonDecodeError Text
    deriving (Show, Eq)

-- | This is expensive and takes long.
packagesAt :: Commit -> IO (Either BuildError RevisionPackages)
packagesAt commit@(Commit hash _) =
  withSystemTempDirectory "nix-package-versions" $ \dir -> do
  path <- emptyTempFile dir (unpack $ fromHash hash)
  mErr <- Nix.downloadTo path commit
  case mErr of
    Just err -> return $ Left $ BuildError $ pack err
    Nothing -> first (JsonDecodeError . pack) <$> Nix.loadFrom path

data RawPackage = RawPackage
    { _raw_name :: Package
    , _raw_version :: Version
    , _raw_fullName :: PackageWithVersion
    , _raw_description :: (Maybe Text)
    }

instance FromJSON RawPackage where
    parseJSON = withObject "RawPackage" $ \v -> RawPackage
       <$> (v .: "pname" <&> Package)
       <*> (v .: "version" <&> Version)
       <*> (v .: "name" <&> PackageWithVersion)
       <*> (v .:? "meta" >>= maybe (pure Nothing) (.:? "description"))

-- | Download info for a revision and build a list of all packages in it.
-- This can take a few minutes.
downloadTo :: FilePath -> Commit -> IO (Maybe String)
downloadTo filePath commit =
  fmap (either Just (const Nothing))
    $ run
    $ shell
    $ command
    $ filePath
  where
    -- | download package versions as JSON and save
    -- them at destination
    command destination =
            "nix-env -qaP --json -f "
            <> GitHub.archiveUrl nixpkgsRepo commit
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

    run :: CreateProcess -> IO (Either String String)
    run cmd = do
        (exitCode, stdOut, stdErr) <- readCreateProcessWithExitCode cmd ""
        return $ case exitCode of
          ExitSuccess   -> Right stdOut
          ExitFailure _ -> Left stdErr

-- | Load data from a json file created with downloadTo
loadFrom :: FilePath -> IO (Either String RevisionPackages)
loadFrom path =
  fmap (fmap toRevisionPackages)
  $ handle exceptionToEither
  $ eitherDecodeFileStrict path
  where
    exceptionToEither (SomeException err) = return $ Left $ show err

    toRevisionPackages :: HashMap KeyName RawPackage -> [PackageDetails]
    toRevisionPackages = map toPackage . HMap.toList

    toPackage :: (KeyName, RawPackage) -> PackageDetails
    toPackage (keyName, RawPackage name version fullName description) =
        PackageDetails
            name
            version
            keyName
            fullName
            description
