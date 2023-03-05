{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}

{-| This module retrieves and parses information about Nix revisions.
   Revisions are available at https://nixos.org/nixos/packages
-}

module Nix.Revision
    ( downloadTo
    , loadFrom
    , revisionsOn
    , channelBranch
    , Revision(..)
    , RevisionPackages
    , Package(..)
    , Channel(..)
    ) where

import Control.Monad.Catch (SomeException(..), handle)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Log (MonadLog, WithSeverity, logDebug)
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
import Data.Functor ((<&>))
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Time.Calendar (Day)
import GHC.Generics (Generic)
import System.Exit (ExitCode(..))
import System.Process (readCreateProcessWithExitCode, shell, CreateProcess(..))

import Data.Git (Commit(..), Branch(..))
import GitHub (AuthenticatingUser(..), Repository(..), archiveUrl, commitsUntil)
import Nix (KeyName(..), FullName(..), Name(..), Version(..))

import qualified Data.HashMap.Strict as HMap

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

-- | The contents of a json file with package information
data Revision = Revision
    { channel  :: Channel
    , commit   :: Commit
    } deriving (Show, Generic, Ord, Eq)

type RevisionPackages = [Package]

-- | The information we have about a nix package in one revision
data Package = Package
    { name :: Name
    , version :: Version
    , keyName :: KeyName
    , fullName :: FullName
    , description :: Maybe Text
    } deriving (Show, Generic, Eq)

instance FromJSON Package
instance ToJSON Package

data RawPackage = RawPackage
    { _raw_name :: Name
    , _raw_version :: Version
    , _raw_fullName :: FullName
    , _raw_description :: (Maybe Text)
    }

instance FromJSON RawPackage where
    parseJSON = withObject "RawPackage" $ \v -> RawPackage
       <$> (v .: "pname" <&> Name)
       <*> (v .: "version" <&> Version)
       <*> (v .: "name" <&> FullName)
       <*> (v .:? "meta" >>= maybe (pure Nothing) (.:? "description"))

-- | Load data from a json file created with downloadTo
loadFrom :: MonadIO m => FilePath -> m (Either String RevisionPackages)
loadFrom path = liftIO
    $ fmap (fmap toRevisionPackages)
    $ handle exceptionToEither
    $ eitherDecodeFileStrict path
    where
        exceptionToEither (SomeException err) = return $ Left $ show err

        toRevisionPackages :: HashMap KeyName RawPackage -> [Package]
        toRevisionPackages = map toPackage . HMap.toList

        toPackage :: (KeyName, RawPackage) -> Package
        toPackage (keyName, RawPackage name version fullName description) =
            Package
                name
                version
                keyName
                fullName
                description


-- | Download info for a revision and build a list of all
-- packages in it. This can take a few minutes.
downloadTo
    :: (MonadIO m, MonadLog (WithSeverity String) m)
    => FilePath -> Commit -> m (Maybe String)
downloadTo filePath commit
    = do
        logDebug $ unwords ["Downloading Nix version for", show commit, "into", filePath]
        res <- liftIO
            $ fmap (either Just (const Nothing))
            $ run
            $ shell
            $ command
            $ filePath
        case res of
            Nothing  -> logDebug $ unwords ["Download successful for", show commit, "into", filePath]
            Just err -> logDebug $ unwords ["Download failed for", show commit, "into", filePath, err]
        return res

    where
        -- | download package versions as JSON and save
        -- them at destination
        command destination =
                "nix-env -qaP --json -f "
                <> archiveUrl gnixpkgs commit
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

-------------------------------------------------------------------------------
-- GitHub + Nix

-- | Last revisions registered for given day
revisionsOn :: MonadIO m => AuthenticatingUser -> Int -> Channel -> Day -> m (Either String [Revision])
revisionsOn guser count channel day
    = liftIO
    $ fmap (fmap $ fmap $ Revision channel)
    $ commitsUntil guser count gnixpkgs (channelBranch channel) day

gnixpkgs :: Repository
gnixpkgs = Repository
    { g_user = "NixOS"
    , g_repo = "nixpkgs"
    }

channelBranch :: Channel -> Branch
channelBranch = Branch . \case
    Nixpkgs_unstable     -> "nixpkgs-unstable"
    Nixpkgs_20_03_darwin -> "nixpkgs-20.03-darwin"
    Nixpkgs_19_09_darwin -> "nixpkgs-19.09-darwin"
    Nixpkgs_19_03_darwin -> "nixpkgs-19.03-darwin"
    Nixpkgs_18_09_darwin -> "nixpkgs-18.09-darwin"
    Nixpkgs_18_03_darwin -> "nixpkgs-18.03-darwin"
    Nixpkgs_17_09_darwin -> "nixpkgs-17.09-darwin"
    Nixos_unstable       -> "nixos-unstable"
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
