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
    ( build
    , downloadNixVersionsTo
    , loadNixVersionsFrom
    , revisionsOn
    , channelBranch
    , Revision(..)
    , RevisionPackages
    , Package(..)
    , Channel(..)
    , GitBranch(..)
    ) where

import Control.Monad.Catch (SomeException(..), bracket, handle, tryJust, MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Log (MonadLog, WithSeverity, logDebug)
import Data.Aeson (FromJSON, eitherDecodeFileStrict, parseJSON, withObject, (.:), (.:?), parseJSON)
import Data.Functor ((<&>))
import Data.List (partition)
import Data.HashMap.Strict (HashMap)
import Data.Text (unpack, Text)
import Data.Time.Calendar (Day, showGregorian)
import GHC.Generics (Generic)
import Nix.Versions.Types (GitHubUser(..), Hash(..), Name(..), Version(..), Commit(..))
import System.Exit (ExitCode(..))
import System.IO.Temp (emptySystemTempFile)
import System.Posix.Files (removeLink)
import System.Process (readCreateProcessWithExitCode, shell, CreateProcess(..))

import qualified Network.HTTP.Req as Req

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
    | Nixos_20_03
    | Nixos_19_09
    | Nixos_19_03
    | Nixos_18_09
    | Nixos_18_03
    | Nixos_17_09
    | Nixos_17_03
    | Nixos_16_09
    | Nixos_16_03
    | Nixos_15_09
    | Nixos_14_12
    | Nixos_14_04
    | Nixos_13_10
    deriving (Show, Read, Eq, Bounded, Enum, Ord)

-- | The contents of a json file with package information
data Revision = Revision
    { channel  :: Channel
    , commit   :: Commit
    } deriving (Show, Generic, Ord, Eq)

type RevisionPackages = HashMap Name Package

-- | The information we have about a nix package in one revision
data Package = Package
    { name :: Name
    , description :: Maybe Text
    , version :: Version
    , nixpkgsPath :: Maybe FilePath
    } deriving (Show, Generic, Eq)

instance FromJSON Package where
    parseJSON = withObject "Package" $ \v -> Package
       <$> (v .: "pname" <&> Name)
       <*> (v .: "meta" >>= (.:? "description"))
       <*>  v .:  "version"
       <*> (v .: "meta" >>= (.:? "position"))

-- TODO REMOVE THIS
build :: (MonadMask m, MonadIO m, MonadLog (WithSeverity String) m)
    => Revision -> m (Either String RevisionPackages)
build (Revision _ commit) =
    withTempFile $ \path -> do
        mErr <- downloadNixVersionsTo path commit
        case mErr of
            Just err -> return $ Left err
            Nothing -> loadNixVersionsFrom path
    where
        -- | Create a temporary file without holding a lock to it.
        withTempFile f = bracket
            (liftIO $ emptySystemTempFile "NIX_REVISION")
            (liftIO . removeLink)
            f

loadNixVersionsFrom :: MonadIO m => FilePath -> m (Either String RevisionPackages)
loadNixVersionsFrom path = liftIO $ handle exceptionToEither $ eitherDecodeFileStrict path
    where
        exceptionToEither (SomeException err) = return $ Left $ show err

-- | Download info for a revision and build a list of all
-- packages in it. This can take a few minutes.
downloadNixVersionsTo
    :: (MonadIO m, MonadLog (WithSeverity String) m)
    => FilePath -> Commit -> m (Maybe String)
downloadNixVersionsTo filePath commit
    = do
        logDebug $ unwords ["Downloading Nix version for", show commit, "into", filePath]
        res <- liftIO
            $ fmap (either Just (const Nothing))
            $ run
            $ shell
            $ command
            $ filePath
        case res of
            Just _  -> logDebug $ unwords ["Download successful for", show commit, "into", filePath]
            Nothing -> logDebug $ unwords ["Download failed for", show commit, "into", filePath]
        return res

    where
        -- | download package versions as JSON and save
        -- them at destination
        command destination =
                "nix-env -qaP --json -f "
                <> commitUrl gnixpkgs commit
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
revisionsOn :: MonadIO m => GitHubUser -> Channel -> Day -> m (Either String [Revision])
revisionsOn guser channel day
    = liftIO
    $ fmap (fmap $ fmap $ Revision channel)
    $ commitsUntil guser gnixpkgs (channelBranch channel) day

gnixpkgs :: GitHubRepo
gnixpkgs = GitHubRepo
    { g_user = "NixOS"
    , g_repo = "nixpkgs-channels"
    }

channelBranch :: Channel -> GitBranch
channelBranch = GitBranch . \case
    Nixpkgs_unstable     -> "nixpkgs-unstable"
    Nixpkgs_20_03_darwin -> "nixpkgs-20.03-darwin"
    Nixpkgs_19_09_darwin -> "nixpkgs-19.09-darwin"
    Nixpkgs_19_03_darwin -> "nixpkgs-19.03-darwin"
    Nixpkgs_18_09_darwin -> "nixpkgs-18.09-darwin"
    Nixpkgs_18_03_darwin -> "nixpkgs-18.03-darwin"
    Nixpkgs_17_09_darwin -> "nixpkgs-17.09-darwin"
    Nixos_unstable       -> "nixos-unstable"
    Nixos_20_03          -> "nixos-20.03"
    Nixos_19_09          -> "nixos-19.09"
    Nixos_19_03          -> "nixos-19.03"
    Nixos_18_09          -> "nixos-18.09"
    Nixos_18_03          -> "nixos-18.03"
    Nixos_17_09          -> "nixos-17.09"
    Nixos_17_03          -> "nixos-17.03"
    Nixos_16_09          -> "nixos-16.09"
    Nixos_16_03          -> "nixos-16.03"
    Nixos_15_09          -> "nixos-15.09"
    Nixos_14_12          -> "nixos-14.12"
    Nixos_14_04          -> "nixos-14.04"
    Nixos_13_10          -> "nixos-13.10"

-------------------------------------------------------------------------------
-- GitHub

-- | Fetch a list of commits until end of Day
-- Sorted oldest to newest
-- Verified commits appear earlier in the list
commitsUntil :: GitHubUser -> GitHubRepo -> GitBranch -> Day -> IO (Either String [Commit])
commitsUntil (GitHubUser guser gtoken) grepo (GitBranch branch) day = do
    response <-
        tryJust isHttpException
        $ fmap Req.responseBody
        $ Req.runReq Req.defaultHttpConfig
        $ Req.req Req.GET url Req.NoReqBody Req.jsonResponse options

    return $ either Left (Right . fmap g_commit . rearrange) response
    where
        -- | Order from oldest to newest and move verified commits to the top
        -- so that they may be used first.
        -- Some commits don't build successfully, the prioritisation of
        -- verified commits tries to mitigate that problem.
        rearrange :: [GitHubCommit] -> [GitHubCommit]
        rearrange = ((++) <$> fst <*> snd) . partition g_verified . reverse

        options = Req.header "User-Agent" guser
               <> Req.queryParam "until" (Just $ showGregorian day <> "T23:59:59Z")
               <> Req.queryParam "sha" (Just branch)
               <> Req.basicAuth guser gtoken

        url = Req.https "api.github.com"
            Req./: "repos"
            Req./: g_user grepo
            Req./: g_repo grepo
            Req./: "commits"

        isHttpException :: Req.HttpException -> Maybe String
        isHttpException = Just . show

data GitHubRepo = GitHubRepo
    { g_user :: Text
    , g_repo :: Text
    }

data GitHubCommit = GitHubCommit
    { g_verified :: Bool
    , g_commit :: Commit
    }

newtype GitBranch = GitBranch { fromGitBranch :: Text }

instance FromJSON GitHubCommit where
    parseJSON = withObject "GitHubCommit " $ \v ->
        construct
        <$> (v .: "sha")
        <*> (v .: "commit" >>= (.: "committer") >>= (.: "date"))
        <*> (v .: "commit" >>= (.: "verification") >>= (.: "verified"))
       where
           construct sha date verified = GitHubCommit verified $ Commit (Hash sha) (readGregorian date)

           readGregorian :: String -> Day
           readGregorian = read . take 10

type Url = String

commitUrl :: GitHubRepo -> Commit -> Url
commitUrl (GitHubRepo user repo) (Commit (Hash hash) _)
    = unpack $ "https://github.com/" <> user <> "/" <> repo <> "/archive/" <> hash <> ".tar.gz"
