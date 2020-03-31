{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-| This module retrieves and parses information about Nix revisions.
   Revisions are available at https://nixos.org/nixos/packages
-}

module Nix.Revision
    ( build
    , commitsSince
    , Revision(..)
    , Package(..)
    ) where

import Control.Exception (SomeException(..), bracket, handle, tryJust, onException)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (unless, (<=<), join)
import Control.Monad.Except (runExceptT, liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, eitherDecodeFileStrict, parseJSON, withObject, (.:), (.:?), parseJSON)
import Data.Bifunctor (bimap)
import Data.Either (isRight)
import Data.List (partition)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Text (pack, unpack, Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Calendar (Day, fromGregorian, toGregorian, showGregorian)
import Data.Foldable ( find)
import GHC.Generics (Generic)
import Nix.Versions.Types (GitHubUser(..), CachePath(..), NixConfig(..), Config(..), Hash(..), Name, Version(..), Commit(..))
import System.Directory (listDirectory)
import System.Exit (ExitCode(..))
import System.IO.Temp (emptySystemTempFile)
import System.Posix.Files (removeLink)
import System.Process (readCreateProcessWithExitCode, shell, CreateProcess(..))
import Text.Read (readMaybe)

import qualified Network.HTTP.Req as Req

-- | The contents of a json file with package information
data Revision = Revision
    { commit :: Commit
    , packages :: HashMap Name Package
    } deriving (Show, Generic)

-- | The information we have about a nix package in one revision
data Package = Package
    { description :: Maybe Text
    , version :: Version
    , nixpkgsPath :: Maybe FilePath
    } deriving (Show, Generic, Eq)

instance FromJSON Package where
    parseJSON = withObject "Package" $ \v -> Package
       <$> (v .: "meta" >>= (.:? "description"))
       <*>  v .:  "version"
       <*> (v .: "meta" >>= (.:? "position"))

-- | Download info for a revision and build a list of all
-- packages in it. This can take a few minutes.
build :: Commit -> IO (Either String Revision)
build commit =
    withTempFile $ \filePath -> do
        downloadNixVersionsTo filePath
        packages <- handle exceptionToEither $ eitherDecodeFileStrict filePath
        return $ Revision commit <$> packages
    where
        -- | Create a temporary file without holding a lock to it.
        withTempFile f = bracket (emptySystemTempFile "NIX_REVISION") removeLink f

        exceptionToEither (SomeException err) = return $ Left $ show err

        downloadNixVersionsTo = run . shell . command

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
-- GitHub

-- | Fetch a list of commits from Day onwards
-- Sorted oldest to newest
-- Verified commits appear earlier in the list
commitsSince :: GitHubUser -> Day -> IO (Either String [Commit])
commitsSince (GitHubUser guser) day = do
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

        options = Req.header "User-Agent" (encodeUtf8 guser)
               <> Req.queryParam "since" (Just $ showGregorian day <> "T00:00:00Z")
               <> Req.queryParam "sha" (Just ("nixpkgs-unstable" :: String))

        url = Req.https "api.github.com"
            Req./: "repos"
            Req./: g_user gnixpkgs
            Req./: g_repo gnixpkgs
            Req./: "commits"

        isHttpException :: Req.HttpException -> Maybe String
        isHttpException = Just . show

        catching exc
            = join
            . liftIO
            . fmap liftEither

gnixpkgs :: GitHubRepo
gnixpkgs = GitHubRepo
    { g_user = "NixOS"
    , g_repo = "nixpkgs"
    }

data GitHubRepo = GitHubRepo
    { g_user :: Text
    , g_repo :: Text
    }

data GitHubCommit = GitHubCommit
    { g_verified :: Bool
    , g_commit :: Commit
    }

instance FromJSON GitHubCommit where
    parseJSON = withObject "GitHubCommit " $ \v ->
        handle
        <$> (v .: "sha")
        <*> (v .: "commit" >>= (.: "committer") >>= (.: "date"))
        <*> (v .: "commit" >>= (.: "verification") >>= (.: "verified"))
       where
           handle sha date verified = GitHubCommit verified $ Commit (Hash sha) (readGregorian date)

           readGregorian :: String -> Day
           readGregorian = read . take 10

type Url = String

commitUrl :: GitHubRepo -> Commit -> Url
commitUrl (GitHubRepo user repo) (Commit (Hash hash) date)
    = unpack $ "https://github.com/" <> user <> "/" <> repo <> "/archive/" <> hash <> ".tar.gz"
