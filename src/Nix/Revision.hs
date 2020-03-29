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
    ( load
    , commitsAt
    , Revision(..)
    , Package(..)

    , commitToFileName
    , fileNameToCommit
    ) where

import Control.Exception (SomeException(..), catch, tryJust, onException)
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
import System.Posix.Files (fileExist, removeLink)
import System.Process (readCreateProcessWithExitCode, shell, CreateProcess(..))
import Text.Read (readMaybe)

import qualified Network.HTTP.Req as Req

-- | The contents of a json file with package information
data Revision = Revision
    { commit :: Commit
    , packages :: HashMap Name Package
    } deriving (Generic)

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

-------------------------------------------------------------------------------
-- Saving and loading revision files

load :: CachePath -> Commit -> IO (Either String Revision)
load dir commit = do
    cached <- loadCached dir commit
    case cached of
      Just val -> return $ Right val
      Nothing  -> loadFromNixpkgs dir commit

-- | List of revision commits saved locally
cachedRevisions :: CachePath -> IO [Commit]
cachedRevisions (CachePath dir) = do
    files  <- liftIO $ listDirectory dir
    return $ mapMaybe fileNameToCommit files

-- | Load a revision saved locally
loadCached :: CachePath -> Commit -> IO (Maybe Revision)
loadCached dir commit = do
    cached <- cachedRevisions dir
    if not (elem commit cached)
       then return Nothing
       else do
            packages <- catch
                    (eitherDecodeFileStrict $ filePath dir commit)
                    (\(SomeException err) -> return $ Left $ show err)
            return $ eitherToMaybe $ Revision commit <$> packages

loadFromNixpkgs :: CachePath -> Commit -> IO (Either String Revision)
loadFromNixpkgs dir commit = do
    res <- downloadFromNix dir commit
    case res of
        Left err -> return $ Left err
        Right _  -> load dir commit

-- | Get JSON version information from nixos.org
downloadFromNix :: CachePath -> Commit -> IO (Either String FilePath)
downloadFromNix dir commit = do
    fileAlreadyThere <- fileExist path
    if fileAlreadyThere
       then return (return path)
       else do
           result <- downloadNixVersionsTo path `onException` delete path
           unless (isRight result) (delete path)
           return result
    where
        path = filePath dir commit

        delete = removeLink

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

-- | Get the place where a JSON file with package versions should be saved.
filePath :: CachePath -> Commit -> FilePath
filePath (CachePath dir) commit = dir <> "/" <> commitToFileName commit

ext :: String
ext = ".json"

commitToFileName :: Commit -> String
commitToFileName  (Commit (Hash hash) date) =
    showGregorian date <> "-" <> unpack hash <> ext

fileNameToCommit :: String -> Maybe (Commit)
fileNameToCommit  fname = Commit hash <$> readMaybe (take 10 fname)
    where
        hash = Hash $ pack $ dropTail (length ext) $ drop 11 fname

        dropTail n s = reverse $ drop n $ reverse s


eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just


-------------------------------------------------------------------------------
-- GitHub

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

-- | Fetch the the last commits at 00 hours on the specified day
-- Verified commits appear earlier in the list
commitsAt :: CachePath -> GitHubUser -> Day -> IO (Either String [Commit])
commitsAt dir (GitHubUser guser) day = headCached >>= maybe headFromGithub (return . Right)
    where
        headCached = (fmap pure . find ((day ==) . commitDate)) <$> cachedRevisions dir

        commitDate (Commit _ date) = date

        headFromGithub = do
            response <-
                tryJust isHttpException
                $ fmap Req.responseBody
                $ Req.runReq Req.defaultHttpConfig
                $ Req.req Req.GET url Req.NoReqBody Req.jsonResponse options

            return $ either Left (Right . fmap g_commit . rearrange) response

        -- | Move verified commits to the top, so that they may
        -- be used first.
        -- Some commits don't build successfully, the prioritisation of
        -- verified commits tries to mitigate that problem.
        rearrange :: [GitHubCommit] -> [GitHubCommit]
        rearrange = ((++) <$> fst <*> snd) . partition g_verified

        options =
            Req.header "User-Agent" (encodeUtf8 guser)
            <>
            Req.queryParam "until" (Just $ showGregorian day <> "T00:00:00Z")

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


