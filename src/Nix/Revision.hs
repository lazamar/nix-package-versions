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
    , Revision(..)
    , Package(..)

    , commitToFileName
    , fileNameToCommit
    ) where

import Control.Exception (SomeException(..), catch, tryJust)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (unless, (<=<), join)
import Control.Monad.Except (runExceptT, liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, eitherDecodeFileStrict, parseJSON, withObject, (.:), (.:?), parseJSON)
import Data.Bifunctor (bimap)
import Data.Either (isRight)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Text (pack, unpack, Text)
import Data.Time.Calendar (Day, fromGregorian, toGregorian, showGregorian)
import Data.Foldable ( find)
import GHC.Generics (Generic)
import Nix.Versions.Types (CachePath(..), NixConfig(..), Config(..), Hash(..), Name, Version(..), Commit(..))
import System.Directory (listDirectory)
import System.Exit (ExitCode(..))
import System.Posix.Files (fileExist, removeLink)
import System.Process (readCreateProcessWithExitCode, shell, CreateProcess(..))
import Text.Read (readMaybe)

import qualified Network.HTTP.Req as Req

-- | Download lists of packages and their versions
-- for commits between from date and to date.
-- Downloads one package list for each month in period.
downloadVersionsInPeriod :: CachePath -> Day -> Day -> IO [Either (Day, String) (Commit, FilePath)]
downloadVersionsInPeriod dir from to = do
    commits <- mapMaybe eitherToMaybe <$> mapConcurrently (headAt dir gnixpkgs) dates
    mapConcurrently (download <=< announce) commits
    where
        (fromYear, _, _) = toGregorian from

        (toYear, _, _) = toGregorian to

        -- One entry per month
        dates
            = takeWhile (<= to)
            $ dropWhile (< from)
            $ [fromGregorian year month 1
              | year <- [fromYear .. toYear]
              , month <- [1..12]
              ]

        announce (Commit hash date) = do
            putStrLn $ "Downloading files for " <> showGregorian date
            return (Commit hash date)

        download commit@(Commit _ date) = do
            result <- downloadFromNix dir commit
            return $ bimap (date,) (commit,) result

------------------------


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

----------------------------

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
           result <- downloadNixVersionsTo path
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


----------------------------------------------------------------
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

newtype GitHubCommit = GitHubCommit
    { unGitHubCommit :: Commit
    }

instance FromJSON GitHubCommit where
    parseJSON = withObject "GitHubCommit " $ \v ->
        handle
        <$> (v .: "sha")
        <*> (v .: "commit" >>= (.: "committer") >>= (.: "date"))
       where
           handle sha date = GitHubCommit $ Commit (Hash sha) (readGregorian date)

           readGregorian :: String -> Day
           readGregorian = read . take 10

type Url = String

commitUrl :: GitHubRepo -> Commit -> Url
commitUrl (GitHubRepo user repo) (Commit (Hash hash) date)
    = unpack $ "https://github.com/" <> user <> "/" <> repo <> "/archive/" <> hash <> ".tar.gz"

-- | Fetch the commit that was the HEAD at 00 hours on the specified day
headAt :: CachePath -> GitHubRepo -> Day -> IO (Either String Commit)
headAt dir repo day = headCached >>= maybe headFromGithub (return . Right)
    where
        headCached = find ((day ==) . commitDate) <$> cachedRevisions dir

        commitDate (Commit _ date) = date

        headFromGithub = runExceptT $ do
            response <- catching  isHttpException
                $ Req.req Req.GET url Req.NoReqBody Req.jsonResponse options

            liftEither
                $ maybe (Left "Received empty commit list from remote") Right
                $ fmap unGitHubCommit
                $ listToMaybe
                $ Req.responseBody response

        options =
            Req.header "User-Agent" "lazamar"
            <>
            Req.queryParam "until" (Just $ pack $ showGregorian day <> "T00:00:00Z")

        url = Req.https "api.github.com"
            Req./: "repos"
            Req./: g_user repo
            Req./: g_repo repo
            Req./: "commits"

        isHttpException :: Req.HttpException -> Maybe String
        isHttpException = Just . show

        catching exc
            = join
            . liftIO
            . fmap liftEither
            . tryJust exc
            . Req.runReq Req.defaultHttpConfig


