module App.Storage.JSON where

{-| JSON storage option. For ad-hoc testing only.
-}

import GHC.Generics
import Control.Concurrent.MVar (MVar, newMVar, readMVar, modifyMVar)
import Control.Exception (throwIO, finally, ErrorCall(..))
import qualified Data.Aeson as JSON
import Data.Aeson (FromJSON, ToJSON)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import Nix (Package, PackageDetails(..), Channel(..))
import Data.Git (Commit(..))
import Data.Time.Period (Period)
import App.Storage

data JSON = JSON
  { json_file :: FilePath
  , json_content :: MVar Content
  }

data Content = Content
  { c_commits :: HashMap Commit CommitState
  , c_packages' :: HashMap Package (HashMap Commit PackageDetails)
  , c_coverage :: HashMap Channel (HashSet (Period, Commit))
  }
  deriving (Generic)

instance FromJSON Content
instance ToJSON Content

withDatabase :: FilePath -> (Database -> IO a) -> IO a
withDatabase path act = do
  exists <- doesFileExist path
  content <-
    if not exists
    then do
      createDirectoryIfMissing True (takeDirectory path)
      return $ Content mempty mempty mempty
    else do
      decoded <- JSON.eitherDecodeFileStrict path
      case decoded of
        Left err -> throwIO $ ErrorCall err
        Right c -> return c

  var <- newMVar content
  let commit = do
        c <- readMVar var
        JSON.encodeFile path c
  act (Database $ JSON path var) `finally` commit

overContents :: JSON -> (Content -> (Content, a)) -> IO a
overContents (JSON _ var) act =
  modifyMVar var $ \contents -> return (act contents)

instance Storage JSON where
  versions json channel pkg =
    overContents json $ \c@Content{..} ->
    let succeeded commit =
          maybe False (Success ==)
          $ HashMap.lookup commit c_commits
        channelCommits =
          HashSet.fromList
          $ filter succeeded
          $ map snd
          $ HashSet.toList
          $ HashMap.lookupDefault mempty channel c_coverage
        details = HashMap.toList $ HashMap.lookupDefault mempty pkg c_packages'
        results =
          [ (det, commit)
          | (commit, det) <- details
          , commit `HashSet.member` channelCommits
          ]
    in (c, results)

  coverage json channel =
    overContents json $ \c@Content{..} ->
    let results =
          [ (period, commit, state)
          | (period, commit) <-
            HashSet.toList $
            HashMap.lookupDefault mempty channel c_coverage
          , Just state <- [HashMap.lookup commit c_commits]
          ]
    in (c, results)

  writeCoverage json period channel commit =
    overContents json $ \c@Content{..} ->
    let entry = HashSet.singleton (period, commit)
        new = c { c_coverage = HashMap.insertWith (<>) channel entry c_coverage }
    in (new, ())

  writePackage json commit details =
    overContents json $ \c@Content{..} ->
    let entry = HashMap.insert commit details mempty
        new = c
          { c_packages' =
            HashMap.insertWith (<>) (name details) entry c_packages'
          }
    in (new, ())

  writeCommitState json commit state =
    overContents json $ \c@Content{..} ->
    let new = c { c_commits = HashMap.insert commit state c_commits }
    in (new, ())


