module App.Storage.JSON where

{-| JSON storage option. For ad-hoc testing only.
-}

import GHC.Generics
import Control.Concurrent.MVar (MVar, newMVar, readMVar, modifyMVar)
import Control.Exception (throwIO, finally, ErrorCall(..))
import qualified Data.Aeson as JSON
import Data.Aeson (FromJSON, ToJSON)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import Data.Time.Calendar (Day)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import Nix (Package, PackageDetails(..), Revision(..), Channel(..))
import Data.Git (Commit(..))
import Data.Time.Period (Period)
import App.Storage

data JSON = JSON
  { json_file :: FilePath
  , json_content :: MVar Content
  }

data Content = Content
  { c_revisions :: HashMap Channel (HashMap Commit (Day, CommitState))
  , c_packages :: HashMap Channel (HashMap Package (HashMap Commit (PackageDetails, Day)))

  , c_commits :: HashMap Commit CommitState
  , c_packages' :: HashMap Package (HashMap Commit PackageDetails)
  , c_coverage :: HashMap Channel (HashMap Period Commit)
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
      return $ Content mempty mempty mempty mempty mempty
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
  versions json channel name = overContents json $ \c@Content{..} ->
    let found = fromMaybe [] $ do
          pkgs <- HashMap.lookup channel c_packages
          vs <- HashMap.lookup name pkgs
          return
            [ (pkg, hash, day)
            | (Commit hash _, (pkg, day)) <- HashMap.toList vs
            ]
    in
    (c, found)

  revisions json channel = overContents json $ \c@Content{..} ->
    let found = fromMaybe [] $ do
          rs <- HashMap.lookup channel c_revisions
          return
            [ (day, Revision channel commit, state)
            | (commit, (day, state)) <- HashMap.toList rs
            ]
    in
    (c, found)

  writePackages json day (Revision channel commit) packages =
    overContents json $ \c@Content{..} ->
      let pkgs = fromMaybe mempty $ HashMap.lookup channel c_packages
          pkgs' = foldr add pkgs packages
          add pkg acc =
            let entry = HashMap.insert commit (pkg, day) mempty
                takeNewer one@(_, day1) two@(_, day2) =
                  if day1 > day2 then one else two
            in
            HashMap.insertWith (HashMap.unionWith takeNewer) (name pkg) entry acc

          new = c { c_packages = HashMap.insert channel pkgs' c_packages }
      in (new, ())

  writeRevisionState json day (Revision channel commit) state =
    overContents json $ \c@Content{..} ->
      let rs = fromMaybe mempty $ HashMap.lookup channel c_revisions
          revisions' = HashMap.insert commit (day, state) rs
          new = c { c_revisions = HashMap.insert channel revisions' c_revisions }
      in (new, ())


  versions' json channel pkg =
    overContents json $ \c@Content{..} ->
    let succeeded commit =
          maybe False (Success ==)
          $ HashMap.lookup commit c_commits
        channelCommits =
          HashSet.fromList
          $ filter succeeded
          $ HashMap.elems
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
            HashMap.toList $
            HashMap.lookupDefault mempty channel c_coverage
          , Just state <- [HashMap.lookup commit c_commits]
          ]
    in (c, results)

  writeCoverage json period channel commit =
    overContents json $ \c@Content{..} ->
    let entry = HashMap.insert period commit mempty
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


