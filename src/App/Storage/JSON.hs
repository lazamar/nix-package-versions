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
import Data.Maybe (fromMaybe)
import Data.Time.Calendar (Day)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import Nix.Versions.Types (Name)
import Data.Git (Commit(..))
import Nix.Revision (Package(..), Revision(..), Channel(..))
import App.Storage

data JSON = JSON
  { json_file :: FilePath
  , json_content :: MVar Content
  }

data Content = Content
  { c_revisions :: HashMap Channel (HashMap Commit (Day, RevisionState))
  , c_packages :: HashMap Channel (HashMap Name (HashMap Commit (Package, Day)))
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
      return $ Content mempty mempty
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


