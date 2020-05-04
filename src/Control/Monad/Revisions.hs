{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Revisions
    ( RevisionsT
    , MonadRevisions
    , packagesFor
    , runMonadRevisions
    )
    where

{- A Monad to handle efficient download of RevisionPackages from Nixpkgs.

Each commit will be downloaded at most once.
-}

import Control.Concurrent.Classy.MVar (MVar(..), readMVar, modifyMVar, putMVar, newEmptyMVar, newMVar)
import Control.Monad (when, void)
import Control.Monad.Conc.Class (MonadConc)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.LimitedConc (MonadLimitedConc(..))
import Control.Monad.Trans.Class (lift, MonadTrans)
import Control.Monad.Trans.Except (runExceptT, ExceptT(..))
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Bifunctor (first)
import Data.Map (Map)
import Data.Text (Text, pack, unpack)
import Nix.Revision (RevisionPackages, downloadNixVersionsTo, loadNixVersionsFrom)
import Nix.Versions.Types (Hash(..), Commit(..), Task(..))
import System.IO.Temp (emptyTempFile, withSystemTempDirectory)
import UnliftIO (MonadUnliftIO)

import qualified Data.Map as Map

-- | A monad that takes care of fetching packages for a commit from Nix
-- at most once and limiting the amount of concurrent requests
class MonadRevisions m where
    packagesFor :: Commit -> m (Either BuildError RevisionPackages)

-- | Pass through instance
instance {-# OVERLAPPABLE #-} (MonadRevisions m, MonadTrans t, Monad m) => MonadRevisions (t m) where
    packagesFor  = lift . packagesFor

runMonadRevisions :: (MonadConc m, MonadIO m) => RevisionsT m a -> m a
runMonadRevisions r = do
    withSystemTempDirectory "NIX_VERSIONS" $ \dir -> do
        commitsVar <- newMVar mempty
        runReaderT r RevisionsState
            { s_storageDir = dir
            , s_commits = commitsVar
            }

-- | An error trying to build a revision
data BuildError
    = BuildError Text
    | JsonDecodeError Text
    deriving (Show, Eq)

data RevisionsState m = RevisionsState
    { s_storageDir :: FilePath
    -- | We keep only file paths in the cache because keeping the
    -- packages themselves would use too much heap
    , s_commits :: MVar m (Map Commit (MVar m (Either BuildError FilePath)))
    }

type RevisionsT m = ReaderT (RevisionsState m) m

instance (MonadUnliftIO m, MonadConc m, MonadIO m, MonadLimitedConc Task m) => MonadRevisions (RevisionsT m) where
    packagesFor commit = do
        RevisionsState{s_commits, s_storageDir} <- ask
        (commitVar, isNew) <- modifyMVar s_commits $ \commitMap -> do
            commitVar <- maybe newEmptyMVar return $ Map.lookup commit commitMap
            return (Map.insert commit commitVar commitMap, (commitVar, Map.member commit commitMap))

        when isNew $ void $ asyncTask BuildNixRevision $ do
            path <- liftIO $ emptyTempFile s_storageDir (unpack $ fromHash hash)
            mErr <- downloadNixVersionsTo path commit
            putMVar commitVar $ maybe (Right path) (Left . BuildError . pack) mErr

        runExceptT $ do
            path <- ExceptT $ readMVar commitVar
            ExceptT $ liftIO $ first (JsonDecodeError . pack) <$> loadNixVersionsFrom path
        where
            Commit hash _ = commit





