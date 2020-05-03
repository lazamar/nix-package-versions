{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.NixEnv where

{- A Monad to handle efficient download of RevisionPackages from Nixpkgs.

Each commit will be downloaded at most once.
-}

import Control.Concurrent.Classy.Async (async)
import Control.Concurrent.Classy.MVar (MVar(..), readMVar, modifyMVar)
import Control.Monad (when, void)
import Control.Monad.Conc.Class (MonadConc)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.MonadLimitedConc (MonadLimitedConc)
import Control.Monad.Trans.Class (lift, MonadTrans)
import Control.Monad.Trans.Except (runExceptT, ExceptT(..))
import Control.Monad.Trans.Reader (ReaderT, reader)
import Data.Bifunctor (first)
import Data.Map (Map)
import Data.Text (Text, pack, unpack)
import Nix.Revision (RevisionPackages, downloadNixVersionsTo, loadNixVersionsFrom)
import Nix.Versions.Types (Hash(..), Commit(..))
import System.IO.Temp (emptyTempFile)

import qualified Data.Map as Map

-- | An error trying to build a revision
data BuildError
    = BuildError Text
    | JsonDecodeError Text

data RevisionsState m = RevisionsState
    { s_storageDir :: FilePath
    -- | We keep only file paths in the cache because keeping the
    -- packages themselves would use too much heap
    , s_commits :: MVar m (Map Commit (MVar m (Either BuildError FilePath)))
    }

type RevisionsT m = ReaderT (RevisionsState m) m

class MonadRevisions m where
    packagesFor :: Commit -> m (Either BuildError RevisionPackages)

instance (MonadRevisions m, MonadTrans t, Monad m) => MonadRevisions (t m) where
    packagesFor  = lift . packagesFor

instance (MonadConc m, MonadIO m) => MonadRevisions (RevisionsT m) where
    packagesFor   commit = do
        mapVar <- reader s_commits
        (commitVar, isNew) <- modifyMVar mapVar $ \commitMap -> do
            commitVar <- maybe newEmptyMVar return $ Map.lookup commit commitMap
            return (Map.insert commit commitVar commitMap, (commitVar, Map.member commit commitMap))

        when isNew $ void $ async $ do
            path <- toFilePath commit
            mErr <- downloadNixVersionsTo path commit
            putMVar commitVar $ maybe (Right path) (Left . BuildError . pack) mErr

        runExceptT $ do
            path <- ExceptT $ readMVar commitVar
            ExceptT $ liftIO $ first (JsonDecodeError . pack) <$> loadNixVersionsFrom path

        where
            toFilePath (Commit (Hash hash) _) = do
                dir <- reader s_storageDir
                liftIO $ emptyTempFile dir (unpack hash)




