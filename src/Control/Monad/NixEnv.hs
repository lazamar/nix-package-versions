{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.NixEnv where

{- A Monad to handle efficient download of RevisionPackages from Nixpkgs.
-}

import Data.Bifunctor (first)
import Data.Text (Text, pack, unpack)
import Nix.Versions.Types (Hash(..), Commit(..))
import Nix.Revision (RevisionPackages, downloadNixVersionsTo, loadNixVersionsFrom)
import Data.Map (Map)
import Control.Monad (when, void)
import Control.Concurrent.Classy.MVar (MVar(..), readMVar, modifyMVar)
import Control.Concurrent.Classy.Async (async)
import Control.Monad.Conc.Class (MonadConc)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT, reader)
import Control.Monad.Trans.Class (lift, MonadTrans)
import Control.Monad.Trans.Except (runExcept,runExceptT, except, ExceptT(..))
import System.IO.Temp (emptyTempFile)

import qualified Data.Map as Map
import qualified Data.Aeson as Aeson

-- | An error trying to build a revision
data BuildError
    = BuildError Text
    | JsonDecodeError Text

data RevisionsState m = RevisionsState
    { s_storageDir :: FilePath
    , s_commits :: MVar m (Map Commit (MVar m (Either BuildError FilePath)))
    }

type RevisionsT m = ReaderT (RevisionsState m) m

class MonadRevisions m where
    withCommit :: Commit -> m (Either BuildError RevisionPackages)

instance (MonadConc m, MonadIO m) => MonadRevisions (RevisionsT m) where
     withCommit commit = do
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




