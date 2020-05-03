{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Control.Monad.NixEnv where

{- A Monad to handle efficient download of RevisionPackages from Nixpkgs.
-}

import Data.Bifunctor (first)
import Data.Text (Text, pack)
import Data.Aeson (eitherDecodeFileStrict')
import Nix.Versions.Types (Hash(..), Commit(..))
import Nix.Revision (Revision(..), Channel(..), RevisionPackages)
import Data.Map (Map)
import Control.Concurrent.Classy.MVar (MVar(..), readMVar, modifyMVar, modifyMVar_)
import Control.Monad.Conc.Class (MonadConc)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT, reader)
import Control.Monad.Trans.Class (lift, MonadTrans)
import Control.Monad.Trans.Except (runExcept,runExceptT, except, ExceptT(..))

import qualified Data.Map as Map

-- | An effectful memoised key-value store
class MonadCache k v m | m -> k, m -> v where
    get :: k -> m v
    set :: k -> v -> m ()

instance (MonadCache k v m, MonadTrans t, Monad m) => MonadCache k v (t m) where
    get k   = lift $ get k
    set k v = lift $ set k v


instance (MonadIO m, MonadConc m) =>
    MonadCache Commit (Either BuildError FilePath) (ReaderT (RevisionsState m) m) where
    get commit = do
        mapVar <- reader s_commits
        commitVar <- modifyMVar mapVar $ \commitMap -> do
            commitVar <- maybe newEmptyMVar return $ Map.lookup commit commitMap
            return (Map.insert commit commitVar commitMap, commitVar)
        readMVar commitVar

    set commit ePath = do
        commitsVar <- reader s_commits
        modifyMVar_ commitsVar $ \commitMap -> do
            cVar <- maybe newEmptyMVar return $ Map.lookup commit commitMap
            putMVar cVar ePath
            return $ Map.insert commit cVar commitMap


-- | An error trying to build a revision
data BuildError
    = BuildError Text
    | JsonDecodeError Text

type MVarMap m k v = MVar m (Map k (MVar m v))

data RevisionsState m = RevisionsState
    { s_storageDir :: FilePath
    , s_commits :: MVar m (Map Commit (MVar m (Either BuildError FilePath)))
    }


class MonadRevisions m where
    withCommit :: Commit -> m (Either BuildError RevisionPackages)

instance (MonadConc m, MonadIO m, MonadCache Commit (Either BuildError FilePath) m) =>
    MonadRevisions (ReaderT FilePath m) where
    withCommit :: Commit -> ReaderT FilePath m (Either BuildError RevisionPackages)
    withCommit c = runExceptT $ do
        path <- ExceptT $ get c
        ExceptT $ liftIO $ first (JsonDecodeError . pack) <$> eitherDecodeFileStrict' path


f :: FilePath -> RevisionPackages
f = undefined




