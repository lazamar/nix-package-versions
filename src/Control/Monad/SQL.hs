{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.SQL where

{-
A monad to provide access to an SQL database handling nested transactions
and coordinating writes and reads from multiple threads.
-}

import Control.Concurrent.Classy.MVar (MVar, withMVar, readMVar, modifyMVar, newMVar, modifyMVar_, swapMVar)
import Control.Monad.Catch (MonadMask, bracket, mask, onException, bracket_)
import Control.Monad.Conc.Class (MonadConc)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT, local, ask, reader, runReaderT)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Database.SQLite.Simple (ToRow(toRow), FromRow(fromRow), SQLData(..), NamedParam((:=)), Query)
import Database.SQLite.Simple.FromField (FromField(..))
import Database.SQLite.Simple.ToField (ToField(..))
import UnliftIO (askUnliftIO, MonadUnliftIO, unliftIO)

import qualified Database.SQLite.Simple as SQL

-- | Access an SQL database
class MonadSQL m where
    execute  :: ToRow q => Query -> q -> m ()
    execute_ :: Query -> m ()
    query    :: (ToRow q, FromRow r) => Query -> q -> m [r]
    query_   :: (FromRow r) => Query -> m [r]
    withTransaction :: m a -> m a

-- | Pass through instance
instance {-# OVERLAPPABLE #-}
    ( MonadUnliftIO (t m)
    , MonadIO m
    , MonadTrans t
    , Monad m
    , MonadSQL m
    ) => MonadSQL (t m) where
    execute  q v = lift $ execute q v
    execute_ q   = lift $ execute_ q
    query    q v = lift $ query q v
    query_   q   = lift $ query_ q
    withTransaction a = do
        u <- askUnliftIO
        lift $ withTransaction $ liftIO $ unliftIO u a


runMonadSQL :: (MonadConc m, MonadIO m) => FilePath -> MonadSQLT m a -> m a
runMonadSQL path m = do
    conn <- liftIO $ SQL.open path
    writeLock <- newMVar True
    let state = DBState conn writeLock
    flip runReaderT state m

data DBState m = DBState
    { conn :: SQL.Connection
    , writeLock :: MVar m Bool -- ^ Is transaction still active
    }

instance (MonadConc m, MonadMask m, MonadIO m) => MonadSQL (ReaderT (DBState m) m) where
    execute  q v = write $ \conn -> liftIO $ SQL.execute conn q v
    execute_ q   = write $ \conn -> liftIO $ SQL.execute_ conn q
    query    q v = reader conn >>= \conn -> liftIO $ SQL.query conn q v
    query_   q   = reader conn >>= \conn -> liftIO $ SQL.query_ conn q
    withTransaction action =
        write $ \conn -> do
            transactionWriteLock <- newMVar True
            let closeTransaction = swapMVar transactionWriteLock False
            local (\s -> s { writeLock = transactionWriteLock }) $ runTransaction conn closeTransaction
        where
            runTransaction conn closeTransaction = do
                mask $ \restore -> do
                    begin
                    r <- restore (action <* closeTransaction) `onException` rollback
                    commit
                    return r
                where
                    begin    = liftIO $ SQL.execute_ conn "BEGIN TRANSACTION"
                    commit   = liftIO $ SQL.execute_ conn "COMMIT TRANSACTION"
                    rollback = liftIO $ SQL.execute_ conn "ROLLBACK TRANSACTION"

type MonadSQLT m a = ReaderT (DBState m) m a

write :: MonadConc m => (SQL.Connection -> MonadSQLT m a) -> MonadSQLT m a
write action = do
    DBState conn writeLock <- ask
    withMVar writeLock $ \case
        False -> error "Trying to write after transaction was closed"
        True  -> action conn
