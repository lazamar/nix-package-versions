{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.SQL
  ( MonadSQL(..)
  , Connection
  , connect
  , runSQL
  )
  where

{- | A monad to provide access to an SQL database handling nested transactions
and coordinating writes and reads from multiple threads.
-}

import Control.Concurrent.MVar (MVar, withMVar, newMVar, swapMVar)
import Control.Monad.Catch (mask, onException, finally)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT(..), local, ask, reader, runReaderT)
import Database.SQLite.Simple (ToRow, FromRow, NamedParam, Query)
import System.FilePath.Posix (takeDirectory)
import System.Directory (createDirectoryIfMissing)

import qualified Database.SQLite.Simple as SQL

newtype Connection = Connection DBState

connect :: FilePath -> (Connection -> IO a) -> IO a
connect path act = do
  liftIO $ createDirectoryIfMissing True (takeDirectory path)
  conn <- liftIO $ SQL.open path
  writeLock <- newMVar True
  let state = DBState conn writeLock
  act (Connection state) `finally` SQL.close conn

runSQL :: Connection -> SQL a -> IO a
runSQL (Connection state) m = runReaderT m state

-- | Access an SQL database
class Monad m => MonadSQL m where
    execute  :: ToRow q => Query -> q -> m ()
    execute_ :: Query -> m ()
    query    :: (ToRow q, FromRow r) => Query -> q -> m [r]
    query_   :: (FromRow r) => Query -> m [r]
    queryNamed :: FromRow r => Query -> [NamedParam] -> m [r]
    withTransaction :: m a -> m a

data DBState = DBState
    { conn :: SQL.Connection
    , writeLock :: MVar Bool -- ^ Is transaction still active
    }

type SQL a = ReaderT DBState IO a

instance MonadSQL (ReaderT DBState IO) where
    execute  q v = write $ \conn -> liftIO $ SQL.execute conn q v
    execute_ q   = write $ \conn -> liftIO $ SQL.execute_ conn q
    query    q v = reader conn >>= \conn -> liftIO $ SQL.query conn q v
    query_   q   = reader conn >>= \conn -> liftIO $ SQL.query_ conn q
    queryNamed q v = reader conn >>= \conn -> liftIO $ SQL.queryNamed conn q v
    withTransaction action =
      write $ \conn -> do
        lock <- liftIO $ newMVar True
        let close = swapMVar lock False
        local (\s -> s { writeLock = lock }) $ runTransaction conn close
      where
        runTransaction conn close = do
          state <- ask
          liftIO $ mask $ \restore -> do
            begin
            r <- (restore (runReaderT action state) `finally` close)
                `onException` rollback
            commit
            return r
          where
            begin    = liftIO $ SQL.execute_ conn "BEGIN TRANSACTION"
            commit   = liftIO $ SQL.execute_ conn "COMMIT TRANSACTION"
            rollback = liftIO $ SQL.execute_ conn "ROLLBACK TRANSACTION"

write :: (SQL.Connection -> SQL a) -> SQL a
write action = do
    state@(DBState conn writeLock) <- ask
    liftIO $ withMVar writeLock $ \case
        False -> error "Trying to write after transaction was closed"
        True  -> runReaderT (action conn) state
