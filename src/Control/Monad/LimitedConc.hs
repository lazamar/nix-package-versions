{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.LimitedConc
    ( MonadLimitedConc
    , LimitedConcT
    , runTask
    , asyncTask
    , runMonadLimitedConc
    ) where

{- | A Monad to limit the amount of concurrent threads for a given task

This is useful because if we create too many threads that download things
from Nix we will exhaust the computer's resources and crash.
-}

import Control.Concurrent.Classy.Async (async, Async)
import Control.Monad.Trans.Reader (ReaderT, reader, runReaderT)
import Control.Monad.Trans.Control (liftWith)
import Control.Monad.Catch (finally)
import Control.Monad.Conc.Class (MonadConc, STM, atomically)
import Control.Monad.STM.Class (retry)
import Control.Concurrent.Classy.MVar (MVar, modifyMVar, newMVar)
import Control.Concurrent.Classy.STM.TVar (TVar, newTVar, readTVar, modifyTVar)
import Data.Map (Map)
import Data.Maybe (fromMaybe)

import qualified Data.Map as Map

class MonadConc m => MonadLimitedConc k m | m -> k where
    runTask   :: k -> m a -> m a
    asyncTask :: k -> m a -> m (Async m a)
    asyncTask k action = async $ runTask k action
    {-# MINIMAL runTask #-}

-- | Pass-through instance
instance {-# OVERLAPPABLE #-} (MonadLimitedConc k m) => MonadLimitedConc k (ReaderT r m) where
    runTask k action = liftWith (\run -> runTask k $ run action)

data ConcState m k = ConcState
    { conc_limits :: Map k Int
    , conc_used :: MVar m (Map k (TVar (STM m) Int))
    }

type LimitedConcT k m = ReaderT (ConcState m k) m

runMonadLimitedConc :: (Ord k, MonadConc m) => Map k Int -> LimitedConcT k m a -> m a
runMonadLimitedConc limits r = do
    usedMapVar <- newMVar mempty
    runReaderT r ConcState { conc_limits = limits , conc_used = usedMapVar }

instance (Ord k, MonadConc m ) => MonadLimitedConc k (LimitedConcT k m) where
    runTask key task = do
        limits <- reader conc_limits
        let maxThreads = fromMaybe maxBound $ Map.lookup key limits

        usedMapVar <-reader conc_used
        threadPool <- modifyMVar usedMapVar $ \usedMap -> do
            threadPool <- maybe (atomically $ newTVar 0) return $ Map.lookup key usedMap
            return (Map.insert key threadPool usedMap, threadPool)

        -- | Block until a thread becomes available for this task
        atomically $ do
            used <- readTVar threadPool
            if used < maxThreads then do
                -- | Take a thread
                modifyTVar threadPool (+ 1)
                return ()
            else
                retry

        finally task
            -- | Return the thread to the pool
            (atomically $ modifyTVar threadPool (\v -> v - 1))


