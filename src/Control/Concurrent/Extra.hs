module Control.Concurrent.Extra (stream) where

import Control.Concurrent.Async
import Control.Concurrent.STM.TBQueue
import Control.Monad (replicateM_)
import Control.Monad.STM

stream :: Int -> ((a -> IO ()) -> IO b) -> (a -> IO ()) -> IO b
stream concurrency produce consume = do
  queue <- newTBQueueIO (fromIntegral concurrency)
  let runConsumers = replicateConcurrently concurrency (worker queue)
      runProducer =  do
        let enqueue work = atomically $ writeTBQueue queue (Just work)
        res <- produce enqueue
        -- signal consumers to stop
        replicateM_ concurrency $ atomically $ writeTBQueue queue Nothing
        return res
  (res,_) <- concurrently runProducer runConsumers
  return res
  where
    worker queue = do
      mwork <- atomically $ readTBQueue queue
      case mwork of
        Nothing -> return () -- finish worker
        Just work -> do
          consume work
          worker queue
