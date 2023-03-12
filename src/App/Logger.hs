module App.Logger
  ( withLogger
  , Logger
  , logInfo
  , logTimed
  )
  where

import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TQueue
import Control.Concurrent.Async (concurrently)
import Control.Exception (finally)
import Prettyprinter (Pretty(..), Doc, (<+>))
import System.TimeIt (timeItT)
import System.IO (hPutStrLn, stderr)


-- Simple logger for sensible output when logging from multiple threads
newtype Logger = Logger (TQueue (Maybe String))

withLogger :: (Logger -> IO a) -> IO a
withLogger f = do
  q <- newTQueueIO
  let consume = logFrom q
      produce = f (Logger q) `finally` atomically (writeTQueue q Nothing)
  fst <$> concurrently produce consume
  where
    logFrom :: TQueue (Maybe String) -> IO ()
    logFrom q = do
      r <- atomically $ readTQueue q
      case r of
        Nothing -> return ()
        Just v -> do
          hPutStrLn stderr v
          logFrom q

logInfo :: Logger -> Doc ann -> IO ()
logInfo (Logger q) doc = do
  atomically $ writeTQueue q (Just $ show doc)

logTimed :: Logger -> Doc ann -> IO a -> IO a
logTimed logger doc act = do
  (time, result) <- timeItT act
  logInfo logger $ "[" <> pretty time <> "] " <+> doc
  return result

