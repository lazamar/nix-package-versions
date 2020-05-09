{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | We needed an instance of MonadLog that was also an instance of MonadConc
-- We accomplish that with the orphan newtype derivation of MonadConc
module Control.Monad.Log2
    ( LoggerT
    , MonadLog2
    , runLoggerT
    , pretty
    , inTerminal
    , logInfoTimed
    , logDebugTimed
    ) where

import Control.Monad.Log (MonadLog, Handler, WithSeverity(..), LoggingT(..), runLoggingT, logInfo, logDebug)
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Conc.Class (MonadConc)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.CPUTime (getCPUTime)
import System.TimeIt (timeItT)

type LoggerT = LoggingT

type MonadLog2 = MonadLog (WithSeverity String)

runLoggerT :: Handler m message -> LoggerT message m a ->  m a
runLoggerT = flip runLoggingT

pretty :: WithSeverity String -> String
pretty (WithSeverity severity msg) = unwords [ "[", show severity, "]", msg ]

inTerminal :: Handler IO (WithSeverity String)
inTerminal = putStrLn . pretty

deriving newtype instance MonadConc m => MonadConc (LoggingT messsage m)

timed log msg action = do
    (time, result) <- timeItT action
    log $ "[" <> show time <> "] " <> msg
    return result

logInfoTimed :: (MonadIO m , MonadLog (WithSeverity String) m) => String -> m a -> m a
logInfoTimed = timed logInfo

logDebugTimed :: (MonadIO m , MonadLog (WithSeverity String) m) => String -> m a -> m a
logDebugTimed  = timed logDebug
