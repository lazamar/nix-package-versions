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
    ) where

import Control.Monad.Log (MonadLog, Handler, WithSeverity(..), LoggingT(..), runLoggingT, logMessage)
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Conc.Class (MonadConc)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.CPUTime (getCPUTime)

type LoggerT = LoggingT

type MonadLog2 = MonadLog (WithSeverity String)

runLoggerT :: Handler m message -> LoggerT message m a ->  m a
runLoggerT = flip runLoggingT

pretty :: WithSeverity String -> String
pretty (WithSeverity severity msg) = unwords [ "[", show severity, "]", msg ]

inTerminal :: Handler IO (WithSeverity String)
inTerminal = putStrLn . pretty

deriving newtype instance MonadConc m => MonadConc (LoggingT messsage m)

logTimed :: (MonadIO m , MonadLog (WithSeverity String) m) => WithSeverity String -> m a -> m a
logTimed msg action = do
    t1 <- liftIO getCPUTime
    a <- action
    t2 <- liftIO getCPUTime
    let t :: Double
        t = fromIntegral (t2-t1) * 1e-12
        time = "[" <> show t <> "]"
    logMessage $ (time <>) <$> msg
    return a
