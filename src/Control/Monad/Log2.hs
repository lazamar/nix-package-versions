{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
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

import Control.Monad.Log (MonadLog, Handler, WithSeverity(..), LoggingT(..), runLoggingT)
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Conc.Class (MonadConc)

type LoggerT = LoggingT

type MonadLog2 = MonadLog (WithSeverity String)

runLoggerT :: Handler m message -> LoggerT message m a ->  m a
runLoggerT = flip runLoggingT

pretty :: WithSeverity String -> String
pretty (WithSeverity severity msg) = unwords [ "[", show severity, "]", msg ]

inTerminal :: Handler IO (WithSeverity String)
inTerminal = putStrLn . pretty

deriving newtype instance MonadConc m => MonadConc (LoggingT messsage m)
