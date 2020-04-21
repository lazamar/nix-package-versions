{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | We needed an instance of MonadLog that was also an instance of MonadConc
-- We accomplish that with the orphan newtype derivation of MonadConc
module Control.Monad.Log2 (LoggerT, runLoggerT, pretty) where

import Control.Monad.Log (Handler, WithSeverity(..), LoggingT(..), runLoggingT)
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Conc.Class (MonadConc)

type LoggerT = LoggingT

runLoggerT :: Handler m message -> LoggerT message m a ->  m a
runLoggerT = flip runLoggingT

pretty :: WithSeverity String -> String
pretty (WithSeverity severity msg) = unwords [ "[", show severity, "]", msg ]

deriving newtype instance MonadConc m => MonadConc (LoggingT messsage m)
