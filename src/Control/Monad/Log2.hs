{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | We needed an instance of MonadLog that was also an instance of MonadConc
module Control.Monad.Log2 (LoggerT, runLoggerT, pretty) where

import Control.Monad.Log (MonadLog(..), Handler, WithSeverity(..))
import Control.Monad.Reader (ReaderT(..))
import Data.List (foldl1')

type LoggerT m message a = ReaderT (Handler m message) m a

runLoggerT :: Handler m message -> LoggerT m message a -> m a
runLoggerT = flip runReaderT

pretty :: WithSeverity String -> String
pretty (WithSeverity severity msg) = unwords [ "[", show severity, "]", msg ]

instance {-# OVERLAPPING #-} Monad m => MonadLog message (ReaderT (Handler m message) m) where
      logMessageFree foldMap' = ReaderT (\handler -> foldl1' (*>) (foldMap' (pure . handler)))



