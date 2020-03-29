{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}

{- A general purpose stateful cache.
-}

module Nix.Cache
    ( CacheT(..)
    ) where


-- | A transformer that adds caching capabilities to a monad
-- The goal is to avoid getting uncached values as much as possible.
class Monad m => CacheT (m :: * -> *) key value | key -> value where
    getCached   :: key -> m (Maybe value)
    getUncached :: key -> m value

    -- | Save a value to the cache
    addToCache  :: (key, value) -> m ()

    -- | Get a value from the cache if available
    retrieve    :: key -> m value
    retrieve key = do
        res <- getCached key
        case res of
          Just value -> return value
          Nothing -> do
              value <- getUncached key
              addToCache (key, value)
              return value

