{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}

{- A general purpose stateful cache.
-}

module Nix.Cache
    ( Cache(..)
    ) where


-- | The goal is to avoid getting uncached values as much as possible.
class Monad m => Cache (m :: * -> *) key value | key -> value where
    getCached   :: key -> m (Maybe value)
    getUncached :: key -> m (Either String value)

    -- | Save a value to the cache
    addToCache  :: (key, value) -> m ()

    -- | Get a value from the cache if available
    retrieve    :: key -> m (Either String value)
    retrieve key = do
        res <- getCached key
        case res of
            Just value -> return $ Right value
            Nothing -> do
                evalue <- getUncached key
                either (const $ return ()) (addToCache . (key,)) evalue
                return evalue
    {-# MINIMAL getCached, getUncached, addToCache #-}

