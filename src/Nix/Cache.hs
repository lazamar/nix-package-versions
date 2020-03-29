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

    -- | Get a value from the cache if available
    retrieve    :: key -> m (Either String value)
    retrieve key = do
        res <- getCached key
        case res of
            Just value -> return $ Right value
            Nothing    -> getUncached key

    cachedKeys :: m [key]

