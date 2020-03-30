{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Nix.Versions
    ( savePackageVersionsForPeriod
    ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.Except (liftIO, liftEither, runExceptT, runExcept)
import Control.Monad (join)
import Data.Time.Calendar (Day, fromGregorian, toGregorian, showGregorian)
import Data.Either (partitionEithers)
import Data.Aeson (encodeFile, eitherDecodeFileStrict)
import Nix.Versions.Types (CachePath, Commit(..), Config(..))
import Data.Maybe (mapMaybe)
import Data.Bifunctor (first)
import Control.Monad ((<=<), (>>))
import Nix.Versions.Database as DB

import qualified Nix.Revision as Revision

-- | Download lists of packages and their versions
-- for commits between 'to' and 'from' dates and save them to
-- the database.
savePackageVersionsForPeriod :: Config -> Day -> Day -> IO [Either String Commit]
savePackageVersionsForPeriod (Config dbFile cacheDir gitUser) from to = do
    conn <- DB.connect cacheDir dbFile
    res <- mapConcurrently (f conn) dates
    DB.disconnect conn
    return res
    where
        f conn date = do
            eCommits <- Revision.commitsAt cacheDir gitUser date
            case eCommits of
                Left err -> return $ Left $ "Unable to get commits from GitHub for " <> show date <> ": " <> show err
                Right coms ->
                    -- We will try only a few commits. If they don't succeed we give up on that revision.
                    let maxAttempts = 20
                        commits = take maxAttempts coms
                    in
                    tryInSequence (date, "Unable to create dervation after " <> show (length commits) <> " attempts.")
                        $ fmap (download conn <=< announce)
                        $ zip [1..] commits

        (fromYear, _, _) = toGregorian from

        (toYear, _, _) = toGregorian to

        -- One entry per month
        dates
            = takeWhile (<= to)
            $ dropWhile (< from)
            $ [ fromGregorian year month 31
              | year <- [fromYear .. toYear]
              , month <- [1..12]
              ]

        announce (tryCount, Commit hash date) = do
            putStrLn $ "Attempt " <> show tryCount <> ". Downloading files for " <> showGregorian date <> ". " <> show hash
            return (Commit hash date)

        download conn commit@(Commit _ date) = do
            mRev <- Revision.loadCached cacheDir commit
            case mRev of
                Just _  -> do
                    putStrLn $ "Cached result for " <> show commit
                    return $ Right commit
                Nothing -> do
                    eRev <- Revision.loadFromNixpkgs cacheDir commit
                    case eRev of
                        Left  err -> return (Left (date, err))
                        Right rev -> do
                            putStrLn $ "Saving Nix result for" <> show commit
                            DB.save conn rev
                            return $ Right $ Revision.commit rev

        eitherToMaybe = either (const Nothing) Just

tryInSequence :: Show e => e -> [IO (Either e a)] -> IO (Either String a)
tryInSequence err l = go 0 l
    where
        go n [] = return $ Left $ "Failed after " <> show n <> " attempts with: " <> show err
        go n (x:xs) = x >>= either (\e -> print e >> go (n + 1) xs) (return . Right)

