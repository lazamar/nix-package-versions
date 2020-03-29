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
savePackageVersionsForPeriod :: Config -> Day -> Day -> IO [Either (Day, String) Commit]
savePackageVersionsForPeriod (Config dbFile cacheDir gitUser) from to = do
    conn <- DB.connect cacheDir dbFile
    res <- mapConcurrently (f conn) dates
    DB.disconnect conn
    return res
    where
        f conn date = do
            eCommits <- Revision.commitsAt cacheDir gitUser date
            case eCommits of
                Left err -> return $ Left (date, err)
                Right coms ->
                    -- We will try only a few commits. If they don't succeed we give up on that revision.
                    let commits = take 4 coms
                    in
                    tryInSequence (date, "Unable to create dervation after " <> show (length commits) <> " attempts.")
                        $ fmap (save conn <=< download <=< announce)
                        $ zip [1..] commits

        (fromYear, _, _) = toGregorian from

        (toYear, _, _) = toGregorian to

        -- One entry per month
        dates
            = takeWhile (<= to)
            $ dropWhile (< from)
            $ [fromGregorian year month 1
              | year <- [fromYear .. toYear]
              , month <- [1..12]
              ]

        announce (tryCount, Commit hash date) = do
            putStrLn $ "Attempt " <> show tryCount <> ". Downloading files for " <> showGregorian date
            return (Commit hash date)

        download commit@(Commit _ date) = first (date,) <$> Revision.load cacheDir commit

        save _    (Left err)  = return (Left err)
        save conn (Right rev) = do
           DB.save conn rev
           return $ Right $ Revision.commit rev

        eitherToMaybe = either (const Nothing) Just

tryInSequence :: Show e => e -> [IO (Either e a)] -> IO (Either e a)
tryInSequence err []     = return $ Left err
tryInSequence err (x:xs) = x >>= either (\e -> print e >> tryInSequence err xs) (return . Right)

