{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Nix.Versions
    ( savePackageVersionsForPeriod
    ) where

import Control.Concurrent.Async (mapConcurrently)
import Data.Time.Calendar (Day, fromGregorian, toGregorian, showGregorian)
import Data.Either (partitionEithers)
import Data.Aeson (encodeFile, eitherDecodeFileStrict)
import Nix.Versions.Types (CachePath, Commit(..), Config(..))
import Data.Maybe (mapMaybe)
import Data.Bifunctor (first)
import Control.Monad ((<=<))
import Nix.Versions.Database as DB

import qualified Nix.Revision as Revision

-- | Download lists of packages and their versions
-- for commits between 'to' and 'from' dates and save them to
-- the database.
savePackageVersionsForPeriod :: Config -> Day -> Day -> IO [Either (Day, String) Commit]
savePackageVersionsForPeriod (Config dbFile cacheDir gitUser) from to = do
    commits <- mapMaybe eitherToMaybe <$> mapConcurrently (Revision.headAt cacheDir gitUser) dates
    conn <- DB.connect cacheDir dbFile
    res <- mapConcurrently (save conn <=< download <=< announce) commits
    DB.disconnect conn
    return res
    where
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

        announce (Commit hash date) = do
            putStrLn $ "Downloading files for " <> showGregorian date
            return (Commit hash date)

        download commit@(Commit _ date) = first (date,) <$> Revision.load cacheDir commit

        save _    (Left err)  = return (Left err)
        save conn (Right rev) = do
           DB.save conn rev
           return $ Right $ Revision.commit rev

        eitherToMaybe = either (const Nothing) Just

