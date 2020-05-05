{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

{- This module takes care of populating the database
-}

module Nix.Versions
    ( savePackageVersionsForPeriod
    ) where

import Control.Arrow ((&&&))
import Control.Concurrent.Classy.Async (mapConcurrently, forConcurrently)
import Control.Monad.Conc.Class (MonadConc)
import Control.Monad.LimitedConc (runTask, MonadLimitedConc)
import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Log (MonadLog, WithSeverity, logDebug, logInfo)
import Data.Bifunctor (first)
import Data.Hashable (Hashable)
import Data.Set (Set)
import Data.Time.Calendar (Day, showGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate, fromWeekDate)
import Nix.Revision (Revision(..), Channel(..), revisionsOn, RevisionPackages)
import Nix.Versions.Database (Connection, RevisionState(..))
import Nix.Versions.Types (Commit(..), Config(..), Task(..))
import Control.Monad.Revisions (MonadRevisions, packagesFor)

import qualified Nix.Versions.Database as DB
import qualified Data.Set as Set
import qualified Data.Map as Map

-- | Download lists of packages and their versions for commits
-- between 'to' and 'from' dates and save them to the database.
savePackageVersionsForPeriod ::
    ( MonadLog (WithSeverity String) m
    , MonadLimitedConc Task m
    , MonadConc m
    , MonadFail m
    , MonadIO m
    , MonadRevisions m
    )
    => Config -> Day -> Day -> m [Either String String]
savePackageVersionsForPeriod (Config dbFile cacheDir gitUser) from to =
    DB.withConnection cacheDir dbFile $ \conn -> do
        daysToDownload <- forConcurrently [minBound..] $ \channel -> do
            days <- daysNeededFor conn channel
            return $ (channel,) <$> days
        results <- mapConcurrently (uncurry $ buildAndSaveDay conn) $ concat daysToDownload
        return $ concat results
    where
        maxAttempts = 10

        buildAndSaveDay :: _ => Connection -> Channel -> Day -> m [Either String String]
        buildAndSaveDay conn channel day = do
            revisionsOn gitUser channel day >>= \case
                Left err -> return $ [Left err]
                Right dayRevisions ->
                    -- We will try only a few revisions. If they don't succeed we give up on that revision.
                    tryInSequence
                        $ fmap (\r -> saveToDatabase conn day r =<< download r)
                        $ take maxAttempts
                        $ dayRevisions

        download :: _ => Revision -> m (Either String RevisionPackages)
        download (Revision _ commit) = first show <$> packagesFor commit

        daysNeededFor :: _ => Connection -> Channel -> m [Day]
        daysNeededFor conn channel = do
            dbRevisions <- DB.revisions conn channel
            return
                $ fmap toMonday
                $ Set.toList
                $ weeksAsked  `Set.difference` weeksCompleted  maxAttempts dbRevisions

        weeksAsked :: Set (Year, Week)
        weeksAsked
            = Set.fromList
            $ filter (isWeekOfInterest . snd)
            $ toWeek <$> [from .. to]

-- | Given the result of a revision download attempt, save the appropriate
-- result to the database and return some info about what was done
saveToDatabase :: _ => Connection -> Day -> Revision -> Either String RevisionPackages -> m (Either String String)
saveToDatabase conn day revision ePackages =
    runTask SaveToDatabase $ do
        case ePackages  of
            Left err -> do
                logDebug $ msg $ "Saving invalid"
                DB.saveRevision conn day revision InvalidRevision
                return $ Left err
            Right packages -> do
                logInfo $ msg "Saving successful"
                DB.saveRevisionWithPackages conn day revision packages
                logInfo $ msg "Saved"
                return $ Right $ msg "Success"
    where
        Revision channel commit = revision
        Commit hash _ = commit
        padded = take 20 . (<> repeat ' ')
        msg txt = unwords [padded txt, showGregorian day, show channel, show hash]

-- | Given a list of revisions successfully or unsuccessfully downloaded
-- return a list of weeks for which we shouldn't try to download revisions
-- any longer.
weeksCompleted :: Int -> [(Day, Revision, RevisionState)] -> Set (Year, Week)
weeksCompleted maxAttempts
    = Set.fromList
    . fmap toWeek
    . Map.keys
    . Map.filter triesWereExhausted
    . Map.fromListWith (\(c1, s1) (c2, s2) -> (c1 + c2, max s1 s2))
    . fmap (revDate &&& ((1,) . revState))
    where
        revState (_,_,s) = s
        revDate  (d,_,_) = d
        triesWereExhausted (count, state) =
            count >= maxAttempts || state == Success

newtype Week = Week Int
    deriving (Eq, Show, Ord)
    deriving newtype (Hashable)

newtype Year = Year Integer
    deriving (Eq, Show, Ord)
    deriving newtype (Hashable)

toWeek :: Day -> (Year, Week)
toWeek day = (Year year, Week week)
    where
        (year,week,_) = toWeekDate day

-- | Returns the Monday from that week
toMonday :: (Year, Week) -> Day
toMonday (Year year, Week week) = fromWeekDate year week 1

-- | Stops at first right
tryInSequence :: Monad m => [m (Either a b)] -> m [Either a b]
tryInSequence values = go [] values
    where
        go acc []     = return acc
        go acc (x:xs) = x >>= \case
            Right result -> return $ (Right result):acc
            Left e       -> go (Left e:acc) xs


-- | We are not interested in every week of the year.
-- We only download revision data for the weeks of interest
isWeekOfInterest :: Week -> Bool
isWeekOfInterest (Week week) = week `mod` 5 == 1
