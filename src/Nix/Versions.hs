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
import Control.Concurrent.Classy.Async (forConcurrently, wait, async)
import Control.Concurrent.Classy.STM.TVar (newTVar, readTVar, writeTVar)
import Control.Monad (foldM)
import Control.Monad.Catch (finally)
import Control.Monad.Conc.Class (MonadConc, atomically)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Log (MonadLog, WithSeverity)
import Control.Monad.Log2 (logInfoTimed)
import Control.Monad.STM.Class (retry)
import Data.Bifunctor (first)
import Data.Hashable (Hashable)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Calendar (Day, showGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate, fromWeekDate)
import Nix.Revision (Revision(..), Channel(..), revisionsOn, RevisionPackages)
import Control.Monad.Revisions (MonadRevisions, packagesFor)

import Data.Git (Commit(..))
import GitHub (AuthenticatingUser(..))
import App.Storage (Database, RevisionState(..))
import qualified App.Storage as Storage

-- | Download lists of packages and their versions for commits
-- between 'to' and 'from' dates and save them to the database.
savePackageVersionsForPeriod ::
    ( MonadLog (WithSeverity String) m
    , MonadConc m
    , MonadFail m
    , MonadIO m
    , MonadRevisions m
    )
    => Database -> AuthenticatingUser -> Day -> Day -> m [Either String String]
savePackageVersionsForPeriod database gitUser from to = do
    let channels = [minBound..]
    daysToDownload <- forConcurrently channels $ \channel -> do
        dbRevisions <- liftIO $ Storage.revisions database channel
        let days = daysMissingIn dbRevisions
            completed = completedRevisions dbRevisions
        return $ (channel, completed,) <$> days
    results <- limitedConcurrency 10 $ fmap buildAndSaveDay $ concat daysToDownload
    return $ concat results
    where
        maxAttempts = 10

        buildAndSaveDay :: _ => (Channel, Set Revision, Day) -> m [Either String String]
        buildAndSaveDay (channel, completed, day) = do
            revisionsOn gitUser maxAttempts channel day >>= \case
                Left err -> return $ [Left err]
                Right dayRevisions ->
                    -- We will try only a few revisions. If they don't succeed we give up on that revision.
                    tryInSequence
                        $ fmap (\r -> saveToDatabase database day r =<< download r)
                        $ filter (not . (`Set.member` completed))
                        $ dayRevisions

        download :: _ => Revision -> m (Either String RevisionPackages)
        download (Revision _ commit) = first show <$> packagesFor commit

        daysMissingIn  :: [(Day, Revision, RevisionState)] -> [Day]
        daysMissingIn revisions
            = fmap toMonday
            $ Set.toList
            $ weeksAsked  `Set.difference` weeksCompleted  maxAttempts revisions

        weeksAsked :: Set (Year, Week)
        weeksAsked
            = Set.fromList
            $ filter (isWeekOfInterest . snd)
            $ toWeek <$> [from .. to]

-- | Given the result of a revision download attempt, save the appropriate
-- result to the database and return some info about what was done
saveToDatabase :: _ => Database -> Day -> Revision -> Either String RevisionPackages -> m (Either String String)
saveToDatabase database day revision ePackages =
    case ePackages  of
        Left err -> do
            logInfoTimed (msg "Saved invalid" $ Just err)
                $ liftIO
                $ Storage.writeRevisionState database day revision InvalidRevision
            return $ Left err
        Right packages -> do
            logInfoTimed (msg "Saved successfull" Nothing)
                $ liftIO
                $ Storage.writePackages database day revision packages
            return $ Right $ msg "Success" Nothing
    where
        Revision channel commit = revision
        Commit hash _ = commit
        padded = take 20 . (<> repeat ' ')
        msg txt mnote = unwords
            [ padded txt
            , showGregorian day
            , show channel
            , show hash
            , case mnote of
                Nothing -> ""
                Just note -> " | " <> note
            ]

completedRevisions :: [(Day, Revision, RevisionState)] -> Set Revision
completedRevisions
    = Set.fromList
    . map (\(_,rev,_) -> rev)
    . filter (\(_,_,state) -> isComplete state)
    where
        isComplete state =
            case state of
              Success         -> True
              InvalidRevision -> True
              PreDownload     -> False
              Incomplete      -> False

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

-- | Run the maximum amount of concurrent computations possible, but no more than that.
--
-- Even though we are limiting the number of threads actively downloading
-- data from nix at any one time, if many threads are trying to write to
-- the database, they will all load the nix data from disk into memory
-- without sharing and be blocked in the queue. This was causing crazy
-- memory usage. Using limitedConcurrency is a simple fix
limitedConcurrency :: MonadConc m => Int -> [m a] -> m [a]
limitedConcurrency maxConcurrency actions = do
    activeThreads <- atomically $ newTVar 0
    let
        takeSlot = atomically $ do
            active <- readTVar activeThreads
            if active < maxConcurrency
               then writeTVar activeThreads (active + 1)
               else retry

        releaseSlot = atomically $ do
            active <- readTVar activeThreads
            writeTVar activeThreads (active - 1)

        -- | Only run the next action if there is an idle CPU core
        runWhenPossible acc action = do
            takeSlot
            a <- async $ finally action releaseSlot
            return $ a:acc

    asyncs <- foldM runWhenPossible [] actions
    traverse wait asyncs

