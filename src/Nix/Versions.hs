{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Nix.Versions
    ( savePackageVersionsForPeriod
    ) where

import Control.Applicative ((<*))
import Control.Concurrent.Classy.Async (async, wait)
import Control.Monad.Conc.Class (MonadConc, STM, atomically, getNumCapabilities)
import Control.Monad.STM.Class (TVar, newTVar, readTVar, writeTVar, retry)
import Control.Monad ((<=<), (>>), foldM)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (liftIO )
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Log (MonadLog, WithSeverity)
import Data.Hashable (Hashable)
import Data.Set (Set)
import Data.Text (pack)
import Data.Time.Calendar (Day, showGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate, fromWeekDate)
import Nix.Revision (Revision(..), Channel(..), build, revisionsOn)
import Nix.Versions.Database (Connection, RevisionState(..))
import Nix.Versions.Types (Hash(..), Commit(..), Config(..))

import qualified Nix.Versions.Database as DB
import qualified Data.Set as Set

-- | Download lists of packages and their versions
-- for commits between 'to' and 'from' dates and save them to
-- the database.
savePackageVersionsForPeriod
    :: (MonadMask m, MonadConc m, MonadIO m, MonadLog (WithSeverity String) m)
    => Config -> Day -> Day -> m [Either String Revision]
savePackageVersionsForPeriod (Config dbFile cacheDir gitUser) from to =
    DB.withConnection cacheDir dbFile $ \conn -> do
        key <- newConcKey
        allRevisions <- limitedConcurrency key $ map (revisionsForChannel conn) allChannels
        return $ concat allRevisions
    where
        allChannels :: [Channel]
        allChannels = [minBound..]

        weeksAsked :: Set (Year, Week)
        weeksAsked
            = Set.fromList
            $ filter (isWeekOfInterest . snd)
            $ toWeek <$> [from .. to]

        triedRevisions :: [(Day, Revision, RevisionState)] -> Set Revision
        triedRevisions
            = Set.fromList
            . fmap (\(_,rev,_) -> rev)
            . filter reachedFinalState

        -- TODO: Currently any week with a revision that reached the final state
        -- is considered to be available. We want to change that so that a week
        -- is available if it either:
        --  - Has one Successful revision
        --  - Has five unsuccessful revisions
        weeksAvailable :: [(Day, Revision, RevisionState)] -> Set (Year, Week)
        weeksAvailable
            = Set.fromList
            . fmap (toWeek . revDate)
            . filter reachedFinalState

        -- | Reaching a final state means that it is not worth it trying to
        -- download that revision again.
        reachedFinalState (_,_,state) = case state of
            Success         -> True
            InvalidRevision -> True
            Incomplete      -> False

        revDate (day, _, _) = day

        revisionsForChannel :: _ => Connection -> Channel -> m [Either String Revision]
        revisionsForChannel conn channel = do
            revisions <- DB.revisions conn channel
            -- TODO: Add log of what is being skipped
            -- One day per week of interest
            let alreadyTried = triedRevisions revisions
                daysNeeded
                    = map toDay
                    $ Set.toList
                    $ weeksAsked `Set.difference` weeksAvailable revisions
            traverse (downloadForDay conn channel alreadyTried) daysNeeded

        downloadForDay conn channel alreadyTried day = do
            eRevisions <- revisionsOn gitUser channel day
            case eRevisions of
                Left err ->
                    return $ Left $ "Unable to get commits from GitHub for " <> show day <> ": " <> show err

                Right [] -> do
                    -- Placeholder revision just to say that this day is invalid
                    DB.saveRevisionWithState conn day (Revision channel (Commit (Hash $ pack $ show day) day)) InvalidRevision
                    let msg = "No commits found for " <> show day
                    liftIO $ putStrLn msg
                    return $ Left msg

                Right revisions ->
                    -- We will try only a few revisions. If they don't succeed we give up on that revision.
                    let maxAttempts = 20
                    in
                    tryInSequence (day, channel, "Unable to create dervation after " <> show maxAttempts <> " attempts.")
                        $ fmap (download conn day <=< announce day)
                        $ take maxAttempts
                        $ zip [1..]
                        $ filter (not . (`Set.member` alreadyTried))
                        $ revisions

        announce :: _ => Day -> (Int, Revision) -> m Revision
        announce day (tryCount, rev@(Revision _ (Commit hash _))) = do
            liftIO $ putStrLn $ "Attempt " <> show tryCount <> ". Downloading files for " <> showGregorian day <> ". " <> show hash
            return rev

        download conn day revision@(Revision channel commit) = do
            s <- commitState conn commit
            case s of
                -- Commit is already in database
                Just _ -> do
                    DB.saveRevision conn day revision
                    return $ Right $ revision
                -- Is new commit
                Nothing -> do
                    DB.saveCommit conn commit PreDownload
                    eRev <- build revision
                    case eRev of
                        Left  err -> do
                            DB.saveRevisionWithState conn day revision InvalidRevision
                            return (Left (day, channel, err))
                        Right packages -> do
                            liftIO $ putStrLn $ "Saving Nix result for" <> show revision
                            DB.saveRevisionWithPackages conn day revision packages
                            return $ Right $ revision

        -- Check whether we already saved the packages associated
        -- with that nixpkgs commit. If we did, just register that this
        -- revision is also valid for our current channel
        commitState :: _ => DB.Connection -> Commit -> m (Maybe RevisionState)
        commitState  conn (Commit hash _) = do
            mCommit <- DB.revisionCommit conn hash
            return $ case snd <$> mCommit of
                Just Success         -> Just Success
                Just InvalidRevision -> Just InvalidRevision
                _                    -> Nothing


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
toDay :: (Year, Week) -> Day
toDay (Year year, Week week) = fromWeekDate year week 1

tryInSequence :: (Show e, MonadIO m) => e -> [m (Either e a)] -> m (Either String a)
tryInSequence err l = go (0 :: Int) l
    where
        go n [] = return $ Left $ "Failed after " <> show n <> " attempts with: " <> show err
        go n (x:xs) = x >>= either (\e -> liftIO (print e) >> go (n + 1) xs) (return . Right)


-- | We are not interested in every week of the year.
-- We only download revision data for the weeks of interest
isWeekOfInterest :: Week -> Bool
isWeekOfInterest (Week week) = week `mod` 5 == 1

----------------------------------------------------------------------------------------------
-- Concurrency

-- | Run the maximum amount of concurrent computations possible, but no more than that.
limitedConcurrency :: MonadConc m => ConcKey m -> [m a] -> m [a]
limitedConcurrency ConcKey {maxConcurrency, activeThreads} actions = do
    asyncs <- foldM runWhenPossible [] actions
    traverse wait asyncs
    where
        -- | Only run the next action if there is an idle CPU core
        runWhenPossible  acc action = do
            takeCapability
            a <- async $ action <* releaseCapability
            return $ a:acc

        takeCapability = atomically $ do
            active <- readTVar activeThreads
            if active < maxConcurrency
               then writeTVar activeThreads (active + 1)
               else retry

        releaseCapability = atomically $ do
            active <- readTVar activeThreads
            writeTVar activeThreads (active - 1)


data ConcKey m = ConcKey
    { maxConcurrency :: Int
    , activeThreads  :: TVar (STM m) Int
    }

newConcKey :: MonadConc m => m (ConcKey m)
newConcKey = do
    maxConcurrency <- getNumCapabilities
    activeThreads  <- atomically $ newTVar 0
    return $ ConcKey maxConcurrency activeThreads

