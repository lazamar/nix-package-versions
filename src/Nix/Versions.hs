{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Nix.Versions
    ( savePackageVersionsForPeriod
    ) where

import Control.Monad.Except (liftIO )
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Catch (MonadMask)
import Control.Concurrent.Classy (MonadConc)
import Control.Concurrent.Classy.Async (mapConcurrently)
import Data.Time.Calendar (Day, showGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate, fromWeekDate)
import Data.Hashable (Hashable)
import Nix.Versions.Types (Commit(..), Config(..))
import Nix.Revision (Revision(..), Channel(..), build, revisionsOn)
import Control.Monad ((<=<), (>>))
import Data.Set (Set)
import Nix.Versions.Database (Connection, RevisionState(..))

import qualified Nix.Versions.Database as DB
import qualified Data.Set as Set

-- | Download lists of packages and their versions
-- for commits between 'to' and 'from' dates and save them to
-- the database.
savePackageVersionsForPeriod
    :: (MonadMask m, MonadConc m, MonadIO m)
    => Config -> Day -> Day -> m [Either String Revision]
savePackageVersionsForPeriod (Config dbFile cacheDir gitUser) from to =
    DB.withConnection cacheDir dbFile $ \conn -> do
        allRevisions <- mapConcurrently (revisionsForChannel conn) allChannels
        return $ concat allRevisions
    where
        allChannels :: [Channel]
        allChannels = [minBound..]

        revisionsForChannel :: _ => Connection -> Channel -> m [Either String Revision]
        revisionsForChannel conn channel = do
            revisions <- DB.revisions conn channel
            let
                weeksAlreadyInDB :: Set (Year, Week)
                weeksAlreadyInDB
                    = Set.fromList
                    $ fmap (toWeek . revDate)
                    $ filter wasAttempted
                    $ revisions

                weeksAsked
                    = Set.toList . Set.fromList -- Remove repeats
                    $ filter (isWeekOfInterest . snd)
                    $ toWeek <$> [from .. to]

                -- | TODO: Add log of what is being skipped
                weeksNeeded = filter (not . (`Set.member` weeksAlreadyInDB)) weeksAsked

                -- | One day per week of interest
                daysNeeded = toDay <$> weeksNeeded

            res <- mapConcurrently (downloadForDay conn channel) daysNeeded
            return res

        wasAttempted (_,_,state) = case state of
            Success         -> True
            InvalidRevision -> True
            Incomplete      -> False

        revDate (day, _, _) = day

        downloadForDay conn channel day = do
            eCommits <- revisionsOn gitUser channel day
            case eCommits of
                Left err -> return $ Left $ "Unable to get commits from GitHub for " <> show day <> ": " <> show err
                Right commits ->
                    -- We will try only a few commits. If they don't succeed we give up on that revision.
                    let maxAttempts = 20
                    in
                    tryInSequence (day, "Unable to create dervation after " <> show maxAttempts <> " attempts.")
                        $ fmap (download conn day <=< announce day)
                        $ take maxAttempts
                        $ zip ([1..] :: [Int]) commits

        announce day (tryCount, rev@(Revision _ (Commit hash _))) = do
            liftIO $ putStrLn $ "Attempt " <> show tryCount <> ". Downloading files for " <> showGregorian day <> ". " <> show hash
            return rev

        download conn day revision = do
            eRev <- build revision
            case eRev of
                Left  err -> do
                    DB.registerInvalidRevision conn day revision
                    return (Left (day, err))
                Right packages -> do
                    liftIO $ putStrLn $ "Saving Nix result for" <> show revision
                    DB.save conn day revision packages
                    return $ Right $ revision

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
