{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module Nix.Versions
    ( savePackageVersionsForPeriod
    ) where

import Control.Monad.Except (liftIO, liftEither, runExceptT, runExcept)
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Concurrent.Classy (MonadConc)
import Control.Concurrent.Classy.Async (mapConcurrently)
import Data.Time.Calendar (Day, fromGregorian, toGregorian, showGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate, fromWeekDate)
import Data.Either (partitionEithers)
import Data.Bifunctor (first)
import Data.Hashable (Hashable)
import Data.Aeson (encodeFile, eitherDecodeFileStrict)
import Nix.Versions.Types (CachePath, Commit(..), Config(..))
import Data.Maybe (mapMaybe)
import Data.Bifunctor (first)
import Control.Monad ((<=<), (>>))
import Nix.Versions.Database as DB
import Data.Set (Set)

import qualified Data.Set as Set
import qualified Nix.Revision as Revision

-- | Download lists of packages and their versions
-- for commits between 'to' and 'from' dates and save them to
-- the database.
savePackageVersionsForPeriod :: (MonadConc m, MonadIO m) => Config -> Day -> Day -> m [Either String Commit]
savePackageVersionsForPeriod (Config dbFile cacheDir gitUser) from to = do
    conn      <- liftIO $ DB.connect cacheDir dbFile
    revisions <- liftIO $ DB.revisions conn
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

    res <- mapConcurrently (downloadForDay conn Revision.Nixpkgs_unstable) daysNeeded
    liftIO $ DB.disconnect conn
    return res
    where
        wasAttempted (_,_,state) = case state of
            Success         -> True
            InvalidRevision -> True
            Incomplete      -> False

        revDate (day, _, _) = day

        downloadForDay conn channel day = do
            eCommits <- liftIO $ Revision.commitsUntil gitUser (Revision.channelBranch channel) day
            case eCommits of
                Left err -> return $ Left $ "Unable to get commits from GitHub for " <> show day <> ": " <> show err
                Right commits ->
                    -- We will try only a few commits. If they don't succeed we give up on that revision.
                    let maxAttempts = 20
                    in
                    tryInSequence (day, "Unable to create dervation after " <> show maxAttempts <> " attempts.")
                        $ fmap (download conn channel day <=< announce day)
                        $ take maxAttempts
                        $ zip [1..] commits

        announce day (tryCount, commit@(Commit hash _)) = do
            liftIO $ putStrLn $ "Attempt " <> show tryCount <> ". Downloading files for " <> showGregorian day <> ". " <> show hash
            return commit

        download conn channel day commit = do
            eRev <- liftIO $ Revision.build channel commit
            case eRev of
                Left  err -> do
                    liftIO $ DB.registerInvalidRevision conn day (Revision.Revision channel commit)
                    return (Left (day, err))
                Right (rev, packages) -> do
                    liftIO $ putStrLn $ "Saving Nix result for" <> show commit
                    liftIO $ DB.save conn day rev packages
                    return $ Right $ Revision.commit rev

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
tryInSequence err l = go 0 l
    where
        go n [] = return $ Left $ "Failed after " <> show n <> " attempts with: " <> show err
        go n (x:xs) = x >>= either (\e -> liftIO (print e) >> go (n + 1) xs) (return . Right)


-- | We are not interested in every week of the year.
-- We only download revision data for the weeks of interest
isWeekOfInterest :: Week -> Bool
isWeekOfInterest (Week week) = week `mod` 5 == 1
