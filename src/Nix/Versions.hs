{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Nix.Versions
    ( savePackageVersionsForPeriod
    , saveP
    ) where

import Control.Arrow ((&&&))
import Control.Concurrent.Classy.Async (async, wait, race, mapConcurrently, forConcurrently)
import Control.Monad.Conc.Class (MonadConc, STM, atomically, getNumCapabilities, threadDelay)
import Control.Monad.STM.Class (TVar, newTVar, readTVar, writeTVar, retry)
import Control.Monad ((<=<), (>>), foldM, forever, void)
import Control.Monad.Catch (MonadMask, finally)
import Control.Monad.Except (liftIO )
import Control.Monad.Fail (MonadFail)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Log (MonadLog, WithSeverity, logDebug, logInfo)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Data.Bifunctor (first)
import Data.Functor ((<&>))
import Data.Hashable (Hashable)
import Data.Set (Set)
import Data.Text (pack)
import Data.Time.Calendar (Day, showGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate, fromWeekDate)
import Nix.Revision (Revision(..), Channel(..), build, revisionsOn, RevisionPackages)
import Nix.Versions.Database (Connection, RevisionState(..))
import Nix.Versions.Types (Hash(..), Commit(..), Config(..))
import Control.Monad.Revisions (MonadRevisions, packagesFor)

import qualified Nix.Versions.Database as DB
import qualified Data.Set as Set
import qualified Data.Map as Map

saveP :: (MonadLog (WithSeverity String) m, MonadConc m, MonadFail m, MonadIO m, MonadRevisions m)
    => Config -> Day -> Day -> m [Either String String]
saveP (Config dbFile cacheDir gitUser) from to =
    DB.withConnection cacheDir dbFile $ \conn -> do
        daysToDownload <- forConcurrently [minBound..] $ \channel -> do
            days <- daysNeededFor conn channel
            return $ (channel,) <$> days
        mapConcurrently (uncurry $ buildAndSaveDay conn) $ concat daysToDownload
    where
        buildAndSaveDay :: _ => Connection -> Channel -> Day -> m (Either String String)
        buildAndSaveDay conn channel day = runExceptT $ do
            dayRevisions <- ExceptT $ revisionsOn gitUser channel day
            -- We will try only a few revisions. If they don't succeed we give up on that revision.
            ExceptT
                $ tryInSequence logInfo (unwords ["Unable to create dervation for",  show channel, showGregorian day])
                $ fmap (\r -> saveToDatabase conn day r =<< download r)
                $ take maxAttempts
                $ dayRevisions

        download :: _ => Revision -> m (Either String RevisionPackages)
        download (Revision _ commit) = first show <$> packagesFor commit

        maxAttempts = 10

        daysNeededFor :: _ => Connection -> Channel -> m [Day]
        daysNeededFor conn channel = do
            dbRevisions <- DB.revisions conn channel
            return
                $ Set.toList
                $ daysAsked `Set.difference` daysAvailable maxAttempts dbRevisions

        daysAsked :: Set Day
        daysAsked
            = Set.fromList
            $ map toMonday
            $ filter (isWeekOfInterest . snd)
            $ toWeek <$> [from .. to]

        saveToDatabase :: _ => Connection -> Day -> Revision -> Either String RevisionPackages -> m (Either String String)
        saveToDatabase conn day revision ePackages = do
            case ePackages  of
                Left err -> do
                    logDebug $ msg $ "Saving invalid revision"
                    DB.saveRevision conn day revision InvalidRevision
                    return $ Left err
                Right packages -> do
                    logInfo $ msg "Saving successful Nix revision"
                    DB.saveRevisionWithPackages conn day revision packages
                    logInfo $ msg "Saved"
                    return $ Right $ msg "Success"
            where
                Revision channel commit = revision
                Commit hash _ = commit
                padded = take 20 . (<> repeat ' ')
                msg txt = unwords [padded txt, showGregorian day, show channel, show hash]

daysAvailable :: Int -> [(Day, Revision, RevisionState)] -> Set Day
daysAvailable maxAttempts
    = Set.fromList
    . fmap (toMonday . toWeek)
    . Map.keys
    . Map.filter triesWereExhausted
    . Map.fromListWith (\(c1, s1) (c2, s2) -> (c1 + c2, max s1 s2))
    . fmap (revDate &&& ((1,) . revState))
    where
        revState (_,_,s) = s
        revDate  (d,_,_) = d
        triesWereExhausted (count, state) =
            count >= maxAttempts || state == Success

-- | Download lists of packages and their versions
-- for commits between 'to' and 'from' dates and save them to
-- the database.
savePackageVersionsForPeriod
    :: forall m . (MonadMask m, MonadConc m, MonadIO m, MonadLog (WithSeverity String) m)
    => Config -> Day -> Day -> m [Either String String]
savePackageVersionsForPeriod (Config dbFile cacheDir gitUser) from to =
    DB.withConnection cacheDir dbFile $ \conn -> do
        key <- newConcKey
        threadMonitor key
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

        weeksAvailable :: [(Day, Revision, RevisionState)] -> Set (Year, Week)
        weeksAvailable
            = Set.fromList
            . fmap toWeek
            . Map.keys
            . Map.filter triesWereExhausted
            . Map.fromListWith (\(c1, s1) (c2, s2) -> (c1 + c2, max s1 s2))
            . fmap (revDate &&& ((1,) . revState))
            where
                triesWereExhausted (count, state) =
                    count >= maxAttempts || state == Success

        revDate (day, _, _)  = day
        revState (_,_,state) = state

        revisionsForChannel :: _ => Connection -> Channel -> m [Either String String]
        revisionsForChannel conn channel = do
            revisions <- DB.revisions conn channel
            -- TODO: Add log of what is being skipped
            -- One day per week of interest
            let daysNeeded
                    = map toMonday
                    $ Set.toList
                    $ weeksAsked `Set.difference` weeksAvailable revisions
            traverse (printAndReturn <=< downloadForDay conn channel) daysNeeded
            where
                printAndReturn v = do
                    liftIO (print v)
                    return v

        maxAttempts = 40

        downloadForDay conn channel day = do
            eRevisions <- revisionsOn gitUser channel day
            case eRevisions of
                Left err ->
                    return $ Left $ "Unable to get commits from GitHub for " <> show day <> ": " <> show err

                Right [] -> do
                    -- Placeholder revision just to say that this day is invalid
                    DB.saveRevision conn day (Revision channel (Commit (Hash $ pack $ show day) day)) InvalidRevision
                    let msg = "No commits found for " <> show day
                    liftIO $ putStrLn msg
                    return $ Left msg

                Right revisions ->
                    -- We will try only a few revisions. If they don't succeed we give up on that revision.
                    tryInSequence' (day, channel, "Unable to create dervation after " <> show maxAttempts <> " attempts.")
                        $ fmap (download conn day)
                        $ take maxAttempts
                        $ zip ([1..] :: [Int])
                        $ revisions

        download conn day (tryCount, revision@(Revision channel commit)) = do
            mState <- DB.revisionState conn revision
            case mState of
                Just InvalidRevision -> return $ Left (day, channel, "Skipped InvalidRevision")
                Just Success         -> return $ Right  $ msg "Skipped Success"
                _ -> do
                    liftIO $ putStrLn $ msg $ "Attempt " <> show tryCount
                    DB.saveRevision conn day revision PreDownload
                    eRev <- build revision
                    case eRev of
                        Left  err -> do
                            DB.saveRevision conn day revision InvalidRevision
                            return $ Left (day, channel, err)
                        Right packages -> do
                            liftIO $ putStrLn $ msg "Saving Nix result"
                            DB.saveRevisionWithPackages conn day revision packages
                            return $ Right $ msg "Downloaded"
            where
                Commit hash _ = commit
                padded = take 20 . (<> repeat ' ')
                msg txt = unwords [padded txt, showGregorian day, show channel, show hash]

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

tryInSequence' :: (Show e, MonadIO m) => e -> [m (Either e a)] -> m (Either String a)
tryInSequence' err l = go (0 :: Int) l
    where
        go n [] = return $ Left $ "Failed after " <> show n <> " attempts with: " <> show err
        go n (x:xs) = x >>= either (\e -> liftIO (print e) >> go (n + 1) xs) (return . Right)

tryInSequence :: Monad m => (e' -> m ()) -> e -> [m (Either e' b)] -> m (Either e b)
tryInSequence onError err values =
    case values of
        []      -> return $ Left err
        (x:xs)  -> x >>= \case
            Right result -> return $ Right result
            Left e       -> onError e >> tryInSequence onError err xs


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
            a <- async $ finally action releaseCapability
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


threadMonitor :: (MonadIO m, MonadConc m) => ConcKey m -> m ()
threadMonitor ConcKey{..} = void $ async $ race showActive ended
    where
        showActive = forever $ do
            threadDelay 5000000
            active <- atomically $ readTVar activeThreads
            liftIO $ putStrLn $ "Active threads: " <> show active

        ended = do
            threadDelay 5000000
            atomically $ do
                active <- readTVar activeThreads
                if active == 0
                   then return ()
                   else retry
