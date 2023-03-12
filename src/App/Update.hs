{- This module takes care of populating the database
-}

module App.Update
  ( updateDatabase
  , frequencyDays
  , Frequency
  ) where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (forConcurrently)
import Control.Exception (mask, onException)
import Control.Monad (void, when)
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TVar
  (newTVarIO, newTVar, readTVar, writeTVar, TVar)
import Control.Monad.STM.Class (retry)
import Data.Foldable (traverse_)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX (posixDayLength)
import Prettyprinter (Pretty(..), (<+>))

import App.Logger (Logger, withLogger, logInfo, logTimed)
import App.Storage (Database, CommitState(..))
import qualified App.Storage as Storage
import Control.Concurrent.Extra (stream)
import Data.Git (Commit(..))
import Data.Time.Period (Period(..))
import GitHub (AuthenticatingUser(..))
import qualified GitHub
import Nix
  ( Channel(..)
  , nixpkgsRepo
  , channelBranch)
import qualified Nix

data State = State
  { s_commits :: TVar (Map Commit (TVar CommitState))
  }

getOrCreateStateFor :: State -> Commit -> IO (TVar CommitState, Bool)
getOrCreateStateFor State{..} commit =
  atomically $ do
    commits <- readTVar s_commits
    case Map.lookup commit commits of
      Just var -> return (var, False)
      Nothing -> do
        var <- newTVar Incomplete
        writeTVar s_commits $ Map.insert commit var commits
        return (var, True)

setStateFor :: State -> Commit -> CommitState -> IO ()
setStateFor state commit cstate = do
  var <- getStateFor state commit
  atomically $ writeTVar var cstate

-- | Blocks until the state is available
getStateFor :: State -> Commit -> IO (TVar CommitState)
getStateFor State{..} commit = do
  atomically $ do
    commits <- readTVar s_commits
    case Map.lookup commit commits of
      Nothing -> retry
      Just var -> return var

process :: Logger -> Database -> State -> Commit -> IO (TVar CommitState)
process logger db state commit = do
  (var, created) <- getOrCreateStateFor state commit
  when created $
    (do
      logInfo logger $ pretty commit <+> "Nix : loading packages"
      epackages <- logTimed logger (pretty commit <+> "Nix finished") $
        Nix.packagesAt commit
      finalState <- save epackages
      setStateFor state commit finalState)
    `onException` setStateFor state commit Broken
  return var
  where
    save = \case
      Right packages -> do
        logInfo logger $ pretty commit <+> "Writing"
        logTimed logger (pretty commit <+> "Writing finished" ) $ do
          let before = Storage.writeCommitState db commit Incomplete
              succeed = Storage.writeCommitState db commit Success
              failure = Storage.writeCommitState db commit Broken
              act = traverse_ (Storage.writePackage db commit) packages
          mask $ \release -> do
            before
            release act `onException` failure
            succeed
          return Success
      Left err -> do
        logInfo logger $ pretty commit <+> "Writing failed: " <> pretty (show err)
        Storage.writeCommitState db commit Broken
        return Broken

-- Download package information from Nix and save it to the database
-- handling at most `concurrency` parallel commits at once.
--
-- No commit is handled twice. The second time will just point to the
-- result of the first time it was attempted.
parallelWriter
  :: Logger
  -> Database
  -> Int
  -- ^ max parallelism
  -> ((Commit -> IO Bool) -> IO a)
    -- ^ save data about a commit to the db
  -> IO a
parallelWriter logger db concurrency f = do
  var <- newTVarIO mempty
  let state = State var
      -- run at most `concurrency` of these in parallel.
      -- only blocks if the Commit hasn't been handled before.
      consume commit = void $ process logger db state commit
      produce enqueue = f $ \commit -> do
        () <- enqueue commit
        cvar <- getStateFor state commit
        atomically $ do
          cstate <- readTVar cvar
          if not (isFinal cstate)
            then retry
            else return $ cstate == Success
  stream concurrency produce consume

newtype Frequency = Frequency NominalDiffTime
  deriving (Show, Eq, Ord)
  deriving newtype (Num, Real)

frequencyDays :: Int -> Frequency
frequencyDays n = Frequency $ fromIntegral n * posixDayLength

-- | Download lists of packages and their versions for commits
-- between 'to' and 'from' dates and save them to the database.
updateDatabase
  :: Database
  -> Frequency
  -> AuthenticatingUser
  -> Period
  -> IO [Either String String]
updateDatabase database freq user targetPeriod =
  withLogger $ \logger -> do
  let channels = [minBound..]
  coverages <- zip channels <$> traverse (Storage.coverage database) channels
  let completed :: Map Commit CommitState
      completed = foldr add mempty $ concatMap snd coverages
        where
        add (_, commit, state) acc = Map.insert commit state acc

      wanted :: [Period]
      wanted =
        [ Period from (from + realToFrac freq)
        | from <- [start, start + realToFrac freq .. end ]
        ]
        where Period start end = targetPeriod

      missing :: [(Channel, Period)]
      missing =
        [ (channel, period)
        | channel <- channels
        , Just covered <- [lookup channel coverages]
        , period <- wanted
        -- we consider an expanded period such that if there is coverage
        -- withnin this time, then the period can be considered covered.
        , not $ any (within $ expanded period) covered
        ]
        where
          expanded (Period s e) = Period (s - halfFreq) (e + halfFreq)
            where halfFreq = realToFrac freq / 2

          within (Period s e) (Period s' e',_,state) =
            s <= s' && e' <= e && state == Success

  capabilities <- getNumCapabilities
  logInfo logger $ "concurrency: " <> pretty capabilities
  parallelWriter logger database capabilities $ \save -> do
    let handled :: Commit -> Bool
        handled commit =
          maybe False isFinal $
          Map.lookup commit completed

        processPeriod :: Channel -> Period -> IO (Either String String)
        processPeriod channel period = do
          commits <- commitsWithin logger channel period
          let maxAttempts = 10
              pending = take maxAttempts $ filter (not . handled) commits
              handle commit = do
                success <- save commit
                when success $
                  Storage.writeCoverage database period channel commit
                return success
          success <- tryInSequence $ map handle pending
          return $ if success
            then Right $ unwords ["Success:", show channel,show $ pretty period]
            else Left $ unwords ["Failure:", show channel, show $ pretty period]

    forConcurrently missing (uncurry processPeriod)
  where
  commitsWithin :: Logger -> Channel -> Period -> IO [Commit]
  commitsWithin logger channel (Period _ end) = do
    r <- GitHub.commitsUntil user 30 nixpkgsRepo (channelBranch channel) end
    case r of
      Left err -> do
        logInfo logger $ "GitHub - failed to list commits: " <> pretty (show err)
        return []
      Right commits ->
        return commits

-- stops on first True
tryInSequence :: [IO Bool] -> IO Bool
tryInSequence [] = return False
tryInSequence (x:xs) = x >>= \case
  True -> return True
  False -> tryInSequence xs

isFinal :: CommitState -> Bool
isFinal = \case
  Success    -> True
  Broken     -> True
  Incomplete -> False

