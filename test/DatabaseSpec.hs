module DatabaseSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.Time.Calendar (Day)
import Data.Time.Clock.POSIX (POSIXTime)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)
import Test.Hspec.Expectations (HasCallStack)

import qualified Test.Hspec as Hspec
import qualified Data.HashMap.Strict as HM

import App.Storage (Database, CommitState(..))
import qualified App.Storage as Storage
import qualified App.Storage.SQLite as SQLite
import qualified App.Storage.JSON as JSON
import Nix
  ( Revision(..)
  , PackageDetails(..)
  , Channel(..)
  , RevisionPackages
  , Version(..)
  , KeyName(..)
  , PackageWithVersion(..)
  , Package(..) )
import Data.Git (Hash(..), Commit(..))
import Data.Time.Period (Period(..))
import GitHub (AuthenticatingUser(..))

channel :: Channel
channel = Nixpkgs_unstable

day :: Day
day = read "2020-03-31"

spec :: Spec
spec = do
  describe "Database" $ do
    describe "SQLite" $ run SQLite.withDatabase
    describe "JSON" $ run JSON.withDatabase
  where
    -- | Create a temporary database file and connect to it to perform tests.
    -- Erases the file after tests are finished
    run withDatabase = testDatabase $ \f ->
      withSystemTempDirectory "NIX_TEST" $ \dirPath ->
      withDatabase (dirPath <> "/Test.db") f

testDatabase :: ((Database -> IO ()) -> IO ()) -> Spec
testDatabase overDatabase = do

  let pname = Package "my-package"
      keyName = KeyName "my-package"
      fullName = PackageWithVersion "my-package"
      pkg   = PackageDetails pname (Version "1.0") keyName fullName Nothing
      time = fromIntegral 10 :: POSIXTime
      commit = Commit (Hash "hash") time
      packages = [pkg]
      revision = Revision channel commit
      period = Period time time

      write db channel commit packages = do
        Storage.writeCommitState db commit Success
        Storage.writeCoverage db period channel commit
        traverse_ (Storage.writePackage db commit) packages

  it "can save and load a revision" $ do
    overDatabase $ \db -> do
      write db channel commit packages
      v1 <- Storage.versions db channel pname
      length v1 `shouldBe` 1

  -- We can add the same thing over and over again and we won't get duplicates
  it "Adding revisions is idempotent" $ do
    overDatabase $ \db -> do
      write db channel commit packages
      v1 <- Storage.versions db channel pname

      write db channel commit packages
      v2 <- Storage.versions db channel pname

      v1 `shouldBe` v2
      length v1 `shouldBe` 1

  it "Searching a package in a channel doesn't return results from a different channel" $ do
    overDatabase $ \db -> do
      let otherPkg      = PackageDetails pname (Version "other-version") keyName fullName Nothing
          otherChannel  = succ channel
          otherCommit = Commit (Hash "otherHash") time
          otherRevision = Revision otherChannel otherCommit
          otherPackages = [otherPkg]

      write db channel commit packages
      write db otherChannel otherCommit otherPackages
      -- Even though the packages have the same name,
      -- because they point to revisions with different commits
      -- they only appear in the respective revision search
      v1 <- Storage.versions db channel pname
      v2 <- Storage.versions db otherChannel pname
      (fst <$> v1) `shouldBe` [pkg]
      (fst <$> v2) `shouldBe` [otherPkg]
      pkg `shouldNotBe` otherPkg

  it "Returns repeated package versions if commits are different" $ do
    overDatabase $ \db -> do
      let newCommit = Commit (Hash "otherHash") time

      write db channel commit packages
      write db channel newCommit packages
      r <- Storage.versions db channel pname
      length r `shouldBe` 2

  it "Returns coverage commits for target channel, but not from other channels" $ do
    overDatabase $ \db -> do
      let otherChannel = succ channel
          commit1 = Commit (Hash "1") time
          commit2 = Commit (Hash "2") time
          commit3 = Commit (Hash "3") time
          period1 = Period (fromIntegral 10) (fromIntegral 20)
          period2 = Period (fromIntegral 20) (fromIntegral 30)
          period3 = Period (fromIntegral 30) (fromIntegral 40)
          writeCoverage channel commit period state = do
            Storage.writeCommitState db commit state
            Storage.writeCoverage db period channel commit

      writeCoverage channel commit1 period1 Incomplete
      writeCoverage channel commit2 period2 Broken
      writeCoverage otherChannel commit3 period3 Success
      r <- Storage.coverage db channel
      length r `shouldBe` 2
      r `shouldBe` [(period1, commit1, Incomplete), (period2, commit2, Broken)]
