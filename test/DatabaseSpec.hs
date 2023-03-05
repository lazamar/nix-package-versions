module DatabaseSpec (spec) where

import Data.Time.Calendar (Day(ModifiedJulianDay))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)
import Test.Hspec.Expectations (HasCallStack)

import qualified Test.Hspec as Hspec
import qualified Data.HashMap.Strict as HM

import App.Storage (Database)
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
  , FullName(..)
  , Name(..) )
import Data.Git (Hash(..), Commit(..))
import GitHub (AuthenticatingUser(..))

defaultChannel :: Channel
defaultChannel = Nixpkgs_unstable

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
  let pname = Name "my-package"
      keyName = KeyName "my-package"
      fullName = FullName "my-package"
      pkg   = PackageDetails pname (Version "1.0") keyName fullName Nothing
      commit = Commit (Hash "hash") (ModifiedJulianDay 10)
      packages = [pkg]
      revision = Revision defaultChannel  commit

  it "can save and load a revision" $ do
    overDatabase $ \db -> do
        () <- Storage.writePackages db day revision packages
        v1 <- Storage.versions db defaultChannel  pname
        length v1 `shouldBe` 1

  -- We can add the same thing over and over again and we won't get duplicates
  it "Adding revisions is idempotent" $ do
    overDatabase $ \db -> do
        () <- Storage.writePackages db day revision packages
        v1 <- Storage.versions db defaultChannel pname
        () <- Storage.writePackages db day revision packages
        v2 <- Storage.versions db defaultChannel pname
        v1 `shouldBe` v2
        length v1 `shouldBe` 1

  it "Searching a package in a channel doesn't return results from a different channel" $ do
    overDatabase $ \db -> do
        let otherPkg      = PackageDetails pname (Version "other-version") keyName fullName Nothing
            otherChannel  = succ defaultChannel
            otherCommit = Commit (Hash "otherHash") (ModifiedJulianDay 10)
            otherRevision = Revision otherChannel otherCommit
            otherPackages = [otherPkg]

        -- Even though the packages have the same name,
        -- because they point to revisions with different commits
        -- they only appear in the respective revision search
        () <- Storage.writePackages db day revision packages
        () <- Storage.writePackages db day otherRevision otherPackages
        v1 <- Storage.versions db defaultChannel pname
        v2 <- Storage.versions db otherChannel   pname
        (getPackage <$> v1) `shouldBe` [pkg]
        (getPackage <$> v2) `shouldBe` [otherPkg]
        pkg `shouldNotBe` otherPkg

  it "When inserting the same package version with a more recent revision, the record is overriden" $ do
    overDatabase $ \db -> do
        let newDay = succ day
            Commit hash _ = commit

        () <- Storage.writePackages db day revision packages
        v1 <- Storage.versions db defaultChannel pname
        () <- Storage.writePackages db newDay revision packages
        v2 <- Storage.versions db defaultChannel pname
        v1 `shouldBe` [(pkg, hash, day)]
        v2 `shouldBe` [(pkg, hash, newDay)]

  it "When inserting the same package version with an older revision, the record is not overriden" $ do
    overDatabase $ \db -> do
        let newDay = succ day
            Commit hash _ = commit

        () <- Storage.writePackages db newDay revision packages
        v1 <- Storage.versions db defaultChannel pname
        () <- Storage.writePackages db day revision packages
        v2 <- Storage.versions db defaultChannel pname
        v1 `shouldBe` [(pkg, hash, newDay)]
        v2 `shouldBe` [(pkg, hash, newDay)]

getPackage :: (PackageDetails, Hash, Day) -> Package
getPackage (p,_,_) = p





