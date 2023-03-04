module DatabaseSpec (spec) where

import Control.Monad.SQL (MonadSQLT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Log2 (runLoggerT, discard)
import Data.Time.Calendar (Day(ModifiedJulianDay))
import Nix.Revision (Revision(..), Package(..), Channel(..), RevisionPackages)
import Nix.Versions.Types (DBFile(..), CachePath(..), Hash(..), Version(..), KeyName(..), FullName(..), Name(..), Commit(..), GitHubUser(..), Config(..))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations (HasCallStack)

import qualified Test.Hspec as Hspec
import qualified Data.HashMap.Strict as HM
import qualified Nix.Versions.Database as P


-- | Create a temporary database file and connect to it to perform tests.
-- Erases the file after tests are finished
overDatabase :: MonadSQLT IO a -> IO a
overDatabase f =
    withSystemTempDirectory "NIX_TEST" $ \dirPath ->
        P.withConnection (CachePath dirPath) (DBFile "Test.db") f

defaultChannel :: Channel
defaultChannel = Nixpkgs_unstable

day :: Day
day = read "2020-03-31"

shouldBe :: (MonadIO m, HasCallStack, Show a, Eq a) => a -> a -> m ()
shouldBe a b = liftIO $ Hspec.shouldBe a b

shouldNotBe  :: (MonadIO m, HasCallStack, Show a, Eq a) => a -> a -> m ()
shouldNotBe a b = liftIO $ Hspec.shouldNotBe a b

spec :: Spec
spec = do
    describe "Database" $ do
        let pname = Name "my-package"
            keyName = KeyName "my-package"
            fullName = FullName "my-package"
            pkg   = Package pname (Version "1.0") keyName fullName Nothing
            commit = Commit (Hash "hash") (ModifiedJulianDay 10)
            packages = [pkg]
            revision = Revision defaultChannel  commit

        it "can save and load a revision" $ do
            overDatabase $ runLoggerT discard $ do
                () <- P.saveRevisionWithPackages day revision packages
                v1 <- P.versions defaultChannel  pname
                liftIO $ length v1 `shouldBe` 1

        -- We can add the same thing over and over again and we won't get duplicates
        it "Adding revisions is idempotent" $ do
            overDatabase $ runLoggerT discard $ do
                () <- P.saveRevisionWithPackages day revision packages
                v1 <- P.versions defaultChannel pname
                () <- P.saveRevisionWithPackages day revision packages
                v2 <- P.versions defaultChannel pname
                v1 `shouldBe` v2
                length v1 `shouldBe` 1

        it "Searching a package in a channel doesn't return results from a different channel" $ do
            overDatabase $ runLoggerT discard $ do
                let otherPkg      = Package pname (Version "other-version") keyName fullName Nothing
                    otherChannel  = succ defaultChannel
                    otherCommit = Commit (Hash "otherHash") (ModifiedJulianDay 10)
                    otherRevision = Revision otherChannel otherCommit
                    otherPackages = [otherPkg]

                -- Even though the packages have the same name,
                -- because they point to revisions with different commits
                -- they only appear in the respective revision search
                () <- P.saveRevisionWithPackages day revision packages
                () <- P.saveRevisionWithPackages day otherRevision otherPackages
                v1 <- P.versions defaultChannel pname
                v2 <- P.versions otherChannel   pname
                (getPackage <$> v1) `shouldBe` [pkg]
                (getPackage <$> v2) `shouldBe` [otherPkg]
                pkg `shouldNotBe` otherPkg

        it "When inserting the same package version with a more recent revision, the record is overriden" $ do
            overDatabase $ runLoggerT discard $ do
                let newDay = succ day
                    Commit hash _ = commit

                () <- P.saveRevisionWithPackages day revision packages
                v1 <- P.versions defaultChannel pname
                () <- P.saveRevisionWithPackages newDay revision packages
                v2 <- P.versions defaultChannel pname
                v1 `shouldBe` [(pkg, hash, day)]
                v2 `shouldBe` [(pkg, hash, newDay)]

        it "When inserting the same package version with an older revision, the record is not overriden" $ do
            overDatabase $ runLoggerT discard $ do
                let newDay = succ day
                    Commit hash _ = commit

                () <- P.saveRevisionWithPackages newDay revision packages
                v1 <- P.versions defaultChannel pname
                () <- P.saveRevisionWithPackages day revision packages
                v2 <- P.versions defaultChannel pname
                v1 `shouldBe` [(pkg, hash, newDay)]
                v2 `shouldBe` [(pkg, hash, newDay)]

getPackage :: (Package, Hash, Day) -> Package
getPackage (p,_,_) = p





