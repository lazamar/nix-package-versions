module DatabaseSpec (spec) where

import Control.Exception (bracket)
import Data.Time.Calendar (Day(ModifiedJulianDay))
import Nix.Revision (Revision(..), Package(..), Channel(..))
import Nix.Versions.Types (DBFile(..), CachePath(..), Hash(..), Version(..), Name(..), Commit(..))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)
import qualified Data.HashMap.Strict as HM
import qualified Nix.Versions.Database as P


-- | Create a temporary database file and connect to it to perform tests.
-- Erases the file after tests are finished
overDatabase :: (P.Connection -> IO a) -> IO a
overDatabase f =
    withSystemTempDirectory "NIX_TEST" $ \dirPath ->
        bracket (P.connect (CachePath dirPath) (DBFile "Test.db")) P.disconnect f

defaultChannel :: Channel
defaultChannel = Nixpkgs_unstable

day :: Day
day = read "2020-03-31"

spec :: Spec
spec = do
    describe "Database" $ do
        let pname = Name "my-package"
            pkg   = Package pname Nothing (Version "1.0") Nothing
            commit = Commit (Hash "hash") (ModifiedJulianDay 10)
            packages = HM.fromList [(pname, pkg)]
            revision = Revision defaultChannel  commit

        it "can save and load a revision" $ do
            overDatabase $ \conn -> do
                () <- P.saveRevisionWithPackages conn day revision packages
                v1 <- P.versions conn defaultChannel  pname
                length v1 `shouldBe` 1

        -- We can add the same thing over and over again and we won't get duplicates
        it "Adding revisions is idempotent" $ do
            overDatabase $ \conn -> do
                () <- P.saveRevisionWithPackages conn day revision packages
                v1 <- P.versions conn defaultChannel pname
                () <- P.saveRevisionWithPackages conn day revision packages
                v2 <- P.versions conn defaultChannel pname
                v1 `shouldBe` v2
                length v1 `shouldBe` 1

        it "Searching a package in a channel doesn't return results from a different channel" $ do
            overDatabase $ \conn -> do
                let otherPkg      = Package pname Nothing (Version "other-version") Nothing
                    otherChannel  = succ defaultChannel
                    otherCommit = Commit (Hash "otherHash") (ModifiedJulianDay 10)
                    otherRevision = Revision otherChannel otherCommit
                    otherPackages = HM.fromList [(pname, otherPkg)]

                -- Even though the packages have the same name,
                -- because they point to revisions with different commits
                -- they only appear in the respective revision search
                () <- P.saveRevisionWithPackages conn day revision packages
                () <- P.saveRevisionWithPackages conn day otherRevision otherPackages
                v1 <- P.versions conn defaultChannel pname
                v2 <- P.versions conn otherChannel   pname
                (getPackage <$> v1) `shouldBe` [pkg]
                (getPackage <$> v2) `shouldBe` [otherPkg]
                pkg `shouldNotBe` otherPkg

        it "When inserting the same package version with a more recent revision, the record is overriden" $ do
            overDatabase $ \conn -> do
                let newDay = succ day
                    Commit hash _ = commit

                () <- P.saveRevisionWithPackages conn day revision packages
                v1 <- P.versions conn defaultChannel pname
                () <- P.saveRevisionWithPackages conn newDay revision packages
                v2 <- P.versions conn defaultChannel pname
                v1 `shouldBe` [(pkg, hash, day)]
                v2 `shouldBe` [(pkg, hash, newDay)]

        it "When inserting the same package version with an older revision, the record is not overriden" $ do
            overDatabase $ \conn -> do
                let newDay = succ day
                    Commit hash _ = commit

                () <- P.saveRevisionWithPackages conn newDay revision packages
                v1 <- P.versions conn defaultChannel pname
                () <- P.saveRevisionWithPackages conn day revision packages
                v2 <- P.versions conn defaultChannel pname
                v1 `shouldBe` [(pkg, hash, newDay)]
                v2 `shouldBe` [(pkg, hash, newDay)]

getPackage :: (Package, Hash, Day) -> Package
getPackage (p,_,_) = p





