module DatabaseSpec (spec) where

import Control.Exception (bracket)
import Control.Exception (evaluate)
import Data.Text (pack)
import Data.Time.Calendar (Day(ModifiedJulianDay))
import Nix.Revision (Revision(..), Package(..), Channel(..))
import Nix.Versions.Types (DBFile(..), CachePath(..), Hash(..), Version(..), Name(..), Commit(..))
import System.IO.Temp (withSystemTempDirectory)
import System.IO.Temp (withSystemTempFile)
import System.Posix.Files (removeLink)
import Test.Hspec (Spec, describe, it, shouldBe)
import qualified Data.HashMap.Strict as HM
import qualified Nix.Versions.Database as P


-- | Create a temporary database file and connect to it to perform tests.
-- Erases the file after tests are finished
overDatabase :: (P.Connection -> IO a) -> IO a
overDatabase f =
    withSystemTempDirectory "NIX_TEST" $ \dirPath ->
        bracket (P.connect (CachePath dirPath) (DBFile "Test.db")) P.disconnect f

day :: Day
day = read "2020-03-31"

spec :: Spec
spec = do
    describe "Database" $ do
        let pname = Name "my-package"
            pkg   = Package Nothing (Version "1.0") Nothing
            commit = Commit (Hash "hash") (ModifiedJulianDay 10)
            packages = HM.fromList [(pname, pkg)]
            revision = Revision Nixpkgs_unstable commit

        it "can save and load a revision" $ do
            overDatabase $ \conn -> do
                () <- P.save conn day revision packages
                v1 <- P.versions conn pname
                length v1 `shouldBe` 1

        -- We can add the same thing over and over again and we won't get duplicates
        it "Adding revisions is idempotent" $ do
            overDatabase $ \conn -> do
                () <- P.save conn day revision packages
                v1 <- P.versions conn pname
                () <- P.save conn day revision packages
                v2 <- P.versions conn pname
                v1 `shouldBe` v2
                length v1 `shouldBe` 1





