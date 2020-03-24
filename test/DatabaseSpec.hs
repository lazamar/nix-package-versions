module DatabaseSpec (spec) where

import Control.Exception (evaluate)
import Nix.Versions.Database (PackageDB(..), VersionInfo(..))
import Nix.Versions.Types (Hash(..), Version(..), Name(..), Commit(..))
import System.IO.Temp (withSystemTempFile)
import Test.Hspec (Spec, describe, it, shouldBe)
import qualified Data.HashMap.Strict as HM
import qualified Nix.Versions.Database.Persistent as P
import Data.Text (pack)
import Data.Time.Calendar (Day(ModifiedJulianDay))
import Control.Exception (bracket)


-- | Create a temporary database file and connect to it to perform tests.
-- Erases the file after tests are finished
overDatabase :: (P.Connection -> IO a) -> IO a
overDatabase f =
    withSystemTempFile "NIX_TEST_DB" $ \filePath _ ->
        bracket (P.connect $ pack filePath) P.disconnect f

spec :: Spec
spec = do
    describe "Database" $ do
        -- We can add the same thing over and over again and we won't get duplicates
        it "Adding a package name is idempotent" $ do
            overDatabase $ \conn -> do
                let pname = Name "my-package"
                    vinfo = VersionInfo (Hash "hash") Nothing Nothing (ModifiedJulianDay 10)
                    db = PackageDB $ HM.fromList [(pname, HM.fromList [(Version "1.0", vinfo )])]

                () <- P.persist conn db
                v1 <- P.versions conn pname
                () <- P.persist conn db
                v2 <- P.versions conn pname
                v1 `shouldBe` v2





