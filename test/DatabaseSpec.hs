module DatabaseSpec (spec) where

import Control.Exception (evaluate)
import Nix.Revision (Revision(..), Package(..))
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
        it "Adding revisions is idempotent" $ do
            overDatabase $ \conn -> do
                let pname = Name "my-package"
                    pkg   = Package Nothing (Version "1.0") Nothing
                    commit = Commit (Hash "hash") (ModifiedJulianDay 10)
                    revision = Revision commit $ HM.fromList [(pname, pkg)]

                () <- P.save conn revision
                v1 <- P.versions conn pname
                () <- P.save conn revision
                v2 <- P.versions conn pname
                v1 `shouldBe` v2
                length v1 `shouldBe` 1





