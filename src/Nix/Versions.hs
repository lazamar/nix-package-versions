module Nix.Versions
    ( saveDatabase
    , loadDatabase
    ) where

import Control.Concurrent.Async (mapConcurrently)
import Data.Time.Calendar (Day, showGregorian)
import Data.Either (partitionEithers)
import Nix.Versions.Database (PackageDB)
import Data.Aeson (encodeFile, eitherDecodeFileStrict)

import qualified Nix.Versions.Database as DB
import qualified Nix.Versions.Json as Json

createDatabase :: Day -> Day -> IO PackageDB
createDatabase from to = do
    (revisionFailures, revisionFiles) <-
        partitionEithers <$> Json.downloadVersionsInPeriod from to

    (jsonFailures, jsons) <-
        partitionEithers <$> mapConcurrently Json.load (fst <$> revisionFiles)

    let database = foldMap DB.create jsons
    encodeFile (databasePath from to) database
    return database

saveDatabase :: Day -> Day -> PackageDB -> IO ()
saveDatabase from to database =
    encodeFile (databasePath from to) database

loadDatabase :: Day -> Day -> IO (Either String PackageDB)
loadDatabase from to =
    eitherDecodeFileStrict (databasePath from to)

databasePath :: Day -> Day -> FilePath
databasePath from to =
    "./saved-versions/database-" <> showGregorian from <> "-" <> showGregorian to
