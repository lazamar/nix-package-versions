module Nix.Versions
    ( saveRevisionsForPeriod
    ) where

import Control.Concurrent.Async (mapConcurrently)
import Data.Time.Calendar (Day, showGregorian)
import Data.Either (partitionEithers)
import Data.Aeson (encodeFile, eitherDecodeFileStrict)

import qualified Nix.Revision as Revision

saveRevisionsForPeriod :: Day -> Day -> IO ()
saveRevisionsForPeriod from to = undefined
    --do
    --(revisionFailures, revisionFiles) <-
        --partitionEithers <$> Revision.downloadVersionsInPeriod from to

    --(jsonFailures, jsons) <-
        --partitionEithers <$> mapConcurrently Revision.load (fst <$> revisionFiles)

    --let database = foldMap DB.create jsons
    --encodeFile (databasePath from to) database
    --return database

databasePath :: Day -> Day -> FilePath
databasePath from to =
    "./saved-versions/database-" <> showGregorian from <> "-" <> showGregorian to
