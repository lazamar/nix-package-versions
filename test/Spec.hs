module Main where

import Data.Time.Calendar (Day)
import Test.Hspec (hspec, Spec, describe, it, shouldBe)

import qualified DatabaseSpec

main :: IO ()
main = hspec $ do
    DatabaseSpec.spec

