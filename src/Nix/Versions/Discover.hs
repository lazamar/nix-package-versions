{-# LANGUAGE TupleSections #-}

{-| This module is responsible for discovering what old versions of a package
   were available, and in what nixpkgs commit can they be found.
-}

module Nix.Versions.Discover
    ( allVersionsOf
    ) where

import Control.Applicative ((<|>))
import Control.Monad (join)
import Data.Bifunctor (first, second)
import Data.Either (rights, partitionEithers)
import Data.Foldable (fold)
import Data.HashMap.Strict (HashMap)
import Data.List (intersperse)
import Data.Maybe (mapMaybe)
import Data.Text (pack, unpack, Text)
import System.Process (shell, readCreateProcess, CreateProcess(..))
import Text.Parsec (ParseError, parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char, spaces, string)

import qualified Nix.Versions.Parsers as Parsers
import Nix.Versions.Types (Name(..), Version(..), Hash(..))
import Data.HashMap.Strict as HashMap

allVersionsOf :: Name -> FilePath -> IO ( [ParseFailure], HashMap Version Hash )
allVersionsOf name path = do
    commits <- pathCommits path
    versionsInfo <- join <$> mapM getVersionInfo commits
    return $ second HashMap.fromList $ partitionEithers versionsInfo
    where
        getVersionInfo :: Hash -> IO [Either ParseFailure (Version, Hash)]
        getVersionInfo hash = do
            versions <- versionMention path hash
            -- All versions will point to the same hash
            return $ second (,hash) <$> versions



-- | All commits that modified that path
pathCommits :: FilePath -> IO [Hash]
pathCommits path = do
    out <- runInNixPkgs command
    return $ rights $ parseCommitHash <$> lines out
    where
        command = "git rev-list master -- " <> path

        parseCommitHash :: String -> Either ParseError Hash
        parseCommitHash = parse Parsers.hash "Commit Hash"

-- | The parse error and the string that caused it.
type ParseFailure = (ParseError, String)

-- | Given a path and a commit, check whether the modifications introduced
-- by that commit changed the package version. If it did, return the
-- version found.
versionMention :: FilePath -> Hash -> IO [Either ParseFailure Version]
versionMention path (Hash hash) = do
    out <- runInNixPkgs command
    return $ parseVersion <$> lines out
    where
        command = "git grep -E '^\\s+version\\s?=\\s?\"[^\"]+\"\\s*;\\s*$' "
                <> unpack hash
                <> " -- "
                <> path
                <> " || true" -- Prevent the command from failing if no match was found

        parseVersion :: String -> Either (ParseError, String) Version
        parseVersion str = first (,str) $ parse parser "Package Version" str

        -- Extracts the version from a string that looks like this:
        --  203623dd6188c0b3ec4a53:pkgs/development/compilers/purescript/purescript/default.nix:  version = "0.13.5";
        parser :: Parser Version
        parser
            =  Parsers.hash
            *> char ':'
            *> Parsers.filePath
            *> char ':'
            *> spaces
            *> string "version"
            *> spaces
            *> char '='
            *> spaces
            *> Parsers.inQuotes Parsers.version


-- | Run a shell command in the nixpkgs repo
runInNixPkgs :: String -> IO String
runInNixPkgs command = readCreateProcess ((shell command) { cwd = Just nixpkgsRepo }) stdin
    where
        stdin = ""

-- | Path to local clone of nixpkgs repo
nixpkgsRepo :: FilePath
nixpkgsRepo = "../nixpkgs"

