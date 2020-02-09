module Version where

import Data.Monoid ((<>))
import System.Process (shell, readCreateProcess, CreateProcess(..))

searchVersions :: FilePath -> IO String
searchVersions path =
    let
        command :: String
        command =
            "git rev-list master -- "
            <> path
            <> " | parallel -q git grep -E '^\\s+version\\s?=\\s?\"[^\"]+\"\\s*;\\s*$' -- "
            <> path

        nixpkgsRepoPath = "/Users/marcelo/Projects/nixpkgs"
    in
    readCreateProcess ((shell command) { cwd = Just nixpkgsRepoPath }) ""
