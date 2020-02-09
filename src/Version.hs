module Version where

import Data.Monoid ((<>))
import System.Process (shell, readCreateProcess, CreateProcess(..))

searchVersions :: FilePath -> IO [String]
searchVersions path = do
    let
        command :: String
        command =
            "git rev-list master -- "
            <> path
            <> " | parallel -q git grep -E '^\\s+version\\s?=\\s?\"[^\"]+\"\\s*;\\s*$' {} -- "
            <> path

        nixpkgsRepoPath = "/Users/marcelo/Projects/nixpkgs"
    out <- readCreateProcess ((shell command) { cwd = Just nixpkgsRepoPath }) ""
    return $ lines out
