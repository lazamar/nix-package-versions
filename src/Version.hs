module Version where

import System.Process (shell, readCreateProcess, CreateProcess(..))

searchVersions :: FilePath -> IO String
searchVersions path =
    readCreateProcess ((shell "ls") { cwd = Just "/etc/" }) ""
