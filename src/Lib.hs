module Lib
    ( someFunc
    ) where

import Version (searchVersions)

someFunc :: IO ()
someFunc = do
    putStrLn "Type a package to find its versions: "
    package <- getLine
    version <- searchVersions package
    sequence $ fmap putStrLn version
    return ()
