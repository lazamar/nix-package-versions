{-# LANGUAGE OverloadedStrings #-}

module App.Server (run) where

import Network.HTTP.Types (status200, status404)
import Network.Wai (Application, Response, responseLBS, rawPathInfo)

import qualified Network.Wai.Handler.Warp as Warp

run :: IO ()
run = do
    putStrLn $ "http://localhost:8080/"
    Warp.run 8080 app

app :: Application
app request respond = respond $ case rawPathInfo request of
    "/"     -> pageHome
    _       -> pageNotFound

pageHome :: Response
pageHome = responseLBS status200 [("Content-Type", "text/plain")]
    "Hello World"

pageNotFound :: Response
pageNotFound = responseLBS
    status404
    [("Content-Type", "text/plain")]
    "404 - Not Found"
