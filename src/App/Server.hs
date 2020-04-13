{-# LANGUAGE OverloadedStrings #-}

module App.Server (run) where

import Control.Monad (mapM_, join)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (pack)
import Data.Text.Encoding (decodeUtf8)
import Data.List (lookup)
import Network.HTTP.Types (status200, status404)
import Network.Wai (Application, Request, Response, responseLBS, rawPathInfo, queryString)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 ((!))
import Nix.Versions.Types (Config(..), Name(..))
import Nix.Versions.Database (Connection)

import qualified Network.Wai.Handler.Warp as Warp
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Nix.Versions.Database as Persistent

run :: Config -> IO ()
run config = do
    conn <- Persistent.connect (config_cacheDirectory config) (config_databaseFile config)
    putStrLn $ "http://localhost:8080/"
    Warp.run 8080 (app conn)

app :: Connection -> Application
app conn request respond = do
    response <- case rawPathInfo request of
        "/"     -> pageHome conn request
        _       -> return pageNotFound
    respond response

pageHome :: Connection -> Request -> IO Response
pageHome conn request = do
    mPackages <- traverse (Persistent.versions conn . Name) mSearchedPackage
    return $ responseLBS status200 [("Content-Type", "text/html")] $ renderHtml $
        H.docTypeHtml $ do
            H.form
                ! A.action "/"
                ! A.method "GET"
                $ do
                H.input
                    ! A.type_ "text"
                    ! A.name (fromString pkgKey)
                    ! A.placeholder "Package name"
            createResults mPackages
    where
        pkgKey = "package"

        mSearchedPackage
            = fmap decodeUtf8
            $ join
            $ lookup (fromString pkgKey)
            $ queryString request

        createResults Nothing        = mempty
        createResults (Just [])      = H.p "No results to found"
        createResults (Just results) =
            H.div $ mapM_ (H.p . fromString . show) results



pageNotFound :: Response
pageNotFound = responseLBS
    status404
    [("Content-Type", "text/plain")]
    "404 - Not Found"
