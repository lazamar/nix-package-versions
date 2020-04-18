{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

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
import Nix.Versions.Types (Hash(..), Version(..), Config(..), Name(..))
import Nix.Versions.Database (Connection)
import Nix.Revision (Package(..))

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
    mPackages <- traverse getVersions mSearchedPackage
    return $ responseLBS status200 [("Content-Type", "text/html")] $ renderHtml $
        H.docTypeHtml do
        H.body do
            H.form ! A.action "." ! A.method "GET" $ do
                H.input
                    ! A.type_ "text"
                    ! A.name (fromString pkgKey)
                    ! A.placeholder "Package name"
            createResults mPackages
    where
        getVersions name = (n,) <$> Persistent.versions conn n
            where n = Name name

        pkgKey = "package"

        mSearchedPackage
            = fmap decodeUtf8
            $ join
            $ lookup (fromString pkgKey)
            $ queryString request

        createResults Nothing                 = mempty
        createResults (Just (name, results)) =
            H.table ! A.class_ "table" $ do
                H.thead $ H.tr do
                    H.th "Attribute Name"
                    H.th "Version"
                    H.th "Revision"
                H.tbody $
                    if null results
                       then H.p "No results to found"
                       else mapM_ (toRow name) results

        toRow (Name name) (Hash hash, Package description (Version v) _) =
            H.tr do
                H.td $ H.text name
                H.td $ H.text v
                H.td $ H.text hash

pageNotFound :: Response
pageNotFound = responseLBS
    status404
    [("Content-Type", "text/plain")]
    "404 - Not Found"
