{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

module App.Server (run) where

import Control.Monad (mapM_, join)
import Data.Foldable (traverse_)
import Data.List (lookup)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Types (status200, status404)
import Network.Wai (Application, Request, Response, responseLBS, rawPathInfo, queryString)
import Nix.Revision (Package(..), Channel(..))
import Nix.Versions.Database (Connection)
import Nix.Versions.Types (Hash(..), Version(..), Config(..), Name(..))
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 ((!))
import Text.Read (readMaybe)

import qualified Network.Wai.Handler.Warp as Warp
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Text as Text

import qualified Nix.Versions.Database as Persistent

run :: Config -> IO ()
run config = do
    let port = 8080
    conn <- Persistent.connect (config_cacheDirectory config) (config_databaseFile config)
    putStrLn $ "Running server on port " <> show port
    Warp.run port (app conn)

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
            H.form
                ! A.action "."
                ! A.method "GET"
                $ do
                selectBox channelKey show selectedChannel [minBound..]
                H.input
                    ! A.type_ "text"
                    ! A.value (fromString $ Text.unpack $ fromMaybe ""  mSearchedPackage)
                    ! A.name (fromString pkgKey)
                    ! A.placeholder "Package name"
            createResults mPackages
    where
        pkgKey = "package"

        channelKey = "channel"

        getVersions name = (n,) <$> Persistent.versions conn selectedChannel n
            where n = Name name

        selectedChannel = fromMaybe Nixpkgs_unstable $ do
            val <- queryValueFor channelKey
            readMaybe $ Text.unpack val

        mSearchedPackage = queryValueFor pkgKey

        queryValueFor key
            = fmap decodeUtf8
            $ join
            $ lookup (fromString key)
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

        toRow (Name name) (Hash hash, Package _ (Version v) _) =
            H.tr do
                H.td $ H.text name
                H.td $ H.text v
                H.td $ H.text hash

pageNotFound :: Response
pageNotFound = responseLBS
    status404
    [("Content-Type", "text/plain")]
    "404 - Not Found"

selectBox :: Eq a => String -> (a -> String) -> a -> [a] -> H.Html
selectBox name toString selected options =
    H.select
        ! A.name (fromString name)
        $ traverse_ toOption options
    where
        toOption value =
            H.option
                ! (if value == selected then A.selected "true" else mempty)
                ! A.value (fromString $ toString value)
                $ fromString (toString value)




