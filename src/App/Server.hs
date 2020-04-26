{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

module App.Server (run, Port(..)) where

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

newtype Port = Port Int
    deriving (Show, Eq, Read)

run :: Port -> Config -> IO ()
run (Port port) config = do
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
        H.head do
            H.link
                ! A.rel "stylesheet"
                ! A.href "https://unpkg.com/purecss@1.0.1/build/pure-min.css"
            H.meta
                ! A.name "viewport"
                ! A.content "width=device-width, initial-scale=1"
            H.style do
                "section { max-width: 768px; margin: auto }"

        H.body $ H.section do
            H.h1 "Nix package versions"
            H.p  "Find all versions of a package that were available in a channel and the revision you can download it from."
            H.form
                ! A.action "."
                ! A.method "GET"
                ! A.class_ "pure-form pure-form-aligned"
                $ do
                H.fieldset $ do
                    H.div ! A.class_ "pure-control-group" $ do
                        H.label "Nix channel"
                        selectBox channelKey show selectedChannel [minBound..]

                    H.div ! A.class_ "pure-control-group" $ do
                        H.label "Package name"
                        H.input
                            ! A.type_ "text"
                            ! A.value (fromString $ Text.unpack $ fromMaybe ""  mSearchedPackage)
                            ! A.name (fromString pkgKey)
                            ! A.placeholder "Package name"

                    H.div ! A.class_ "pure-controls" $ do
                        H.input
                            ! A.type_ "submit"
                            ! A.value "Search"
                            ! A.class_ "pure-button pure-button-primary"
            createResults mPackages

            H.h2 "Downloading older package versions"
            H.pre $
                fromString $ unlines
                    [ "import (builtins.fetchGit {                                                     "
                    , "    # Descriptive name to make the store path easier to identify                "
                    , "    name = \"nixos-unstable-2018-09-12\";                                       "
                    , "    url = \"https://github.com/nixos/nixpkgs-channels/\";                       "
                    , "    # Commit hash for nixos-unstable as of 2018-09-12                           "
                    , "    # `git ls-remote https://github.com/nixos/nixpkgs-channels nixos-unstable`  "
                    , "    ref = \"refs/heads/nixos-unstable\";                                        "
                    , "    rev = \"ca2ba44cab47767c8127d1c8633e2b581644eb8f\";                         "
                    , "}) {}                                                                           "
                    ]

            H.p
                ! A.style "text-align: center"
                $ do
                "Created by "
                H.a ! A.href "http://lazamar.github.io/" $
                    "Marcelo Lazaroni"


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
            H.table
                ! A.class_ "mq-table pure-table-bordered pure-table"
                ! A.style "width: 100%"
                $ do
                H.thead $ H.tr do
                    H.th "Attribute Name"
                    H.th "Version"
                    H.th "Revision"
                H.tbody $
                    if null results
                       then H.p "No results found"
                       else mapM_ (toRow name) $ zip [0..] results

        toRow (Name name) (ix, (Package _ (Version v) _, Hash hash, _)) =
            H.tr
                ! (if odd ix then A.class_ "pure-table-odd" else mempty)
                $ do
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




