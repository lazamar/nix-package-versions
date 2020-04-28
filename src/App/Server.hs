{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

module App.Server (run, Port(..)) where

import Control.Arrow ((&&&))
import Control.Monad (mapM_, join)
import Data.Either (fromRight)
import Data.Foldable (traverse_)
import Data.List (lookup)
import Data.Maybe (fromMaybe, fromJust)
import Data.String (fromString, IsString)
import Data.Text.Encoding (decodeUtf8)
import Data.Text (Text)
import Data.Time.Calendar (Day, showGregorian)
import Network.HTTP.Types (status200, status404, renderQuery, queryTextToQuery)
import Network.Wai (Application, Request, Response, responseLBS, rawPathInfo, queryString)
import Nix.Revision (Package(..), Channel(..), GitBranch(..), channelBranch)
import Nix.Versions.Database (Connection)
import Nix.Versions.Types (Hash(..), Version(..), Config(..), Name(..))
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 ((!))
import Text.Blaze (toValue, toMarkup)

import qualified Network.Wai.Handler.Warp as Warp
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Text as Text
import qualified Skylighting as S

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
    mPackages <- traverse getVersions mSearched
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
                fromString $ S.styleToCss S.pygments

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
                        selectBox
                            channelKey
                            toChannelBranch
                            (fromMaybe Nixpkgs_unstable mSelectedChannel)
                            [minBound..]

                    H.div ! A.class_ "pure-control-group" $ do
                        H.label "Package name"
                        H.input
                            ! A.type_ "text"
                            ! A.value (fromString $ Text.unpack $ maybe "" fromName mSearchedPackage)
                            ! A.name (fromString pkgKey)
                            ! A.placeholder "Package name"

                    H.div ! A.class_ "pure-controls" $ do
                        H.input
                            ! A.type_ "submit"
                            ! A.value "Search"
                            ! A.class_ "pure-button pure-button-primary"
            createResults mPackages

            fromMaybe mempty
                $ installationInstructions <$> mSearchedPackage <*> mSelectedChannel <*> mSelectedRevision

            H.footer $ H.p
                ! A.style "text-align: center"
                $ do
                "Created by "
                H.a ! A.href "http://lazamar.github.io/" $
                    "Marcelo Lazaroni"


    where
        nixSyntax :: S.Syntax
        nixSyntax = fromJust $ S.lookupSyntax "nix" S.defaultSyntaxMap

        pkgKey, channelKey, revisionKey, instructionsAnchor :: IsString a => a
        pkgKey = "package"
        channelKey = "channel"
        revisionKey = "revision"
        instructionsAnchor = "instructions"

        getVersions (channel, name) = (channel, name,) <$> Persistent.versions conn channel name

        mSelectedChannel = do
            val <- queryValueFor channelKey
            fromChannelBranch val

        mSearchedPackage = Name <$> queryValueFor pkgKey

        mSearched = (,) <$> mSelectedChannel <*> mSearchedPackage

        mSelectedRevision = Hash <$> queryValueFor revisionKey

        queryValueFor key
            = fmap decodeUtf8
            $ join
            $ lookup (fromString key)
            $ queryString request

        createResults Nothing = mempty
        createResults (Just (channel, name, results)) =
            H.table
                ! A.class_ "pure-table-bordered pure-table"
                ! A.style "width: 100%"
                $ do
                H.thead $ H.tr do
                    H.th "Package"
                    H.th "Version"
                    H.th "Revision"
                    H.th "Date"
                H.tbody $
                    if null results
                       then H.p "No results found"
                       else mapM_ (toRow channel name) $ zip [0..] results

        toRow :: Channel -> Name -> (Int, (Package, Hash, Day)) -> H.Html
        toRow channel name (ix, (Package _ (Version v) _, hash, day)) =
            H.tr
                ! (if odd ix then A.class_ "pure-table-odd" else mempty)
                $ do
                H.td $ H.text $ fromName name
                H.td $ H.text v
                H.td $ H.a
                    ! A.href (toValue $ revisionLink channel name hash)
                    ! A.title "Click for installation instructions"
                    $ H.text $ fromHash hash
                H.td $ toMarkup $ showGregorian day

        revisionLink :: Channel -> Name -> Hash -> Text.Text
        revisionLink  channel (Name name) (Hash hash) =
            "./" <> query <> "#" <> instructionsAnchor
            where
                query
                    = decodeUtf8
                    $ renderQuery True
                    $ queryTextToQuery
                        [ (pkgKey, Just name)
                        , (revisionKey, Just hash)
                        , (channelKey, Just $ toChannelBranch channel)
                        ]

        installationInstructions :: Name -> Channel -> Hash -> H.Html
        installationInstructions (Name name) channel hash =
            H.div
                ! A.id instructionsAnchor
                $ do
                H.h2 $ toMarkup $ "Install " <> name
                H.table ! A.class_ "pure-table" $ do
                    H.tr do
                        H.th "Package"
                        H.td $ toMarkup name
                    H.tr do
                        H.th "Channel"
                        H.td $ toMarkup $ toChannelBranch channel
                    H.tr do
                        H.th "Revision"
                        H.td $ toMarkup $ fromHash hash

                H.p $ "Install " <> H.code (toMarkup name) <> " with " <> H.code "nix-env" <> "."
                nixCodeBlock $ "nix-env -qaP ghc -f " <> revisionURL hash

                H.p $ "Use " <> H.code (toMarkup name) <> " in a " <> H.code "nix-shell" <> "."
                nixCodeBlock $ "nix-shell -p bat -I nixpkgs=" <> revisionURL hash

                H.p $ "Use " <> H.code (toMarkup name) <> " in a nix script"
                nixCodeBlock $ Text.unlines
                        [ "let"
                        , "     pkgs = import (builtins.fetchGit {"
                        , "         # Descriptive name to make the store path easier to identify                "
                        , "         name = \"my-old-revision\";                                                 "
                        , "         url = \"https://github.com/nixos/nixpkgs-channels/\";                       "
                        , "         ref = \"refs/heads/" <> toChannelBranch channel <> "\";                     "
                        , "         rev = \""<> fromHash hash <> "\";                                           "
                        , "     }) {};                                                                           "
                        , ""
                        , "     myPkg = pkgs." <> name
                        , "in"
                        , "..."
                        ]

        nixCodeBlock :: Text -> H.Html
        nixCodeBlock
            = S.formatHtmlBlock S.defaultFormatOpts
            . fromRight []
            . S.tokenize (S.TokenizerConfig S.defaultSyntaxMap False) nixSyntax

pageNotFound :: Response
pageNotFound = responseLBS
    status404
    [("Content-Type", "text/plain")]
    "404 - Not Found"

selectBox :: Eq a => Text -> (a -> Text) -> a -> [a] -> H.Html
selectBox name toString selected options =
    H.select
        ! A.name (toValue name)
        $ traverse_ toOption options
    where
        toOption value =
            H.option
                ! (if value == selected then A.selected "true" else mempty)
                ! A.value (toValue $ toString value)
                $ toMarkup (toString value)

fromChannelBranch :: Text -> Maybe Channel
fromChannelBranch = flip lookup $ (toChannelBranch &&& id) <$> [minBound..]

toChannelBranch :: Channel -> Text
toChannelBranch = fromGitBranch . channelBranch

revisionURL :: Hash -> Text
revisionURL (Hash hash) = "https://github.com/NixOS/nixpkgs-channels/archive/" <> hash <> ".tar.gz"

