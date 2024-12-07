{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Server (run, Port(..)) where

import Control.Arrow ((&&&))
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Either (fromRight)
import Data.List (sortOn)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Map as Map
import Data.String (fromString, IsString)
import Data.Text.Encoding (decodeUtf8)
import Data.Text (Text)
import Data.Time.Calendar (Day, showGregorian)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Network.HTTP.Types (status200, status404, renderQuery, queryTextToQuery)
import Network.Wai (Application, Request, Response, requestHeaders, responseLBS, rawPathInfo, queryString)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 ((!))
import Text.Blaze (toValue, toMarkup)
import System.IO (hPutStrLn, stderr)

import qualified Network.Wai.Handler.Warp as Warp
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Text as Text
import qualified Skylighting as S

import Data.Git (Hash(..), Branch(..), Commit(..))
import Nix
  ( PackageDetails(..)
  , Channel(..)
  , channelBranch
  , Version(..)
  , Package(..)
  , KeyName(..)
  , PackageWithVersion(..))
import App.Storage (Database)
import qualified App.Storage as Storage

newtype Port = Port Int
    deriving (Show, Eq, Read)

run :: Database -> Port -> IO ()
run database (Port port) = do
  hPutStrLn stderr $ "Running server on port " <> show port
  Warp.run port $ app database

app :: Database -> Application
app database request respond = do
  response <- case rawPathInfo request of
    "/" -> pageHome database request
    _   -> return pageNotFound
  respond response

pageHome :: MonadIO m => Database -> Request -> m Response
pageHome database request = do
    mPackages <- traverse getVersions mSearched
    return $ responseLBS status200 [("Content-Type", "text/html")] $ renderHtml $
        H.docTypeHtml do
        H.head do
            if lookup "DNT" (requestHeaders request) /= Just "1" &&
               lookup "Sec-GPC" (requestHeaders request) /= Just "1"
            then analytics
            else return ()
            H.title "Nix Package Versions"
            H.link
                ! A.rel "shortcut icon"
                ! A.href favicon
            H.link
                ! A.rel "icon"
                ! A.type_ "image/webp"
                ! A.href favicon
            H.link
                ! A.rel "apple-touch-icon"
                ! A.type_ "image/webp"
                ! A.href favicon
            H.link
                ! A.rel "stylesheet"
                ! A.href "https://unpkg.com/purecss@1.0.1/build/pure-min.css"
            H.meta
                ! A.name "viewport"
                ! A.content "width=device-width, initial-scale=1"
            H.style do
                "body {                             "
                "    margin: 0;                     "
                "    min-height: 100vh;             "
                "    display: flex;                 "
                "    flex-flow: column;             "
                "    align-content: space-between;  "
                "}                                  "
                "section { max-width: 1000px; margin: 0 auto auto; padding: 1em }"
                "div.sourceCode {                   "
                "    background-color: #EEE;        "
                "    padding: 1em;                  "
                "    scrollbar-width: none;         "
                "}                                  "
                fromString $ S.styleToCss S.pygments
        H.body $ do
            H.section do
                H.h1 $ H.a
                    ! A.href "."
                    ! A.style "text-decoration: none; color: inherit"
                    $ "Nix package versions"
                H.p  "Find all versions of a package that were available in a channel and the revision you can download it from."
                H.p  "Click on the revision number for installation instructions."
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
                                ! A.value (fromString $ Text.unpack $ maybe "" unPackage mSearchedPackage)
                                ! A.name (fromString pkgKey)
                                ! A.placeholder "Package name"

                        H.div ! A.class_ "pure-controls" $ do
                            H.input
                                ! A.type_ "submit"
                                ! A.value "Search"
                                ! A.class_ "pure-button pure-button-primary"
                createResults mPackages

                fromMaybe mempty
                    $ installationInstructions
                        <$> mSearchedPackage
                        <*> mSelectedVersion
                        <*> mSelectedKeyName
                        <*> mSelectedChannel
                        <*> mSelectedRevision

                H.h2 "Motivation and Method"
                H.p do
                    "Installing older versions of packages in Nix is easy but currently there is no official way to find out what "
                    "revision has the package version that I need. This page provides this functionality by letting you see what "
                    "versions were available in the past, when they were available, what revision to install them from, and what command to use."

                H.p do
                    "The list of versions available here is not exhaustive. To check what versions of packages were ever available data "
                    "was collected from past revisions in 5 weeks intervals. This means that if a package changed versions more often "
                    "than every 5 weeks there may be versions missing."

                H.p do
                    "During the retrieval of version information, revisions for some periods could not be successfully built for "
                    "some channels and this also causes versions to be missing from the list."

            H.footer
                ! A.style (toValue $ unwords
                    [ "text-align: center;"
                    , "padding: 10px;"
                    , "background-color: #f2f2f2;"
                    , "font-size: .8em;"
                    ])
                $ H.p do
                "Created by "
                H.a ! A.href "http://lazamar.github.io/" $
                    "Marcelo Lazaroni"
                ". Check it on "
                H.a ! A.href "https://github.com/lazamar/nix-package-versions" $
                    "GitHub"


    where
        nixSyntax :: S.Syntax
        nixSyntax = fromJust $ S.lookupSyntax "nix" S.defaultSyntaxMap

        versionKey, keyNameKey, fullNameKey, pkgKey, channelKey, revisionKey, instructionsAnchor :: IsString a => a
        versionKey = "version"
        keyNameKey = "keyName"
        fullNameKey = "fullName"
        pkgKey = "package"
        channelKey = "channel"
        revisionKey = "revision"
        instructionsAnchor = "instructions"

        getVersions (channel, pkg) = liftIO $ do
          versions <- Storage.versions database channel pkg
          return (channel, sortOn (commitDate . snd) $ dedupe versions)

        commitDate (Commit _ time) = time

        mSelectedChannel = do
            val <- queryValueFor channelKey
            fromChannelBranch val

        mSearchedPackage :: Maybe Package
        mSearchedPackage = Package <$> queryValueFor pkgKey

        mSearched :: Maybe (Channel, Package)
        mSearched = (,) <$> mSelectedChannel <*> mSearchedPackage

        mSelectedRevision = Hash <$> queryValueFor revisionKey
        mSelectedKeyName = KeyName <$> queryValueFor keyNameKey
        mSelectedVersion = Version <$> queryValueFor versionKey

        queryValueFor key
            = fmap decodeUtf8
            $ join
            $ lookup (fromString key)
            $ queryString request

        createResults Nothing = mempty
        createResults (Just (channel,results)) =
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
                       else mapM_ (toRow channel) $ zip [0..] results

        toRow :: Channel -> (Int, (PackageDetails, Commit)) -> H.Html
        toRow channel (ix, (package, Commit hash time)) =
            H.tr
                ! (if odd ix then A.class_ "pure-table-odd" else mempty)
                $ do
                H.td $ H.text $ unPackage $ name package
                H.td $ H.text $ unVersion $ version package
                H.td $ H.a
                    ! A.href (toValue $ revisionLink channel hash package)
                    ! A.title "Click for installation instructions"
                    $ H.text $ fromHash hash
                H.td $ toMarkup $ showGregorian $ toDay time

        revisionLink :: Channel -> Hash -> PackageDetails -> Text.Text
        revisionLink  channel (Hash hash) PackageDetails{name,version,fullName,keyName} =
            "./" <> query <> "#" <> instructionsAnchor
            where
                query
                    = decodeUtf8
                    $ renderQuery True
                    $ queryTextToQuery
                        [ (pkgKey, Just $ unPackage name)
                        , (versionKey, Just $ unVersion version)
                        , (fullNameKey, Just $ unPackageWithVersion fullName)
                        , (keyNameKey, Just $ fromKeyName keyName)
                        , (revisionKey, Just hash)
                        , (channelKey, Just $ toChannelBranch channel)
                        ]

        installationInstructions :: Package -> Version -> KeyName -> Channel -> Hash -> H.Html
        installationInstructions (Package name) (Version version) (KeyName keyName) channel hash =
            H.div
                ! A.id instructionsAnchor
                $ do
                H.h2 $ toMarkup $ "Install " <> name
                H.table
                    ! A.class_ "pure-table"
                    ! A.style "margin-bottom: 1em" $ do
                    H.tr do
                        H.th "Package"
                        H.td $ toMarkup name
                    H.tr do
                        H.th "Version"
                        H.td $ toMarkup version
                    H.tr do
                        H.th "Channel"
                        H.td $ toMarkup $ toChannelBranch channel
                    H.tr do
                        H.th "Revision"
                        H.td $ toMarkup $ fromHash hash

                H.hr

                H.p $ "Install " <> H.code (toMarkup name) <> " with " <> H.b (H.code "nix-env") <> "."
                nixCodeBlock $ "nix-env -iA " <> keyName <> " -f " <> revisionURL hash

                H.hr

                H.p $ "Use " <> H.code (toMarkup keyName) <> " in a " <> H.b (H.code "nix-shell") <> "."
                nixCodeBlock $ "nix-shell -p " <> keyName <> " -I nixpkgs=" <> revisionURL hash

                H.hr

                H.p $ "Use " <> H.code (toMarkup keyName) <> " with " <> H.b (H.code "nix shell") <> "."
                nixCodeBlock $ "nix shell github:nixos/nixpkgs/" <> fromHash hash <> "#" <> keyName

                H.hr

                H.p $ "Install " <> H.code (toMarkup keyName) <> " with " <> H.b (H.code "nix profile") <> "."
                nixCodeBlock $ "nix profile install nixpkgs/" <> fromHash hash <> "#" <> keyName

                H.hr

                H.p $ "Use " <> H.code (toMarkup name) <> " in a nix script via tarball"
                nixCodeBlock $ Text.unlines
                        [ "let"
                        , "    pkgs = import (builtins.fetchTarball {"
                        , "        url = \"" <> revisionURL hash <> "\";"
                        , "    }) {};"
                        , ""
                        , "    myPkg = pkgs." <> keyName <> ";"
                        , "in"
                        , "..."
                        ]

                H.hr

                H.p $ "Use " <> H.code (toMarkup name) <> " in a nix script via Git"
                nixCodeBlock $ Text.unlines
                        [ "let"
                        , "     pkgs = import (builtins.fetchGit {"
                        , "         # Descriptive name to make the store path easier to identify"
                        , "         name = \"my-old-revision\";"
                        , "         url = \"https://github.com/NixOS/nixpkgs/\";"
                        , "         ref = \"refs/heads/" <> toChannelBranch channel <> "\";"
                        , "         rev = \""<> fromHash hash <> "\";"
                        , "     }) {};"
                        , ""
                        , "     myPkg = pkgs." <> keyName <> ";"
                        , "in"
                        , "..."
                        ]

        nixCodeBlock :: Text -> H.Html
        nixCodeBlock
            = S.formatHtmlBlock S.defaultFormatOpts
            . fromRight []
            . S.tokenize (S.TokenizerConfig S.defaultSyntaxMap False) nixSyntax

favicon :: H.AttributeValue
favicon = "data:image/webp;base64,UklGRmACAABXRUJQVlA4TFQCAAAvD8ADEE/loI0kR3LP5XsED+75c4hNw2HbtoFEp70b4Cf6/We61g4iSZKU7LkXDGAFTfiP+JmBGwBgoujiCe7uLh09GzAEFYOwAoswAq3TuTu8u5wGpHZA8gCv/x2d76WyjJ/2uplZ1N6NLutZSciT+p4sIpkxeSNy5OnfP23b/tek9btMf3yPTo0+WT7bme77yM+st6hJYosl+cYjI0cZsAMnUQkDBAYFILCGgwY7OCCCYMAwg8AKE6JwtG0gAUSZExIMxpiJY9LGtr4fx95XzRb8nl/r/f3I724izL9nVGn39zPK/I5QAm79im7kL2KVZ+j5u/Ufb/QZ8axNYJF+2/uzwX1tTRO0TMEktW0vFozCW5CFmuYXHmFDWZjSGDOQU/aAIxIEOCxgSMNWIyMDVBTmq7CaOPwRDAwSAFHwBDXosEUMUCAAYFAAoMBBixUQHABBwCTfv/ZHO7P/nUWoT1LscP21uxnSjq1oS9LdujWAAJLo25tqRM0t9/ms34AgSbJpq59t29Y337dt2zbet23b/uv0EiL6PwH4z1CTGsNbg6drfd/5jMXUj9fn8eXzL47gCH++UlG+OH8DtxPwxtvMhVyRpHammx4VBHiys6oq05nkmpa7TGsYLt4JPa33TUDHQ11Sotc1NjE13dYJAA1pefrY6NHA0kIF5QtLwC8qidk9eLoslXIAkkzMINK0GNi4lgszGruqU1KvzvaB45FPNi+5vrl9dn3z5RDY2YOqQFf2eDsXGDrZBgCNwZ8Ql/s2uXKEb3MsIeH2SPSu9n/3Rw=="

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
toChannelBranch = unBranch . channelBranch

revisionURL :: Hash -> Text
revisionURL (Hash hash) = "https://github.com/NixOS/nixpkgs/archive/" <> hash <> ".tar.gz"

analytics :: H.Html
analytics = do
    H.script
        ! A.async "true"
        ! A.src "https://www.googletagmanager.com/gtag/js?id=UA-68439335-6"
        $ ""
    H.script do
        "window.dataLayer = window.dataLayer || []; "
        "function gtag(){dataLayer.push(arguments);}"
        "gtag('js', new Date());                    "
        "gtag('config', 'UA-68439335-6');           "

dedupe :: [(PackageDetails, Commit)] -> [(PackageDetails, Commit)]
dedupe xs = Map.elems $ Map.fromListWith newest
  [ (fullName pkg, (pkg, commit))
  | (pkg, commit) <- xs
  ]
  where
  newest a@(_, Commit _ t1) b@(_, Commit _ t2) =
    if t1 > t2
    then a
    else b

toDay :: POSIXTime -> Day
toDay posix = day
  where UTCTime day _ = posixSecondsToUTCTime posix


