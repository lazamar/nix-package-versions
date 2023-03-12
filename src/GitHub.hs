module GitHub
  ( AuthenticatingUser(..)
  , Repository(..)
  , Failure (..)
  , archiveUrl
  , commitsUntil
  )
  where

import Control.Monad (when)
import Control.Monad.Catch (tryJust)
import Data.ByteString (ByteString)
import Data.List (partition)
import Data.Text (unpack, Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Calendar (Day)
import Data.Time.Clock (UTCTime(..), NominalDiffTime)
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Req as Req
import qualified Network.HTTP.Types.Status as HTTP (Status(..))
import Prettyprinter (Pretty(..))
import Data.Aeson (FromJSON, parseJSON, withObject, (.:), parseJSON)

import Data.Time.Period (prettyPOSIX)
import Data.Git (Commit(..), Hash(..), Branch(..))

data AuthenticatingUser = AuthenticatingUser
  { g_username :: ByteString
  , g_authToken :: ByteString
  }
  deriving (Show)

data Repository = Repository
  { g_user :: Text
  , g_repo :: Text
  }

data Failure
  = Unknown String
  | Retry NominalDiffTime
  | NotFound

instance Pretty Failure where
  pretty (Unknown str) = "Unknown: " <> pretty str
  pretty NotFound = "Not Found"
  pretty (Retry s) = "Retry (" <> pretty (show s) <> ")"

-- | Fetch a list of commits until given time.
-- Sorted oldest to newest
-- Verified commits appear earlier in the list
commitsUntil
  :: AuthenticatingUser
  -> Int -- ^ number of commits to retrieve (max 100)
  -> Repository
  -> Branch
  -> POSIXTime
  -> IO (Either Failure [Commit])
commitsUntil (AuthenticatingUser guser gtoken) count grepo (Branch branch) time = do
  when (count > 100) $ error "trying to retrieve more than max commits"
  response <-
    tryJust isHttpException
    $ fmap Req.responseBody
    $ Req.runReq Req.defaultHttpConfig
    $ Req.req Req.GET url Req.NoReqBody Req.jsonResponse options

  return $ either Left (Right . fmap g_commit . rearrange) response
  where
    -- | Order from oldest to newest and move verified commits to the top
    -- so that they may be used first.
    -- Some commits don't build successfully, the prioritisation of
    -- verified commits tries to mitigate that problem.
    rearrange :: [GitHubCommit] -> [GitHubCommit]
    rearrange = ((++) <$> fst <*> snd) . partition g_verified . reverse

    options = Req.header "User-Agent" guser
        -- TODO: use correct time here.
      <> Req.queryParam "until" (Just $ show $ prettyPOSIX time)
      <> Req.queryParam "sha" (Just branch)
      <> Req.queryParam "per_page" (Just count)
      <> Req.basicAuth guser gtoken

    url = Req.https "api.github.com"
      Req./: "repos"
      Req./: g_user grepo
      Req./: g_repo grepo
      Req./: "commits"

    isHttpException :: Req.HttpException -> Maybe Failure
    isHttpException = \case
      Req.JsonHttpException err -> Just $ Unknown err
      Req.VanillaHttpException e -> case e of
        HTTP.InvalidUrlException url' err ->
          Just $ Unknown $ "Invalid URL (" <> show url' <> "): " <> err
        HTTP.HttpExceptionRequest _request exceptionContent ->
          case exceptionContent of
            HTTP.StatusCodeException r bytes
              | 404 <- HTTP.statusCode $ HTTP.responseStatus r -> Just NotFound
              | 403 <- HTTP.statusCode $ HTTP.responseStatus r
              , Just bs <- lookup "Retry-After" (HTTP.responseHeaders r)
              , n <- read $ unpack $ decodeUtf8 bs ->
                  Just (Retry $ fromIntegral (n :: Int))
              | otherwise -> Just $ Unknown $ unwords
                [ show $ HTTP.responseStatus r
                , unpack $ decodeUtf8 bytes
                ]
            _ -> Just $ Unknown $ show exceptionContent


data GitHubCommit = GitHubCommit
  { g_verified :: Bool
  , g_commit :: Commit
  }

instance FromJSON GitHubCommit where
  parseJSON = withObject "GitHubCommit " $ \v ->
    construct
    <$> (v .: "sha")
    <*> (v .: "commit" >>= (.: "committer") >>= (.: "date"))
    <*> (v .: "commit" >>= (.: "verification") >>= (.: "verified"))
   where
     construct sha date verified =
         GitHubCommit verified $ Commit (Hash sha) (readGregorian date)

     readGregorian :: String -> POSIXTime
     readGregorian =  fromDay . read . take 10

fromDay :: Day -> POSIXTime
fromDay day = utcTimeToPOSIXSeconds $ UTCTime day 0

type Url = String

-- | Address to download a commit's content
archiveUrl :: Repository -> Commit -> Url
archiveUrl (Repository user repo) (Commit (Hash hash) _) =
  unpack $ "https://github.com/"
    <> user <> "/"
    <> repo
    <> "/archive/"
    <> hash
    <> ".tar.gz"
