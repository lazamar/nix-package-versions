module GitHub
  ( AuthenticatingUser(..)
  , Repository(..)
  , archiveUrl
  , commitsUntil
  )
  where

import Control.Monad (when)
import Control.Monad.Catch (tryJust)
import Data.ByteString (ByteString)
import Data.List (partition)
import Data.Text (unpack, Text)
import Data.Time.Calendar (Day, showGregorian,)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import qualified Network.HTTP.Req as Req
import Data.Aeson (FromJSON, parseJSON, withObject, (.:), parseJSON)

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

-- | Fetch a list of commits until given time.
-- Sorted oldest to newest
-- Verified commits appear earlier in the list
commitsUntil
  :: AuthenticatingUser
  -> Int -- ^ number of commits to retrieve (max 100)
  -> Repository
  -> Branch
  -> POSIXTime
  -> IO (Either String [Commit])
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
      <> Req.queryParam "until" (Just $ showGregorian (toDay time) <> "T23:59:59Z")
      <> Req.queryParam "sha" (Just branch)
      <> Req.queryParam "per_page" (Just count)
      <> Req.basicAuth guser gtoken

    url = Req.https "api.github.com"
      Req./: "repos"
      Req./: g_user grepo
      Req./: g_repo grepo
      Req./: "commits"

    isHttpException :: Req.HttpException -> Maybe String
    isHttpException = Just . show

toDay :: POSIXTime -> Day
toDay posix = day
  where UTCTime day _ = posixSecondsToUTCTime posix

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
