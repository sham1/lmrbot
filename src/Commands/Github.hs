{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module Commands.Github
(
    github
)
where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Casing
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (pack)
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Response
import Data.Time
import GHC.Generics
import Network.HTTP.Client (Manager)
import Network.IRC (privmsg)
import Servant.API
import Servant.Client
import Text.Printf

data Repo = Repo
    { repoFullName        :: String
    , repoHtmlUrl         :: String
    , repoStargazersCount :: Int
    , repoWatchersCount   :: Int
    , repoForksCount      :: Int
    , repoPushedAt        :: UTCTime
    , repoLanguage        :: String
    , repoOpenIssues      :: Int
    }
    deriving (Eq, Show, Ord, Generic)

instance FromJSON Repo where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

type RepoInfo = Header "User-Agent" String
             :> "repos" 
             :> Capture "owner" String
             :> Capture "repo" String
             :> Get '[JSON] Repo

repoInfo :: Client RepoInfo
repoInfo = client (Proxy :: Proxy RepoInfo)

data Query = Query String String
    deriving (Eq, Show, Ord)

parser :: Parser Query
parser = Query
     <$> (string ":github" *> space *> ident)
     <*> (((() <$ char '/') <|> skipSpace) *> ident)
     where ident = many1 
                 $ satisfy (\x -> isDigit x || isAlpha_iso8859_15 x || x == '-')

baseUrl :: BaseUrl
baseUrl = BaseUrl Https "api.github.com" 443 ""

repoMessage :: Repo -> String
repoMessage Repo{..} = 
    printf fmt repoHtmlUrl (dat repoPushedAt) repoStargazersCount 
           repoWatchersCount repoOpenIssues repoForksCount repoLanguage
    where fmt = "%s - Last push on %s.\
               \ Repository has %d stars, %d watchers, %d open issues\
               \ and %d forks. Written in %s."
          dat = formatTime defaultTimeLocale "%b %d, %Y"

github :: MonadIO m => Manager -> Response m
github manager = fromMsgParser parser $ \_ chan (Query user repo) -> do
    x <- liftIO . flip runClientM (ClientEnv manager baseUrl) $ do
        r <- repoInfo (Just "tsahyt/lmrbot") user repo
        pure . pack . repoMessage $ r

    let result = case x of
                     Left _   -> "Error while searching for repository"
                     Right x' -> x'

    return $ privmsg (fromMaybe "" chan) result
