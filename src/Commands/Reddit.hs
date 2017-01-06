{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Commands.Reddit
(
    randomReddit,
    newManager,
    tlsManagerSettings
)
where

import Servant.API
import Servant.Client
import Control.Monad.IO.Class
import Control.Monad.Except
import Data.ByteString.Char8 (ByteString)
import Data.Proxy
import Data.Response
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS
import Data.Yaml
import qualified Data.Vector as V

newtype SubReddit = SubReddit String
    deriving (Eq, Show, Ord, ToHttpApiData)

type RedditRandom = "r" 
                 :> Capture "subreddit" SubReddit 
                 :> Header "User-Agent" String
                 :> "random" :> Get '[JSON] Reddit

query :: SubReddit -> Maybe String -> Manager -> BaseUrl -> ClientM Reddit
query = client (Proxy :: Proxy RedditRandom)

baseUrl :: BaseUrl
baseUrl = BaseUrl Http "api.reddit.com" 80 ""

data Reddit = Reddit { url :: String }
    deriving (Show, Eq, Ord)

instance FromJSON Reddit where
    parseJSON (Array v) = parseJSON (v V.! 0)
    parseJSON (Object v) = do
        --link <- v .: "link" 
        url  <- v .: "url"
        return $ Reddit url
    parseJSON _ = error "Malformed reddit response"

randomReddit :: MonadIO m => Manager -> SubReddit -> ByteString -> Response m
randomReddit man sub cmd = simpleCmd cmd $ \_ chan -> do
    let uagent = Just "linux:tsahyt/lmrbot:v0.1.0 (by /u/tsahyt)"
    res <- liftIO $ runExceptT (query sub uagent man baseUrl)
    return undefined
