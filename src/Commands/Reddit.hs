{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
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

newtype SubReddit = SubReddit String
    deriving (Eq, Show, Ord, ToHttpApiData)

type RedditRandom = "r" 
                 :> Capture "subreddit" SubReddit 
                 :> "random" :> Get '[JSON] String

query :: SubReddit -> Manager -> BaseUrl -> ClientM String
query = client (Proxy :: Proxy RedditRandom)

baseUrl :: BaseUrl
baseUrl = BaseUrl Http "api.reddit.com" 80 ""

randomReddit :: MonadIO m => Manager -> SubReddit -> ByteString -> Response m
randomReddit man sub cmd = simpleCmd cmd $ \_ chan -> do
    res <- liftIO $ runExceptT (query sub man baseUrl)
    return undefined
