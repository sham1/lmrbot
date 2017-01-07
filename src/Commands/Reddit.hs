{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Commands.Reddit
(
    startrek,
    wcgw,
    meme,
    randomReddit,
    wallpaper,
    wallpaperWide
)
where

import Servant.API
import Servant.Client
import Control.Monad.IO.Class
import Control.Monad.Except
import Data.ByteString.Char8 (ByteString, pack)
import Data.Proxy
import Data.Maybe
import Data.Response
import Network.IRC
import Network.HTTP.Client (Manager)
import Data.Aeson
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
    parseJSON (Array v) = do
        let Object o0 = v V.! 0
        Object o1 <- o0 .: "data"
        Array o2 <- o1 .: "children"
        let Object o3 = o2 V.! 0
        Object o4 <- o3 .: "data"
        u <- o4 .: "url"
        return $ Reddit u
    parseJSON _ = error "Error parsing reddit response"

randomReddit :: MonadIO m => Manager -> SubReddit -> ByteString -> Response m
randomReddit man sub cmd = simpleCmd' cmd $ \_ chan -> do
    let uagent = Just "linux:tsahyt/lmrbot:v0.1.0 (by /u/tsahyt)"
    res <- liftIO $ runExceptT (query sub uagent man baseUrl)
    case res of
        Left _  -> return Nothing
        Right r -> return . Just $ privmsg (fromMaybe "" chan) (pack $ url r)

startrek :: MonadIO m => Manager -> Response m
startrek m = randomReddit m (SubReddit "startrekgifs") ":startrek"

wcgw :: MonadIO m => Manager -> Response m
wcgw m = randomReddit m (SubReddit "whatcouldgowrong") ":wcgw"

meme :: MonadIO m => Manager -> Response m
meme m = randomReddit m (SubReddit "linuxmemes") ":meme"

wallpaper :: MonadIO m => Manager -> Response m
wallpaper m = randomReddit m (SubReddit "wallpapers") ":wallpaper"
