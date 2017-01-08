{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
module Commands.Reddit
(
    startrek,
    wcgw,
    meme,
    randomReddit,
    wallpaper
)
where

import Servant.API
import Servant.Client
import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Monad.Random
import Data.ByteString.Char8 (ByteString, pack)
import Data.Proxy
import Data.Maybe
import Data.Monoid
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

data Reddit = Reddit { nsfw :: Bool, url :: String, title :: String }
    deriving (Show, Eq, Ord)

instance FromJSON Reddit where
    parseJSON (Array v) = do
        let Object o0 = v V.! 0
        Object o1 <- o0 .: "data"
        Array o2 <- o1 .: "children"
        let Object o3 = o2 V.! 0
        Object o4 <- o3 .: "data"
        url   <- o4 .: "url"
        nsfw  <- o4 .: "over_18"
        title <- o4 .: "title"
        return Reddit{..}
    parseJSON o@(Object _) = parseJSON (Array (V.fromList [o]))
    parseJSON _ = fail "Error parsing Reddit data!"

randomReddit :: MonadIO m 
             => Bool -> Manager -> SubReddit -> ByteString -> Response m
randomReddit showTitle man sub cmd = simpleCmd' cmd $ \_ chan -> do
    let uagent = Just "linux:tsahyt/lmrbot:v0.1.0 (by /u/tsahyt)"
    res <- liftIO $ runExceptT (query sub uagent man baseUrl)
    case res of
        Left _  -> return Nothing
        Right Reddit{..} -> 
            let ret = if nsfw then "(NSFW!) " else mempty 
                   <> pack url
                   <> if showTitle then " : " <> pack title else mempty
             in return . Just $ privmsg (fromMaybe "" chan) ret

startrek :: MonadIO m => Manager -> Response m
startrek m = randomReddit True m (SubReddit "startrekgifs") ":startrek"

wcgw :: MonadIO m => Manager -> Response m
wcgw m = randomReddit True m (SubReddit "whatcouldgowrong") ":wcgw"

meme :: MonadIO m => Manager -> Response m
meme m = randomReddit True m (SubReddit "linuxmemes") ":meme"

wallpaper :: (MonadRandom m, MonadIO m) => Manager -> m (Response m)
wallpaper m = do
    let subs = [ SubReddit "wallpapers", SubReddit "unixwallpapers"
               , SubReddit "widescreenwallpaper" ]
        bound = pred $ V.length subs
    i <- getRandomR (0, bound)
    return $ randomReddit False m (subs V.! i) ":wallpaper"