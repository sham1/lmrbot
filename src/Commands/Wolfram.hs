{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Commands.Wolfram
(
    wolfram,
    newManager,
    defaultManagerSettings
)
where

import Servant.API
import Servant.Client
import Control.Monad.IO.Class
import Control.Monad.Except
import Network.IRC
import Data.Response
import Data.BotConfig
import Data.Maybe
import Data.Proxy
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (ByteString, unpack, pack)
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)

newtype Query = Query String
    deriving (Eq, Show, Ord, ToHttpApiData)

parser :: Parser Query
parser = Query . unpack <$>
    (char ':' *> choice [ string "hal", string "wa" ] 
              *> skipSpace *> takeByteString)

type WolframSimple = "v1" 
                  :> "result" 
                  :> QueryParam "appid" WolframAPIKey 
                  :> QueryParam "i" Query 
                  :> Get '[PlainText] String

query :: Maybe WolframAPIKey 
      -> Maybe Query 
      -> Manager 
      -> BaseUrl 
      -> ClientM String
query = client (Proxy :: Proxy WolframSimple)

baseUrl :: BaseUrl
baseUrl = BaseUrl Http "api.wolframalpha.com" 80 ""

wolfram :: MonadIO m => Manager -> Maybe WolframAPIKey -> Response m
wolfram _ Nothing = emptyResponse
wolfram man appid = fromMsgParser parser $ \_ chan q -> do
    res <- liftIO $
        runExceptT (query appid (Just q) man baseUrl)
    return $ case res of
        Left _ -> privmsg (fromMaybe "" chan) 
                      "Error while querying WolframAlpha"
        Right r -> privmsg (fromMaybe "" chan) (pack r)
