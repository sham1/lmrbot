{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
module Commands.Wolfram
(
    WAResult (..),
    QueryResult (..),
    Pod (..),
    wolfram
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
import Data.Monoid
import Data.Proxy
import GHC.Generics
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (unpack, pack)
import Network.HTTP.Client (Manager)
import Data.Aeson

newtype Query = Query String
    deriving (Eq, Show, Ord, ToHttpApiData)

parser :: Parser Query
parser = Query . unpack <$>
    (char ':' *> choice [ string "hal", string "wa" ] 
              *> space *> skipSpace *> takeByteString)

type WolframShort = "v1" 
                 :> "result" 
                 :> QueryParam "appid" WolframAPIKey 
                 :> QueryParam "i" Query 
                 :> Get '[PlainText] String

data WAResult = WAResult { queryresult :: QueryResult }
    deriving (Eq, Show, Ord, Generic)

instance FromJSON WAResult

data QueryResult = QueryResult
    { success :: Bool
    , pods :: [Pod]
    }
    deriving (Eq, Show, Ord, Generic)

instance FromJSON QueryResult

data Pod = Pod
    { title     :: String
    , subpods   :: Maybe [Pod]
    , plaintext :: Maybe String
    }
    deriving (Eq, Show, Ord, Generic)

instance FromJSON Pod

type WolframFull = "v2"
                :> "query"
                :> QueryParam "appid" WolframAPIKey
                :> QueryParam "input" Query
                :> QueryParam "output" String
                :> QueryParam "format" String
                :> QueryParam "includepodid" String
                :> Get '[PlainText] WAResult

instance MimeUnrender PlainText WAResult where
    mimeUnrender _ = eitherDecode

type WolframAPI = WolframShort :<|> WolframFull

shortAnswer :: Client WolframShort
longAnswer :: Client WolframFull
shortAnswer :<|> longAnswer = client (Proxy :: Proxy WolframAPI)

baseUrl :: BaseUrl
baseUrl = BaseUrl Http "api.wolframalpha.com" 80 ""

wolfram :: MonadIO m => Manager -> Maybe WolframAPIKey -> Response m
wolfram _ Nothing = emptyResponse
wolfram man appid = fromMsgParser parser $ \p chan q -> do
    let u = fromMaybe "Dave" $ msgUser' p
    res <- liftIO $
        runExceptT (shortAnswer appid (Just q) man baseUrl)
    case res of
        Left _ -> do
            res' <- liftIO $ runExceptT (longAnswer appid (Just q) 
                                                          (Just "json") 
                                                          (Just "plaintext") 
                                                          (Just "Result") 
                                                          man baseUrl)
            case res' of
                Right (WAResult (QueryResult _ 
                    [Pod _ (Just [Pod _ _ (Just r)]) _])) -> 
                    return $ privmsg (fromMaybe "" chan) (pack r)
                _ -> return $ privmsg (fromMaybe "" chan) $
                    "I'm sorry " <> u <> ", I'm afraid I can't do that."
        Right r -> return $ privmsg (fromMaybe "" chan) (pack r)
