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
import Network.IRC
import Data.Response
import Data.BotConfig
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Data.List (intersperse)
import GHC.Generics
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (ByteString, unpack, pack)
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
    , primary   :: Maybe Bool
    }
    deriving (Eq, Show, Ord, Generic)

instance FromJSON Pod

getPrimaryPods :: WAResult -> [Pod]
getPrimaryPods (WAResult q) = filter ((== Just True) . primary) (pods q)

constructPodAnswer :: Pod -> ByteString
constructPodAnswer p = case subpods p of
    Nothing -> maybe "" pack (plaintext p)
    Just xs -> mconcat . intersperse "; " . map constructPodAnswer $ xs

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
    let u   = fromMaybe "Dave" $ prefixUser <$> p
        env = ClientEnv man baseUrl
    res <- liftIO $ flip runClientM env $ shortAnswer appid (Just q)
    case res of
        Left _ -> do
            res' <- liftIO $ flip runClientM env $ 
                        longAnswer appid (Just q) (Just "json") 
                            (Just "plaintext") Nothing
            case res' of
                Right r -> let ret = mconcat 
                                   . map constructPodAnswer . getPrimaryPods $ r
                            in return $ privmsg (fromMaybe "" chan) ret
                _ -> return $ privmsg (fromMaybe "" chan) $
                    "I'm sorry " <> u <> ", I'm afraid I can't do that."
        Right r -> return $ privmsg (fromMaybe "" chan) (pack r)
