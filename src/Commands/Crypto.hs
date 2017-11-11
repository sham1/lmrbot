{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
module Commands.Crypto (
    crypto
) where

import Control.Monad.IO.Class
import Network.HTTP.Client (Manager)
import Numeric
import Data.Response
import Data.Maybe (fromMaybe)
import Control.Applicative
import Data.Aeson
import Data.Proxy
import Data.Attoparsec.ByteString.Char8 hiding (take)
import Data.ByteString.Char8 (pack, unpack, ByteString)
import Servant.API
import GHC.Generics
import Network.IRC
import Servant.Client

data TickerData = TickerData
    { name :: String
    , symbol :: String
    , price_usd :: String
    , price_btc :: String
    , market_cap_usd :: Maybe String
    , percent_change_1h :: String
    , percent_change_24h :: String
    } deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON TickerData
instance FromJSON TickerData

type Ticker
     = "v1" :> "ticker" :> Capture "currency" String :> Get '[JSON] [TickerData]

ticker :: Client Ticker
ticker = client (Proxy :: Proxy Ticker)

data Coin
    = BTC
    | ETH
    | BCH
    | BTG
    | XRP
    | LTC
    | DASH
    | ETC
    | XMR
    | MIOTA
    | ZEC
    | OtherCoin String
    deriving (Show, Eq, Read, Ord)

coinName :: Coin -> String
coinName BTC = "bitcoin"
coinName ETH = "ethereum"
coinName BCH = "bitcoin-cash"
coinName BTG = "bitcoin-gold"
coinName XRP = "ripple"
coinName LTC = "litecoin"
coinName DASH = "dash"
coinName ETC = "ethereum-classic"
coinName XMR = "monero"
coinName MIOTA = "iota"
coinName ZEC = "zcash"
coinName (OtherCoin x) = x

coinParse :: Parser Coin
coinParse =
    choice
        [ ETH <$ choice (map string ["eth", "Ethereum", "ETH", "ethereum"])
        , BCH <$
          choice (map string ["bch", "Bitcoin Cash", "BCH", "bitcoin cash"])
        , BTG <$
          choice (map string ["btg", "Bitcoin Gold", "BTG", "bitcoin gold"])
        , BTC <$ choice (map string ["btc", "Bitcoin", "BTC", "bitcoin"])
        , XRP <$ choice (map string ["xrp", "Ripple", "XRP", "ripple"])
        , LTC <$ choice (map string ["ltc", "Litecoin", "LTC", "litecoin"])
        , DASH <$ choice (map string ["dash", "DASH", "Dash"])
        , ETC <$
          choice
              (map string ["etc", "Ethereum Classic", "ETC", "ethereum classic"])
        , XMR <$ choice (map string ["xmr", "Monero", "XMR", "monero"])
        , MIOTA <$ choice (map string ["miota", "Iota", "MIOTA", "iota"])
        , ZEC <$ choice (map string ["zec", "ZCash", "Zcash", "ZEC", "zcash"])
        , OtherCoin . unpack <$> takeByteString
        ]

parser :: Parser Coin
parser =
    (string ":cc" <|> string ":crypto" <|> string ":coin") *> space *> coinParse

baseUrl :: BaseUrl
baseUrl = BaseUrl Https "api.coinmarketcap.com" 443 ""

tickerMessage :: TickerData -> ByteString
tickerMessage td =
    pack . unwords $
    [ name td
    , "(" ++ symbol td ++ ") |"
    , showFFloatAlt (Just 2) (read @Double $ price_usd td) " USD"
    , showFFloatAlt (Just 8) (read @Double $ price_btc td) " BTC"
    , "| Change:"
    , showFFloatAlt (Just 2) (read @Double $ percent_change_1h td) "% (1h)"
    , showFFloatAlt (Just 2) (read @Double $ percent_change_24h td) "% (24h)"
    , maybe
          ""
          (\x -> "| Market Cap: " ++ 
                showFFloatAlt (Just 2) (read @Double x) " USD")
          (market_cap_usd td)
    ]

crypto :: MonadIO m => Manager -> Response m
crypto manager = fromMsgParser parser $ \_ chan coin -> do
    x <- liftIO . flip runClientM (ClientEnv manager baseUrl) $
        tickerMessage . head <$> ticker (coinName coin)

    let result = case x of
                    Left _ -> "Error while querying coinmarketcap.com"
                    Right x' -> x'

    return $ privmsg (fromMaybe "" chan) result
