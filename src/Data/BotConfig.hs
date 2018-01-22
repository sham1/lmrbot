{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.BotConfig
(
    BotConfig (..),
    WolframAPIKey (..),
    defaultConfig,
    readConfig
)
where

import Network
import Network.IRC
import Data.Time.Clock
import Data.Yaml
import Data.HostMask
import Data.ByteString.Char8 (ByteString, pack)
import Servant.API
import GHC.Generics

newtype WolframAPIKey = AppId String
    deriving (Eq, Show, Ord, ToHttpApiData, Generic)

instance FromJSON WolframAPIKey

data BotConfig = BotConfig
    { server     :: HostName
    , service    :: PortID
    , chans      :: [Channel]
    , banChans   :: [Channel]
    , botnick    :: UserName
    , botpwd     :: Maybe ByteString
    , adminUsers :: [HostMask]
    , rateTime   :: NominalDiffTime
    , rateChans  :: [Channel]
    , umodes     :: [ByteString]
    , wolframAPI :: Maybe WolframAPIKey
    , ignored    :: [HostMask]
    , silent     :: Bool
    }
    deriving (Show, Eq, Generic)

instance FromJSON PortID where
    parseJSON (Number s) = 
        let x :: Int = truncate s 
         in return . PortNumber . fromIntegral $ x
    parseJSON _ = fail "type error in parsing PortID"

instance FromJSON ByteString where
    parseJSON = fmap pack . parseJSON

instance FromJSON BotConfig

defaultConfig :: BotConfig
defaultConfig = BotConfig
    { server     = "irc.freenode.net"
    , service    = PortNumber 6667
    , chans      = []
    , banChans   = []
    , botnick    = "lmrbot"
    , botpwd     = Nothing
    , adminUsers = []
    , rateTime   = 300
    , rateChans  = []
    , umodes     = ["+B"]
    , wolframAPI = Nothing
    , ignored    = []
    , silent     = True
    }

readConfig :: FilePath -> IO (Maybe BotConfig)
readConfig = decodeFile
