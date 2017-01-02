{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.BotConfig
(
    BotConfig (..),
    defaultConfig,
    readConfig
)
where

import Network
import Network.IRC
import Data.Time.Clock
import Data.Yaml
import Data.ByteString.Char8 (ByteString, pack)
import GHC.Generics

data BotConfig = BotConfig
    { server     :: HostName
    , service    :: PortID
    , chans      :: [Channel]
    , botnick    :: UserName
    , botpwd     :: Maybe ByteString
    , adminUsers :: [UserName]
    , rateTime   :: NominalDiffTime
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
    { server     = "irc.snoonet.org"
    , service    = PortNumber 6667
    , chans      = ["#linuxmasterrace"]
    , botnick    = "anActualBotnet"
    , botpwd     = Nothing
    , adminUsers = ["tsahyt"]
    , rateTime   = 300
    }

readConfig :: FilePath -> IO (Maybe BotConfig)
readConfig = decodeFile
