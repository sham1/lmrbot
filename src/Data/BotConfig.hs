{-# LANGUAGE OverloadedStrings #-}
module Data.BotConfig
(
    BotConfig (..),
    defaultConfig
)
where

import Network
import Network.IRC

data BotConfig = BotConfig
    { server  :: HostName
    , service :: PortID
    , chans   :: [Channel]
    , botnick :: UserName
    }

defaultConfig :: BotConfig
defaultConfig = BotConfig
    { server  = "irc.snoonet.org"
    , service = PortNumber 6667
    , chans   = ["#tsahyt-bot-test"]
    , botnick = "anActualBotnet"
    }
