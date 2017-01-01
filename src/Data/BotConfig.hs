{-# LANGUAGE OverloadedStrings #-}
module Data.BotConfig
(
    BotConfig (..),
    defaultConfig
)
where

import Network.Socket
import Network.IRC

data BotConfig = BotConfig
    { server  :: HostName
    , service :: ServiceName
    , chans   :: [Channel]
    , botnick :: UserName
    }

defaultConfig :: BotConfig
defaultConfig = BotConfig
    { server  = "irc.snoonet.org"
    , service = "6667"
    , chans   = ["#tsahyt-bot-test"]
    , botnick = "anActualBotnet"
    }
