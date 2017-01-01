{-# LANGUAGE OverloadedStrings #-}
module Data.BotConfig
(
    BotConfig (..),
    defaultConfig
)
where

import Network
import Network.IRC
import Data.Time.Clock

data BotConfig = BotConfig
    { server    :: HostName
    , service   :: PortID
    , chans     :: [Channel]
    , botnick   :: UserName
    , adminUser :: UserName
    , rateTime  :: NominalDiffTime
    }

defaultConfig :: BotConfig
defaultConfig = BotConfig
    { server    = "irc.snoonet.org"
    , service   = PortNumber 6667
    , chans     = ["#linuxmasterrace"]
    , botnick   = "anActualBotnet"
    , adminUser = "tsahyt"
    , rateTime  = 300
    }
