{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.BotConfig
import Pipes.Network.TCP
import Pipes
import Data.Maybe
import qualified Pipes.Prelude as P
import Pipes.ByteString
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Network.IRC -- (decode, Message (..), encode, nick, joinChan)

startSocket :: BotConfig -> IO Socket
startSocket BotConfig{..} = fst <$> connectSock server service

register BotConfig{..} = do
    yield $ nick botnick
    yield $ user botnick "0" "*" "bot"
    mapM_ (yield . joinChan) chans

parseIRC :: Monad m => Pipe ByteString Message m ()
parseIRC = P.map decode >-> filterJust

filterJust :: Monad m => Pipe (Maybe a) a m ()
filterJust = P.filter isJust >-> P.map fromJust

response :: Monad m => Pipe Message ByteString m ()
response = P.map go >-> filterJust >-> P.map encode
    where go = Just

inbound, outbound :: MonadIO m => Consumer' ByteString m ()
inbound = P.map (B.append "<-- ") >-> stdout
outbound = P.map (flip B.append "\n" . B.append "--> ") >-> stdout

main :: IO ()
main = do
    s <- startSocket defaultConfig
    let up   = fromSocket s 1024
        down = toSocket s
    -- bootstrap commands for nick and initial join
    runEffect $ 
        register defaultConfig >-> P.map encode >-> P.tee outbound >-> down

    -- bot loop
    runEffect $ 
        up >-> P.tee inbound >-> parseIRC >-> response >-> P.drain
