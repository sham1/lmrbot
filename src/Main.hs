{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.BotConfig
import Data.ByteString.Char8 (ByteString)
import Data.Maybe
import Data.Response
import Network
import Network.IRC
import Pipes
import Pipes.ByteString (stdout)
import qualified Data.ByteString.Char8 as B
import qualified Pipes.Prelude as P
import System.IO (Handle, hSetBuffering, BufferMode (..), hIsEOF)

network :: MonadIO m => BotConfig -> m Handle
network BotConfig{..} = liftIO $ do
    h <- connectTo server service
    hSetBuffering h NoBuffering
    return h

fromHandleLine :: MonadIO m => Handle -> Producer ByteString m ()
fromHandleLine h = do
    eof <- liftIO $ hIsEOF h
    unless eof $ do
        x <- liftIO $ B.hGetLine h
        yield x
        fromHandleLine h

toHandleLine :: MonadIO m => Handle -> Consumer ByteString m ()
toHandleLine h = do
    x <- await
    liftIO (B.hPutStrLn h x)
    toHandleLine h

register BotConfig{..} = do
    yield $ user botnick "0" "*" "bot"
    yield $ nick botnick

joins BotConfig {..} = mapM_ (yield . joinChan) chans

parseIRC :: Monad m => Pipe ByteString Message m ()
parseIRC = P.map decode >-> filterJust

filterJust :: Monad m => Pipe (Maybe a) a m ()
filterJust = P.filter isJust >-> P.map fromJust

response :: Monad m => Pipe Message ByteString m ()
response = P.map go >-> filterJust >-> P.map encode
    where go :: Message -> Maybe Message
          go m = listToMaybe . mapMaybe (`respond` m) $ rsps
          rsps = [ pingR ]

inbound, outbound :: MonadIO m => Consumer' ByteString m ()
inbound = P.map (flip B.append "\r\n" . B.append "<-- ") >-> stdout
outbound = P.map (flip B.append "\r\n" . B.append "--> ") >-> stdout

main :: IO ()
main = do
    h <- network defaultConfig
    let up   = fromHandleLine h
        down = toHandleLine h

    -- bootstrap commands for nick and initial join
    runEffect $ up >-> P.take 2 >-> inbound
    runEffect $ 
        register defaultConfig >-> P.map encode >-> P.tee outbound >-> down

    -- bot loop
    runEffect $ 
        up >-> P.tee inbound >-> parseIRC >-> response 
           >-> P.tee outbound >-> down
