{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Pipes.Network
(
    network,
    fromHandleLine,
    toHandleLine,
    register,
    joins,
    parseIRC,
    filterJust,
    inbound,
    outbound
)
where

import Control.Monad
import Data.BotConfig
import Data.ByteString.Char8 (ByteString)
import Data.Maybe
import Data.Monoid
import Network
import Network.IRC
import Pipes
import Pipes.ByteString (stdout)
import System.IO (Handle, hSetBuffering, BufferMode (..), hIsEOF)
import qualified Pipes.Prelude as P
import qualified Data.ByteString.Char8 as B

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

register, joins :: Monad m => BotConfig -> Producer Message m ()
register BotConfig{..} = do
    yield $ user botnick "0" "*" "bot"
    yield $ nick botnick

joins BotConfig {..} = mapM_ (yield . joinChan) chans

parseIRC :: Monad m => Pipe ByteString Message m ()
parseIRC = P.map decode >-> filterJust

filterJust :: Monad m => Pipe (Maybe a) a m ()
filterJust = P.filter isJust >-> P.map fromJust

inbound, outbound :: MonadIO m => Consumer' ByteString m ()
inbound = P.map (\x -> "<-- " <> x <> "\r\n") >-> stdout
outbound = P.map (\x -> "--> " <> x <> "\r\n") >-> stdout
