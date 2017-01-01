{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Response
(
    Response (..),
    pingR,
    isPing,
    simpleCmd,
    simpleCmd',
    fromMsgParser,
    fromMsgParser',
    fromUser,
    fromAdmin,
    rateLimit,
    userLimit
)
where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Data.BotConfig
import Data.ByteString.Char8 (ByteString)
import Data.Attoparsec.ByteString.Char8
import Data.Time.Clock.POSIX
import Data.Maybe
import Network.IRC
import Debug.Trace

import qualified Data.Map as M

newtype Response m = Response { respond :: Message -> m (Maybe Message) }

-- | Respond to standard ping messages
pingR :: Applicative m => Response m
pingR = Response $ \m@Message{..} -> pure $ do
    guard (isPing m)
    return $ pong (head msg_params)

isPing :: Message -> Bool
isPing Message{..} = msg_command == "PING"

simpleCmd :: Monad m 
          => ByteString 
          -> (Maybe Prefix -> Maybe Channel -> m Message)
          -> Response m
simpleCmd x f = fromMsgParser (() <$ string x) (\x y _ -> f x y)

simpleCmd' :: Monad m 
           => ByteString 
           -> (Maybe Prefix -> Maybe Channel -> m (Maybe Message))
           -> Response m
simpleCmd' x f = fromMsgParser' (() <$ string x) (\x y _ -> f x y)
          
-- | Construct a response given a parser for the content of a PRIVMSG.
fromMsgParser :: Monad m => Parser a 
              -> (Maybe Prefix -> Maybe Channel -> a -> m Message) 
              -> Response m
fromMsgParser p f = fromMsgParser' p (\x y z -> pure <$> f x y z)

-- | Construct a response given a parser for the content of a PRIVMSG, which can
-- still fail after parsing.
fromMsgParser' :: Monad m => Parser a 
               -> (Maybe Prefix -> Maybe Channel -> a -> m (Maybe Message)) 
               -> Response m
fromMsgParser' p f = Response $ \Message{..} ->
    runMaybeT $ do
        guard (msg_command == "PRIVMSG")
        let (chan,msg) = case msg_params of
                             [c,m] -> (Just c, m)
                             [m]   -> (Nothing, m)
                             _     -> error "Invalid message format"
        x <- MaybeT $ return (either (const Nothing) Just (parseOnly p msg))
        MaybeT $ f msg_prefix chan x

fromAdmin :: BotConfig -> Message -> Bool
fromAdmin BotConfig{..} = fromUser adminUser

fromUser :: UserName -> Message -> Bool
fromUser n Message{..} =
    case msg_prefix of
        Just (NickName u _ _) -> n == u
        _ -> False

msgUser :: Message -> Maybe UserName
msgUser Message{..} =
    case msg_prefix of
        Just (NickName u _ _) -> Just u
        _ -> Nothing

-- | Rate limit a 'Response' according to values in the 'BotConfig'
rateLimit :: MonadIO m => BotConfig -> Response m -> m (Response m)
rateLimit c res = do
    mvar <- liftIO newEmptyMVar
    return . Response $ \m -> do
        res <- respond res m
        case res of
            Nothing -> return Nothing
            r -> do
                ts  <- fromMaybe 0 <$> liftIO (tryTakeMVar mvar)
                tc  <- utcTimeToPOSIXSeconds <$> liftIO getCurrentTime
                liftIO (putMVar mvar tc)
                liftIO (print ts)
                return $ if tc - ts <= rateTime c && not (fromAdmin c m)
                         then Nothing
                         else r

-- | Rate limit a 'Response' according to value in config, but for each user
-- individually, i.e. each user gets to use the command every X seconds at most.
userLimit :: MonadIO m => BotConfig -> Response m -> m (Response m)
userLimit c res = do
    mvar <- liftIO (newMVar M.empty)
    return . Response $ \m -> do
        res <- respond res m
        case res of
            Nothing -> return Nothing
            r -> do
                umap <- liftIO (takeMVar mvar)
                tc   <- utcTimeToPOSIXSeconds <$> liftIO getCurrentTime
                let u     = fromMaybe "__unknown__" $ msgUser m
                    ts    = fromMaybe 0 $ M.lookup u umap
                    umap' = M.insert u tc umap
                liftIO (putMVar mvar umap')
                return $ if tc - ts <= rateTime c && not (fromAdmin c m)
                         then Nothing
                         else r
