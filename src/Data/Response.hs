{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Response
(
    Response (..),
    pingR,
    isPing,
    simpleCmd,
    fromMsgParser,
    fromMsgParser'
)
where

import Control.Monad
import Control.Monad.Trans.Maybe
import Data.ByteString.Char8 (ByteString)
import Data.Attoparsec.ByteString.Char8
import Network.IRC
import Debug.Trace

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
