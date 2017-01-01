{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Response
(
    Response (..),
    pingR,
    isPing
)
where

import Control.Monad
import Network.IRC
import Debug.Trace

newtype Response = Response { respond :: Message -> Maybe Message }

pingR :: Response
pingR = Response $ \m@Message{..} -> do
    guard (isPing m)
    return $ pong (head msg_params)

isPing :: Message -> Bool
isPing Message{..} = msg_command == "PING"
