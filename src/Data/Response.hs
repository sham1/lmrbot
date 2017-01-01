{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Response
(
    Response (..),
    pingR
)
where

import Control.Monad
import Network.IRC
import Debug.Trace

newtype Response = Response { respond :: Message -> Maybe Message }

pingR :: Response
pingR = Response $ \Message{..} -> do
    guard (msg_command == "PING")
    return $ pong (head msg_params)
