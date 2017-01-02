{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Commands.Admin
(
    joinCmd,
    leaveCmd,
    modeCmd,
    inviteR,
    isInvite,
)
where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.BotConfig
import Data.Attoparsec.ByteString.Char8
import Data.Maybe
import Network.IRC
import Data.Response

joinCmd :: Monad m => BotConfig -> Response m
joinCmd r = fromMsgParser' (string ":join" *> space *> takeByteString) $ 
    \p _ c -> runMaybeT $ do
        guard (fromAdmin' r p)
        return $ joinChan c

leaveCmd :: Monad m => BotConfig -> Response m
leaveCmd r = fromMsgParser' 
    (string ":leave" *> optional (space *> takeByteString)) $ \p c c'->
    runMaybeT $ do
        chan <- MaybeT . return $ c
        guard (fromAdmin' r p)
        return . part . fromMaybe chan $ c'

modeCmd :: Monad m => BotConfig -> Response m
modeCmd r = fromMsgParser'
    (string ":mode" *> space *> takeByteString) $ \p _ m ->
    runMaybeT $ do
        guard (fromAdmin' r p)
        return . mode r $ m

-- | Invite response
inviteR :: Monad m => BotConfig -> Response m
inviteR r = Response $ \m@Message{..} -> runMaybeT $ do
    guard (isInvite m && fromAdmin' r msg_prefix)
    return $ joinChan (last msg_params)

isInvite :: Message -> Bool
isInvite Message{..} = msg_command == "INVITE"
