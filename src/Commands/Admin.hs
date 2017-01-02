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
import Data.Monoid
import Data.ByteString.Char8 (ByteString)
import Data.Attoparsec.ByteString.Char8
import Data.Maybe
import Network.IRC
import Data.Response

joinCmd :: Monad m => BotConfig -> Response m
joinCmd r = fromMsgParser' (string ":join" *> space *> takeByteString) $ 
    \p _ c -> runMaybeT $ do
        NickName n _ _ <- MaybeT . return $ p
        guard (n == adminUser r)
        return $ joinChan c

leaveCmd :: Monad m => BotConfig -> Response m
leaveCmd r = fromMsgParser' 
    (string ":leave" *> optional (space *> takeByteString)) $ \p c c'->
    runMaybeT $ do
        NickName n _ _ <- MaybeT . return $ p
        chan <- MaybeT . return $ c
        guard (n == adminUser r)
        return . part . fromMaybe chan $ c'

mode :: BotConfig -> ByteString -> Message
mode BotConfig{..} m = Message Nothing ("MODE " <> botnick <> " " <> m) []

modeCmd :: Monad m => BotConfig -> Response m
modeCmd r = fromMsgParser'
    (string ":mode" *> space *> takeByteString) $ \p _ m ->
    runMaybeT $ do
        NickName n _ _ <- MaybeT. return $ p
        guard (n == adminUser r)
        return . mode r $ m

-- | Invite response
inviteR :: Monad m => BotConfig -> Response m
inviteR r = Response $ \m@Message{..} -> runMaybeT $ do
    NickName n _ _ <- MaybeT. return $ msg_prefix
    guard (isInvite m && n == adminUser r)
    return $ joinChan (last msg_params)

isInvite :: Message -> Bool
isInvite Message{..} = msg_command == "INVITE"
