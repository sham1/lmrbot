{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Commands.Admin
(
    joinCmd,
    leaveCmd,
    nickCmd,
    modeCmd,
    inviteR,
    isInvite,
    say,
    source
)
where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.BotConfig
import Data.ByteString.Char8 (ByteString, cons)
import Data.Attoparsec.ByteString.Char8
import Data.Maybe
import Network.IRC
import Data.Response

joinCmd :: Monad m => BotConfig -> Response m
joinCmd r = fromMsgParser' (string ":join" *> space *> takeByteString) $ 
    \p _ c -> runMaybeT $ do
        guard (fromAdmin' r p)
        if c `elem` banChans r
            then fail "banned channel"
            else return $ joinChan c

leaveCmd :: Monad m => BotConfig -> Response m
leaveCmd r = fromMsgParser' 
    (string ":leave" *> optional (space *> takeByteString)) $ \p c c'->
    runMaybeT $ do
        chan <- MaybeT . return $ c
        guard (fromAdmin' r p)
        return . part . fromMaybe chan $ c'

nickCmd :: Monad m => BotConfig -> Response m
nickCmd r = fromMsgParser' (string ":nick" *> space *> takeByteString) $ 
    \p _ c -> runMaybeT $ do
        guard (fromAdmin' r p)
        return . nick $ c

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
    if (last msg_params) `elem` banChans r
        then fail "banned channel"
        else return $ joinChan (last msg_params)

isInvite :: Message -> Bool
isInvite Message{..} = msg_command == "INVITE"

data SayCmd = SayCmd Channel ByteString
    deriving (Eq, Show, Ord, Read)

say :: Monad m => BotConfig -> Response m
say r = fromMsgParser' parser $ \p _ (SayCmd chan text) ->
    runMaybeT $ do
        guard (fromAdmin' r p)
        return $ privmsg chan text
    where parser = SayCmd <$> (string ":say" *> space *> channel)
                          <*> (space *> takeByteString)
          channel = cons <$> char '#' <*> takeWhile1 chanChars
          chanChars x = isAlpha_iso8859_15 x || isDigit x

source :: Monad m => Response m
source = simpleCmd ":source" $ \_ chan ->
    return $ privmsg (fromMaybe "" chan) "https://github.com/tsahyt/lmrbot"
