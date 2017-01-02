{-# LANGUAGE OverloadedStrings #-}
module Commands.Admin
(
    joinCmd,
    leaveCmd
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
