{-# LANGUAGE OverloadedStrings #-}
module Commands.Admin
(
    joinCmd,
    leaveCmd
)
where

import Control.Monad
import Control.Monad.Trans.Maybe
import Data.BotConfig
import Data.Attoparsec.ByteString.Char8
import Network.IRC
import Data.Response

joinCmd :: Monad m => BotConfig -> Response m
joinCmd r = fromMsgParser' (string ":join" *> space *> takeByteString) $ 
    \p _ c -> runMaybeT $ do
        NickName n _ _ <- MaybeT . return $ p
        guard (n == adminUser r)
        return $ joinChan c

leaveCmd :: Monad m => BotConfig -> Response m
leaveCmd r = simpleCmd' ":leave" $ \p c ->
    runMaybeT $ do
        NickName n _ _ <- MaybeT . return $ p
        chan <- MaybeT . return $ c
        guard (n == adminUser r)
        return $ part chan
