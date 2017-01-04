{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Response
(
    Response (..),
    pingR,
    isPing,
    isNSNotice,
    mode,
    simpleCmd,
    simpleCmd',
    fromMsgParser,
    fromMsgParser',
    ctcpCmd,
    ctcpCmd',
    ctcpVersion,
    fromUser,
    fromAdmin,
    fromAdmin',
    rateLimit,
    rateLimit',
    userLimit,
    userLimit',
    emptyCooldown,
    UserCooldown
)
where

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Data.BotConfig
import Data.Monoid
import Data.ByteString.Char8 (ByteString)
import Data.Attoparsec.ByteString.Char8
import Data.Time.Clock.POSIX
import Data.Maybe
import Network.IRC

import qualified Data.Map as M

newtype Response m = Response { respond :: Message -> m (Maybe Message) }

-- | Respond to standard ping messages
pingR :: Applicative m => Response m
pingR = Response $ \m@Message{..} -> pure $ do
    guard (isPing m)
    return $ pong (head msg_params)

isPing :: Message -> Bool
isPing Message{..} = msg_command == "PING"

isNSNotice :: Message -> Bool
isNSNotice m@Message{..} = msg_command == "NOTICE" && fromUser "NickServ" m

mode :: BotConfig -> ByteString -> Message
mode BotConfig{..} m = Message Nothing ("MODE " <> botnick <> " " <> m) []

simpleCmd :: Monad m 
          => ByteString 
          -> (Maybe Prefix -> Maybe Channel -> m Message)
          -> Response m
simpleCmd x f = fromMsgParser (() <$ string x) (\a b _ -> f a b)

simpleCmd' :: Monad m 
           => ByteString 
           -> (Maybe Prefix -> Maybe Channel -> m (Maybe Message))
           -> Response m
simpleCmd' x f = fromMsgParser' (() <$ string x) (\a b _ -> f a b)
          
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

-- | Response on CTCP commands like VERSION. This is identical to normal
-- commands but wraps the parser in @\001@s for CTCP. The supplied function must
-- decide whether to NOTICE or PRIVMSG itself.
ctcpCmd :: Monad m
        => Parser a
        -> (Maybe Prefix -> Maybe ByteString -> a -> m Message)
        -> Response m
ctcpCmd p f = ctcpCmd' p (\x y z -> pure <$> f x y z)

-- | Like 'ctcpCmd' but allowing failure.
ctcpCmd' :: Monad m
         => Parser a
         -> (Maybe Prefix -> Maybe ByteString -> a -> m (Maybe Message))
         -> Response m
ctcpCmd' p = fromMsgParser' (ctcpChar *> p <* ctcpChar)
    where ctcpChar = char '\001'

ctcpVersion :: Monad m => Response m
ctcpVersion = ctcpCmd (() <$ string "VERSION") $ \_ u _ ->
    return $ notice (fromMaybe "" u) "lmrbot -- github.com/tsahyt/lmrbot"

notice :: ByteString -> ByteString -> Message
notice u m = Message Nothing "NOTICE" [u,m]

fromAdmin :: BotConfig -> Message -> Bool
fromAdmin BotConfig{..} m = fromMaybe False $ do
    u <- msgUser m
    return $ u `elem` adminUsers

fromAdmin' :: BotConfig -> Maybe Prefix -> Bool
fromAdmin' BotConfig{..} p = fromMaybe False $ do
    NickName u _ _ <- p
    return $ u `elem` adminUsers

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
    rateLimit' c mvar res

-- | Like 'rateLimit' but with external timestamp 'MVar'.
rateLimit' :: MonadIO m 
           => BotConfig -> MVar POSIXTime -> Response m -> m (Response m)
rateLimit' c mvar res = return . Response $ \m -> do
        res' <- respond res m
        case res' of
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
    userLimit' c mvar res

type UserCooldown = M.Map UserName POSIXTime

emptyCooldown :: MonadIO m => m (MVar UserCooldown)
emptyCooldown = liftIO (newMVar M.empty)

-- | Like 'userLimit' but supplying an external cooldown map. This is useful for
-- sharing cooldown times between commands.
userLimit' :: MonadIO m 
           => BotConfig -> MVar UserCooldown -> Response m -> m (Response m)
userLimit' c mvar res = return . Response $ \m -> do
        res' <- respond res m
        case res' of
            Nothing -> return Nothing
            r -> do
                umap <- liftIO (takeMVar mvar)
                tc   <- utcTimeToPOSIXSeconds <$> liftIO getCurrentTime
                let u     = fromMaybe ".unknown." $ msgUser m
                    ts    = fromMaybe 0 $ M.lookup u umap
                    umap' = M.insert u tc umap
                liftIO (putMVar mvar umap')
                return $ if tc - ts <= rateTime c && not (fromAdmin c m)
                         then Nothing
                         else r
