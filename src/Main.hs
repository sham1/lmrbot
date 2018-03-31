{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad.Random ()
import Control.Monad.Trans.Maybe
import Data.BotConfig
import Data.ByteString.Char8 (ByteString)
import Data.Maybe
import Data.Monoid
import Data.Response
import Network.IRC
import Pipes
import Pipes.Network
import qualified Pipes.Prelude as P
import qualified Data.ByteString.Char8 as B

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Commands.Admin
import Commands.Quote
import Commands.Interject
import Commands.Wolfram
import Commands.Github
import Commands.Reddit
import Commands.Crypto
import Commands.RotCipher
import Commands.Markov
import Commands.CIA
import Commands.Distrowatch

import Options.Applicative

data Options = Options
    { configPath :: Maybe FilePath
    }

optParser :: Parser Options
optParser = Options 
    <$> optional 
        (strOption ( long "config" 
                  <> metavar "FILE" 
                  <> help "Config File Location" ))

optInfo :: ParserInfo Options
optInfo = info (helper <*> optParser)
    ( fullDesc
   <> progDesc "An IRC Bot"
   <> header "lmrbot - A spambot" )

response :: Monad m => Response m -> Pipe Message ByteString m ()
response rsp =
    P.mapM (respond rsp) >-> filterJust >-> P.map (encode . filterLong)
  where
    filterLong t
        | [c, m] <- msg_params t
        , B.length m >= 448 = t {msg_params = [c, longMsg]}
        | otherwise = t
    longMsg = "Nah, too much work. You're on your own"

bootstrap :: MonadIO m 
          => BotConfig 
          -> Producer ByteString m () 
          -> Consumer ByteString m () 
          -> Effect m ()
bootstrap conf up down = do
    let encSend = P.map encode >-> P.tee (outbound conf) >-> down

    up >-> P.take 2 >-> inbound conf
    register conf >-> encSend

    -- wait for and respond to initial ping
    up >-> P.tee (inbound conf) >-> parseIRC >-> P.dropWhile (not . isPing) 
       >-> P.take 1 >-> response pingR >-> P.tee (outbound conf) >-> down

    -- drain until nickserv notice
    up >-> P.tee (inbound conf) >-> parseIRC >-> P.dropWhile (not . isNSNotice) 
       >-> P.take 1 >-> P.drain

    -- do nickserv auth and modes, then wait 1s before joining
    auth conf >-> encSend
    modes conf >-> encSend
    liftIO (threadDelay 1000000)
    joins conf >-> encSend

main :: IO ()
main = do
    opts <- execParser optInfo
    conf <- fmap (fromMaybe defaultConfig) . runMaybeT $ do
                p <- MaybeT . return $ configPath opts
                MaybeT $ readConfig p
    h <- network conf
    man <- newManager tlsManagerSettings
    let up   = fromHandleLine h
        down = toHandleLine h

    runEffect $ bootstrap conf up down 

    cooldown <- emptyCooldown
    comms' <- mconcat <$> sequence (comms conf cooldown man)

    -- bot loop
    runEffect $ 
        up >-> P.tee (inbound conf) >-> parseIRC 
           >-> response (userIgnore conf comms')
           >-> P.tee (outbound conf) >-> down

    where comms c ulim man = 
              [ return pingR
              , return $ inviteR c
              , return ctcpVersion
              , return $ joinCmd c
              , return $ leaveCmd c
              , return $ nickCmd c
              , return $ modeCmd c
              , return $ say c
              , return source
              , userLimit' c ulim rotCmd
              , userLimit' c ulim rmsfact
              , userLimit' c ulim rms
              , userLimit' c ulim linus
              , userLimit' c ulim theo 
              , userLimit' c ulim catv
              , userLimit' c ulim arch
              , userLimit' c ulim =<< startrek man
              , userLimit' c ulim =<< wcgw man
              , userLimit' c ulim =<< meme man
              , userLimit' c ulim =<< wallpaper man
              , userLimit' c ulim nlab
              , userLimit' c ulim trump
              , userLimit' c ulim marxov
              , userLimit' c ulim stirner
              , userLimit' c ulim trek
              , userLimit' c ulim interject
              , userLimit' c ulim cia
              , userLimit' c ulim fbi
              , userLimit' c ulim fiveeyes
              , userLimit' c ulim (distrowatch man)
              , return $ wolfram man (wolframAPI c)
              , return $ github man
              , return $ crypto man
              ]
