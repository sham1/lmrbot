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

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Commands.Admin
import Commands.Quote
import Commands.Interject
import Commands.Wolfram
import Commands.Reddit

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

response :: Monad m => [Response m] -> Pipe Message ByteString m ()
response rsps = P.mapM go >-> filterJust >-> P.map encode
    where go m = listToMaybe . catMaybes <$> mapM (`respond` m) rsps

bootstrap :: MonadIO m 
          => BotConfig 
          -> Producer ByteString m () 
          -> Consumer ByteString m () 
          -> Effect m ()
bootstrap conf up down = do
    let encSend = P.map encode >-> P.tee outbound >-> down

    up >-> P.take 2 >-> inbound
    register conf >-> encSend

    -- wait for and respond to initial ping
    up >-> P.tee inbound >-> parseIRC >-> P.dropWhile (not . isPing) 
       >-> P.take 1 >-> response [ pingR ] >-> P.tee outbound >-> down

    -- drain until nickserv notice
    up >-> P.tee inbound >-> parseIRC >-> P.dropWhile (not . isNSNotice) 
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
    comms' <- sequence (comms conf cooldown man)

    -- bot loop
    runEffect $ 
        up >-> P.tee inbound >-> parseIRC >-> response comms'
           >-> P.tee outbound >-> down

    where comms c ulim man = 
              [ return pingR
              , return $ inviteR c
              , return ctcpVersion
              , return $ joinCmd c
              , return $ leaveCmd c
              , return $ modeCmd c
              , userLimit' c ulim rms
              , userLimit' c ulim linus
              , userLimit' c ulim theo 
              , userLimit' c ulim catv
              , userLimit' c ulim (startrek man)
              , rateLimit c interject
              , return $ wolfram man (wolframAPI c)
              ]
