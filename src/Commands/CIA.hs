{-# LANGUAGE OverloadedStrings #-}
module Commands.CIA
(
    cia,
    fbi,
    fiveeyes
)
where

import Control.Applicative
import Control.Monad.Random.Class
import Data.Response
import Data.Maybe
import Data.Monoid
import Network.IRC
import Data.Attoparsec.ByteString.Char8 (string)
import qualified Data.ByteString.Char8 as B

cia :: MonadRandom m => Response m
cia =
    fromMsgParser parser $ \_ chan _ -> do
        c <- getRandomR (1337 :: Int, 99999 :: Int)
        let ret = "This incident has been reported. Case #" <> B.pack (show c)
        return $ privmsg (fromMaybe "" chan) ret
  where
    parser = string ":cia" <|> string ":CIA"

fbi :: MonadRandom m => Response m
fbi =
    fromMsgParser parser $ \_ chan _ -> do
        c <- getRandomR (1337 :: Int, 99999 :: Int)
        let ret = "This incident was lost in a secret memo. Case #" <> B.pack (show c)
        return $ privmsg (fromMaybe "" chan) ret
  where
    parser = string ":fbi" <|> string ":FBI"

fiveeyes :: MonadRandom m => Response m
fiveeyes = 
    fromMsgParser $ \_ chan _ -> do
        c <- getRandomR (1337 :: Int, 99999 :: Int)
        let ret = "This incident was reported to a member of and share amongst the Five Eyes. Case #" <> B.pack (show c)
        return $ privmsg (fromMaybe "" chan) ret
    where
        parser = string ":fiveeyes" <|> string ":FIVEEYES"
