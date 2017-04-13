{-# LANGUAGE OverloadedStrings #-}
module Commands.CIA
(
    cia
)
where

import Control.Monad.Random.Class
import Data.Response
import Data.Maybe
import Data.Monoid
import Network.IRC
import qualified Data.ByteString.Char8 as B

cia :: MonadRandom m => Response m
cia = simpleCmd ":cia" $ \_ chan -> do
    c <- getRandomR (1337 :: Int, 99999 :: Int)
    let ret = "This incident has been reported. Case #" <> B.pack (show c)
    return $ privmsg (fromMaybe "" chan) ret
