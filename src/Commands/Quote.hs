{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Commands.Quote
(
    quote,
    rms
)
where

import Control.Monad.Random.Class
import Data.Response
import Data.Maybe
import Network.IRC
import Data.Vector (Vector, (!))
import Data.ByteString.Char8 (ByteString)
import Data.Attoparsec.ByteString.Char8
import qualified Data.Vector as V

type Quote = ByteString

quote :: MonadRandom m => ByteString -> Vector Quote -> Response m
quote cmd qs = simpleCmd cmd $ \_ chan -> do
    r <- getRandomR (0, pred (V.length qs))
    return $ privmsg (fromMaybe "" chan) (qs ! r)

rms :: MonadRandom m => Response m
rms = quote ".rms"
    [ "I'd just like to interject for a moment..."
    , "Thanks to Mr. Gates, we now know that an open Internet with protocols\
     \ anyone can implement is communism; it was set up by that famous \
     \ communist agent, the US Department of Defense."
    ]
