{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
module Commands.Quote
(
    quote,
    rms,
    linus,
    theo
)
where

import Control.Monad.Random.Class
import Data.FileEmbed
import Data.Response
import Data.Maybe
import Network.IRC
import Data.Vector (Vector, (!))
import Data.ByteString.Char8 (ByteString)
import Data.Attoparsec.ByteString.Char8
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as B

type Quote = ByteString

quote :: MonadRandom m => ByteString -> Vector Quote -> Response m
quote cmd qs = simpleCmd cmd $ \_ chan -> do
    r <- getRandomR (0, pred (V.length qs))
    return $ privmsg (fromMaybe "" chan) (qs ! r)

rms :: MonadRandom m => Response m
rms = quote ".rms" rmsQs
    where rmsQs = fromEmbed ($(embedFile "quotes/rms"))

linus :: MonadRandom m => Response m
linus = quote ".linus" linusQs
    where linusQs = fromEmbed ($(embedFile "quotes/linus"))

theo :: MonadRandom m => Response m
theo = quote ".theo" theoQs
    where theoQs = fromEmbed ($(embedFile "quotes/theo"))

fromEmbed = V.fromList . B.lines
