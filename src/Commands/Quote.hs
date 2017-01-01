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

import Control.Applicative
import Control.Monad.Random.Class
import Control.Monad.Trans.Maybe
import Data.FileEmbed
import Data.Response
import Data.Monoid
import Data.Maybe
import Network.IRC
import Data.Vector (Vector, (!))
import Data.ByteString.Char8 (ByteString)
import Data.Attoparsec.ByteString.Char8
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as B
import Text.Printf

type Quote = ByteString

qcmd :: ByteString -> Parser (Maybe Int)
qcmd cmd = string cmd *> optional (read <$> (space *> many1 digit))

quote :: MonadRandom m => ByteString -> Vector Quote -> Response m
quote cmd qs = fromMsgParser (qcmd cmd) $ \_ chan k -> do
    let bounds = pred (V.length qs)
    r <- case k of
        Nothing -> getRandomR (0, bounds)
        Just r' -> if 0 <= r' && r' <= bounds 
                   then return r' 
                   else getRandomR (0, bounds)
    let prefix = printf "[%d/%d] " r bounds
    return $ privmsg (fromMaybe "" chan) (B.pack prefix <> qs ! r)

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
