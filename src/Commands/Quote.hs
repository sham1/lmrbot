{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
module Commands.Quote
(
    quote,
    rms,
    rmsfact,
    linus,
    theo,
    catv
)
where

import Control.Applicative
import Control.Monad.Random.Class
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
qcmd cmd = string cmd *> optional (space *> signed decimal)

quote :: MonadRandom m => ByteString -> Vector Quote -> Response m
quote cmd qs = fromMsgParser (qcmd cmd) $ \_ chan k -> do
    let bounds = V.length qs
    r <- case k of
        Nothing -> getRandomR (0, pred bounds)
        Just r' -> return (r' `mod` bounds)
    let pfix = printf "[%d/%d] " r (pred bounds)
    return $ privmsg (fromMaybe "" chan) (B.pack pfix <> qs ! r)

rms :: MonadRandom m => Response m
rms = quote ":rms" rmsQs
    where rmsQs = fromEmbed ($(embedFile "quotes/rms"))

rmsfact :: MonadRandom m => Response m
rmsfact = quote ":rmsfact" rmsQs
    where rmsQs = fromEmbed ($(embedFile "quotes/rmsfact"))

linus :: MonadRandom m => Response m
linus = quote ":linus" linusQs
    where linusQs = fromEmbed ($(embedFile "quotes/linus"))

theo :: MonadRandom m => Response m
theo = quote ":theo" theoQs
    where theoQs = fromEmbed ($(embedFile "quotes/theo"))

catv :: MonadRandom m => Response m
catv = quote ":catv" catvQs
    where catvQs = fromEmbed ($(embedFile "quotes/catv"))

fromEmbed :: ByteString -> Vector ByteString
fromEmbed = V.fromList . B.lines
