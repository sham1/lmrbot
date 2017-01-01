{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Commands.Quote
(
    quote,
    rms,
    linus
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

linus :: MonadRandom m => Response m
linus = quote ".linus"
    [ "There aren't enough swear-words in the English language, so now I'll\
     \ have to call you perkeleen vittupää just to express my disgust and\
     \ frustration with this crap."
    , "Microsoft isn't evil, they just make really crappy operating systems."
    , "That's the spirit. Greg has taught you well. You have controlled your\
     \ fear. Now, release your anger. Only your hatred can destroy me. Come to\
     \ the dark side, Sarah. We have cookies."
    , "I don’t care about you."
    , "We don't merge kernel code just because user space was written by a\
     \ retarded monkey on crack."
    , "Software is like sex; it's better when it's free."
    , "Christ, people. Learn C, instead of just stringing random characters\
     \ together until it compiles (with warnings)."
    ]
