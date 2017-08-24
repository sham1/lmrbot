{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Commands.Quote
(
    quote,
    rms,
    rmsfact,
    linus,
    theo,
    catv,
    arch
)
where

import Control.Applicative
import Control.Monad.Random.Class
import Data.FileEmbed
import Data.Response
import Data.Monoid
import Data.Maybe
import Data.Array hiding (bounds)
import Data.Ord (comparing)
import Network.IRC
import Data.Vector (Vector)
import Data.ByteString.Char8 (ByteString)
import Data.Attoparsec.ByteString.Char8
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as B
import Text.Printf

type Quote = ByteString

similarity :: ByteString -> ByteString -> Int
similarity xs ys =
    minimum [ table ! (m,i) | i <- [0..n] ]
    where (m,n) = (B.length xs, B.length ys)
     
          table :: Array (Int, Int) Int
          table = array bnds [(ij, dist ij) | ij <- range bnds]
          bnds  = ((0,0),(m,n))
     
          dist (0,_) = 0
          dist (i,0) = i
          dist (i,j) = minimum 
              [ table ! (i-1,j) + 1
              , table ! (i,j-1) + 1
              , if xs `B.index` (pred i) == ys `B.index` (pred j)
                then table ! (i-1,j-1) 
                else 1 + table ! (i-1,j-1)
              ]

search :: ByteString -> Vector Quote -> Maybe Int
search pat xs 
    | V.null xs = Nothing
    | otherwise = Just . V.minIndexBy (comparing (similarity pat)) $ xs
{-# INLINE search #-}

data QuoteCmd
    = RandomQuote
    | Numbered Int
    | Search ByteString

qcmd :: ByteString -> Parser QuoteCmd
qcmd cmd = string cmd *> go
    where go = (Numbered <$> (space *> signed decimal))
           <|> (Search <$> (space *> takeByteString))
           <|> pure RandomQuote

quote :: MonadRandom m => ByteString -> Vector Quote -> Response m
quote cmd qs = fromMsgParser (qcmd cmd) $ \_ chan k -> do
    let bounds = V.length qs
    res <- case k of
        RandomQuote -> Just <$> getRandomR (0, pred bounds)
        Numbered r' -> pure (Just $ r' `mod` bounds)
        Search pat  -> pure $ search pat qs
    case res of
        Nothing ->
            return $ privmsg (fromMaybe "" chan) "No quote found!"
        Just r -> do
            let pfix = printf "[%d/%d] " r (pred bounds)
            return $ privmsg (fromMaybe "" chan) (B.pack pfix <> qs V.! r)

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

arch :: MonadRandom m => Response m
arch = quote ":arch" archQs
    where archQs = fromEmbed ($(embedFile "quotes/arch"))

fromEmbed :: ByteString -> Vector ByteString
fromEmbed = V.fromList . B.lines
