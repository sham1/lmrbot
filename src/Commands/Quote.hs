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
import Control.Monad
import Control.Monad.Random.Class
import Data.FileEmbed
import Data.Response
import Data.Monoid
import Data.Maybe
import Data.Array hiding (bounds)
import Data.List (tails, inits, minimumBy)
import Data.Ord (comparing)
import Network.IRC
import Data.Vector (Vector)
import Data.ByteString.Char8 (ByteString)
import Data.Attoparsec.ByteString.Char8
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as B
import Text.Printf

type Quote = ByteString

editDistance :: ByteString -> ByteString -> Int
editDistance xs ys = table ! (m,n)
    where (m,n) = (B.length xs, B.length ys)
          x     = array (1,m) (zip [1..] $ B.unpack xs)
          y     = array (1,n) (zip [1..] $ B.unpack ys)
     
          table :: Array (Int, Int) Int
          table = array bnds [(ij, dist ij) | ij <- range bnds]
          bnds  = ((0,0),(m,n))
     
          dist (0,j) = j
          dist (i,0) = i
          dist (i,j) = minimum 
              [ table ! (i-1,j) + 1
              , table ! (i,j-1) + 1
              , if x ! i == y ! j 
                then table ! (i-1,j-1) 
                else 1 + table ! (i-1,j-1)
              ]

search :: ByteString -> [Quote] -> Maybe Int
search _ [] = Nothing
search pat xs = Just . fst . 
    minimumBy (comparing 
        (minimum . map (editDistance pat . B.pack) . (tails >=> inits) . snd)) 
       . zip [0..] . map B.unpack $ xs

data QuoteCmd
    = RandomQuote
    | Numbered Int
    | Search ByteString

qcmd :: ByteString -> Parser QuoteCmd
qcmd cmd = string cmd *> go
    where go = (Search <$> (space *> takeByteString))
           <|> (Numbered <$> (space *> signed decimal))
           <|> pure RandomQuote

quote :: MonadRandom m => ByteString -> Vector Quote -> Response m
quote cmd qs = fromMsgParser (qcmd cmd) $ \_ chan k -> do
    let bounds = V.length qs
    r <- case k of
        RandomQuote -> getRandomR (0, pred bounds)
        Numbered r' -> return (r' `mod` bounds)
        Search pat  -> return . fromMaybe 0 $ search pat (V.toList qs)
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
