{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Commands.RotCipher
(
    rotCmd
)
where

import Control.Arrow
import Data.Char
import Data.FileEmbed
import Data.Maybe
import Data.Response
import Data.Ord (comparing, Down (..))
import Data.List (sortBy, sort, group, maximumBy)
import Data.ByteString.Char8 (ByteString, unpack, pack)
import Data.Attoparsec.ByteString.Char8
import Data.HashSet (HashSet)
import Network.IRC
import qualified Data.HashSet as S
import qualified Data.ByteString.Char8 as B

rot :: Int -> String -> String
rot k = map (rotOne k)

rotOne :: Int -> Char -> Char
rotOne k x 
    | isAlpha x && isUpper x = go 65 x
    | isAlpha x && isLower x = go 97 x
    | otherwise = x
    where go :: Int -> Char -> Char
          go off = chr . (+ off) . (`rem` 26) . subtract off . (+ k) . ord

dictMatch :: String -> Double
dictMatch = percentage . map (flip S.member dictionary . pack) 
          . filter (all isAlpha) . words
    where percentage xs = let n = fromIntegral . length $ xs 
                              k = fromIntegral . length . filter id $ xs
                           in k / n

dictionary :: HashSet ByteString
dictionary = S.fromList . B.words $ ($(embedFile "etc/wordlist.txt"))

derot :: String -> String
derot s = let choices = map (`rot` s) [1..25] in best choices
    where best [] = "I cannot decipher an empty string!"
          best xs = maximumBy (comparing dictMatch) xs

data RotCmd = Encrypt { key :: Int, plaintext :: String }
            | Decrypt { ciphertext :: String }

parser :: Parser RotCmd
parser = string ":rot" *> space *> choice [ enc, dec ]
    where enc = Encrypt 
            <$> (decimal <* space) <*> (unpack <$> takeByteString)
          dec = Decrypt . unpack 
            <$> (string "decipher" *> space *> takeByteString)

rotCmd :: Monad m => Response m
rotCmd = fromMsgParser parser $ \_ chan method -> case method of
    Encrypt{..} -> let x = pack $ rot key plaintext
                    in return $ privmsg (fromMaybe "" chan) x
    Decrypt{..} -> let x = pack $ derot ciphertext
                    in return $ privmsg (fromMaybe "" chan) x
