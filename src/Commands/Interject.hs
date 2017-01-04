{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Commands.Interject
(
    interject
)
where

import Data.Char (isAlphaNum)
import Data.Response
import Control.Applicative
import Data.Maybe
import Network.IRC
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
import Text.Printf

data Interjection = MkI
    { gnu   :: Maybe String
    , linux :: Maybe String
    , posix :: Maybe String
    , size  :: InterjectionLength
    }
    deriving (Show)

data InterjectionLength = Short | Full
    deriving (Show, Eq)

emptyInterject :: Interjection
emptyInterject = MkI Nothing Nothing Nothing Full

parser :: Parser Interjection
parser = choice [ try short
                , try param
                , string ":interject" *> pure emptyInterject ]
    where param = MkI
              <$> (string ":interject" *> skipSpace *> comp)
              <*> (satisfy isSep *> comp)
              <*> (satisfy isSep *> comp)
              <*> pure Full
          
          short = MkI
              <$> (string ":interject'" *> skipSpace *> comp)
              <*> (satisfy isSep *> comp)
              <*> pure Nothing
              <*> pure Short

          isSep c = isSpace c || inClass ",;:." c

          comp = optional (choice [ quoted, singleWord ])
          quoted = char '"' *> many1 (satisfy charSet <|> space) <* char '"'
          singleWord = many1 (satisfy charSet)
          charSet x = isAlphaNum x || inClass "-_" x

interjection :: String
interjection = 
    "I'd just like to interject for a moment. What you're referring to as %s,\
   \ is in fact, %s/%s, or as I've recently taken to calling it, %s plus %s.\
   \ %s is not an operating system unto itself, but rather another free\
   \ component of a fully functioning %s system made useful by the %s corelibs,\
   \ shell utilities and vital system components comprising a full OS as\
   \ defined by %s."

shortInterjection :: String
shortInterjection = 
    "I'd just like to interject for a moment. What you're referring to as %s,\
   \ is in fact, %s/%s, or as I've recently taken to calling it, %s plus %s."

interject :: Monad m => Response m
interject = fromMsgParser parser $ \_ chan MkI{..} ->
    let x = case size of
                Full -> printf interjection 
                            linux' gnu' linux' gnu' linux' 
                            linux' gnu' gnu' posix'
                Short -> printf shortInterjection
                            linux' gnu' linux' gnu' linux'
        gnu'   = fromMaybe "GNU" gnu
        linux' = fromMaybe "Linux" linux
        posix' = fromMaybe "POSIX" posix
     in pure $ privmsg (fromMaybe "" chan) (B.pack x)
