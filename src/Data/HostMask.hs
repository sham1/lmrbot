{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.HostMask
(
    HostMask (..),
    MaskComp (..),
    parseHostMask,
    matchHostMask
)
where

import Control.Applicative
import Data.Char (isAlphaNum)
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (ByteString, pack)
import Data.Either (isRight)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import GHC.Generics

data HostMask = HostMask
    { nick :: NonEmpty MaskComp
    , user :: NonEmpty MaskComp
    , host :: NonEmpty MaskComp
    }
    deriving (Eq, Show, Ord, Read, Generic)

data MaskComp 
    = MaskString ByteString
    | MaskWildCard
    deriving (Eq, Show, Ord, Read, Generic)

parseHostMask :: ByteString -> Either String HostMask
parseHostMask = parseOnly hostMask

hostMask :: Parser HostMask
hostMask = HostMask
    <$> (maskComp csNick <* char '!')
    <*> (maskComp csNick <* char '@')
    <*> (maskComp csHost <* endOfInput)

maskComp :: (Char -> Bool) -> Parser (NonEmpty MaskComp)
maskComp cs = N.fromList <$> many1 (choice [ wildcard, strComp ])
    where wildcard  = MaskWildCard <$ char '*'
          strComp   = MaskString . pack <$> many1 (satisfy cs)

csNick, csHost :: Char -> Bool
csNick x = isAlphaNum x || inClass "[]{}-_^|\\" x
csHost x = csNick x || inClass "/."

matcher :: HostMask -> Parser ()
matcher HostMask{..} = () <$ 
    (go csNick nick *> char '!' *> go csNick user *> char '@' *> go csHost host)
    where go cs = traverse (matchComp cs)
          matchComp :: (Char -> Bool) -> MaskComp -> Parser ByteString
          matchComp cs MaskWildCard   = pack <$> many (satisfy cs)
          matchComp _  (MaskString x) = string x

matchHostMask :: HostMask -> ByteString -> Bool
matchHostMask mask x = isRight (parseOnly (matcher mask) x)
