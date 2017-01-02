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
import Data.Traversable
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
    <$> (maskComp <* char '!')
    <*> (maskComp <* char '@')
    <*> (maskComp <* endOfInput)

maskComp :: Parser (NonEmpty MaskComp)
maskComp = N.fromList <$> many1 (choice [ wildcard, strComp ])
    where wildcard  = MaskWildCard <$ char '*'
          strComp   = MaskString . pack <$> many1 (satisfy charSet)

charSet x = isAlphaNum x || inClass "[]-_^.|" x

matcher :: HostMask -> Parser ()
matcher HostMask{..} = () <$ 
    (go nick *> char '!' *> go nick *> char '@' *> go host)
    where go = traverse matchComp
          matchComp :: MaskComp -> Parser ByteString
          matchComp MaskWildCard   = pack <$> many (satisfy charSet)
          matchComp (MaskString x) = string x

matchHostMask :: HostMask -> ByteString -> Bool
matchHostMask mask x = isRight (parseOnly (matcher mask) x)
