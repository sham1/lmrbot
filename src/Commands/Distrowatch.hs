{-# LANGUAGE OverloadedStrings #-}
module Commands.Distrowatch (
    distrowatch
) where

import qualified Data.Response as R
import Control.Monad.IO.Class
import Network.HTTP.Client
import Data.ByteString.Char8 (ByteString, unpack)
import qualified Data.ByteString.Lazy as B
import Data.Monoid
import Data.Maybe
import Network.IRC
import Text.HTML.TagSoup

data Distro = Distro
    { distroURL :: ByteString
    , distroName :: ByteString
    }
    deriving (Show, Eq)

randomDistro :: MonadIO m => Manager -> m Distro
randomDistro manager = liftIO $ do
    req1 <- parseRequest "http://distrowatch.com/random"
    rsp1 <- withResponseHistory req1 manager $ pure . hrFinalRequest
    let url = "http://distrowatch.com" <> path rsp1 <> queryString rsp1

    req2 <- parseRequest $ unpack url
    rsp2 <- responseBody <$> httpLbs req2 manager

    let name = case head . drop 5 . parseTags $ rsp2 of
                TagText x
                    | "DistroWatch" `B.isPrefixOf` x -> B.drop 17 x
                _ -> "<unknown>"

    pure $ Distro url (B.toStrict name)

distrowatch :: MonadIO m => Manager -> R.Response m
distrowatch manager =
    R.simpleCmd ":distro" $ \_ c -> do
        distro <- randomDistro manager
        let resp = distroName distro <> " - " <> distroURL distro
        pure $ privmsg (fromMaybe "" c) resp
