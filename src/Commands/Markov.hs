{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Commands.Markov
(
    MarkovMap (..),
    randomStart,
    markov,
    nlab
)
where

import Control.Arrow
import Control.Monad.Random.Class
import Data.ByteString.Char8 (ByteString, pack, unpack)
import Data.Distribution hiding (toList, on)
import Data.FileEmbed
import Data.Maybe
import Data.Ord
import Data.Response
import Data.Char
import Data.Function
import Data.List (tails, groupBy, sortBy, sort, group)
import Data.Map.Strict (Map)
import Network.IRC (privmsg)
import qualified Data.Map.Strict as M

newtype History a = History { unHist :: [a] }
    deriving (Eq, Show, Ord)

newtype MarkovMap a = MarkovMap { unMM :: Map (History a) (Distribution a) }
    deriving (Eq, Show, Ord)

runChain :: forall a m. (Ord a, Show a, MonadRandom m) 
         => Int -> [a] -> MarkovMap a -> m [a]
runChain l start (MarkovMap m) = fst <$> go ([], History start)
    where go :: ([a], History a) -> m ([a], History a)
          go (xs, hist)
              | length xs == l = return (xs, hist)
              | otherwise = do
                  let cs = fromMaybe (error $ "Key not in chain " ++ show hist)
                         $ M.lookup hist m
                  r <- getSample . fromDistribution $ cs

                  let History (h:hs) = hist
                  go (xs ++ [h], History $ hs ++ [r])

randomStart :: MonadRandom m => MarkovMap a -> m [a]
randomStart (MarkovMap m) = 
    unHist . (M.keys m !!) <$> getRandomR (0, pred $ M.size m)

seperators :: String
seperators = ":;,."

tokenize :: String -> [String]
tokenize = concatMap (foldr go []) . words
    where go x [] | x `elem` seperators = [[], [x]]
                  | otherwise = [[x]]
          go x xs | x `elem` seperators = [] : [x] : xs
                  | otherwise = let (y:ys) = xs in (x:y) : ys

render :: [String] -> String
render = dropWhile isSpace . foldr go []
    where go x xs | x `elem` map pure seperators = x ++ xs
                  | otherwise = " " ++ x ++ xs

buildChain :: forall a. (Ord a) => Int -> [a] -> MarkovMap a
buildChain n tokens = 
    let ngrams = map (take (n + 1)) . take (length tokens) 
               . tails . cycle $ tokens
        ndist  = map (History . fst . head &&& fromList . groupOccurrences)
               . groupBy ((==) `on` fst)
               . sortBy (comparing fst) . map (init &&& last) $ ngrams
     in MarkovMap . M.fromList $ ndist

    where groupOccurrences = percentage . group . sort . map snd

          percentage :: [[c]] -> [(c, Probability)]
          percentage xs = 
              let total = fromIntegral . length . concat $ xs
               in map (head &&& (/ total) . fromIntegral . length) xs

markov :: MonadRandom m 
       => ByteString -> MarkovMap String -> [[String]] -> Response m
markov cmd chain starts = simpleCmd cmd $ \_ chan -> do
    s <- (starts !!) <$> getRandomR (0, pred $ length starts)
    ret <- render <$> runChain 32 s chain
    return $ privmsg (fromMaybe "" chan) (pack ret)

nlab :: MonadRandom m => Response m
nlab = markov ":nlab" chain $ map tokenize starts
    where chain = buildChain 2 . tokenize . unpack $ 
                      ($(embedFile "etc/markov/nlab"))
          starts = [ "An orientifold"
                   , "A monad"
                   , "An explicit"
                   , "Ordinary categories"
                   , "A bicategory"
                   , "The morphisms"
                   , "More generally" 
                   , "The term" ]
