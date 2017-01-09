{-# LANGUAGE ScopedTypeVariables #-}
module Commands.Markov
where

import Control.Arrow
import Control.Monad.Random.Class
import Data.Distribution hiding (toList, on)
import Data.Maybe
import Data.Ord
import Data.Char
import Data.Function
import Data.List (tails, groupBy, sortBy, sort, group)
import Data.Map (Map)
import qualified Data.Map as M

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

tokenize :: String -> [String]
tokenize = concatMap (foldr go []) . words
    where go x [] | isPunctuation x = [[], [x]]
                  | otherwise = [[x]]
          go x xs | isPunctuation x = [] : [x] : xs
                  | otherwise = let (y:ys) = xs in (x:y) : ys

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
