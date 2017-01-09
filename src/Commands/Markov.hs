{-# LANGUAGE ScopedTypeVariables #-}
module Commands.Markov
where

import Control.Arrow
import Control.Monad.Random.Class
import Data.Distribution hiding (toList, on)
import Data.Maybe
import Data.Proxy
import Data.Ord
import Data.Foldable
import Data.Function
import Data.List (tails, groupBy, sortBy, sort, group)
import Data.Map (Map)
import qualified Data.Map as M

import GHC.TypeLits
import qualified GHC.Exts as E

newtype History a = History [a]
    deriving (Eq, Show, Ord)

newtype MarkovMap a = MarkovMap { unMM :: Map (History a) (Distribution a) }
    deriving (Eq, Show, Ord)

runChain :: forall a m. (Ord a, MonadRandom m) => Int -> MarkovMap a -> m [a]
runChain l (MarkovMap m) = do
    xs <- (M.keys m !!) <$> getRandomR (0, pred $ M.size m)
    fst <$> go ([], xs)

    where go :: ([a], History a) -> m ([a], History a)
          go (xs, hist)
              | length xs == l = return (xs, hist)
              | otherwise = do
                  let cs = fromMaybe (error "Key not in chain") 
                         $ M.lookup hist m
                  r <- getSample . fromDistribution $ cs

                  let History (h:hs) = hist
                  go (xs ++ [h], History $ hs ++ [r])

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
