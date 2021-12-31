module Utils(chunks, lmap, rmap) where

import Data.Bifunctor(bimap, Bifunctor)

-- | Chunks a list into a list of lists where each sub list
-- has at most n elements.
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)

lmap :: Bifunctor f => (a -> c) -> f a b -> f c b
lmap f = bimap f id

rmap :: Bifunctor f => (b -> d) -> f a b -> f a d
rmap f = bimap id f