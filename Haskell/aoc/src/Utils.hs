module Utils(chunks) where

-- | Chunks a list into a list of lists where each sub list
-- has at most n elements.
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)