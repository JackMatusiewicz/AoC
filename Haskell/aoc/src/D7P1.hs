module D7P1(solve) where

import Scaffolding
import Data.List.Split(splitOn)
import Control.Monad(join)

formatInput :: [String] -> [Int]
formatInput = join . (fmap . fmap) read . fmap (splitOn ",")

solve :: IO ()
solve = do
    v <- getData "DaySeven.txt"
    print $ sum $ formatInput v