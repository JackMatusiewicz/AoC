module D8P1(solve) where

import Scaffolding

formatLine :: String -> Int
formatLine = length . filter (\a -> length a `elem` [2,3,4,7]) . words . tail . dropWhile (/= '|')

solve :: IO ()
solve = do
    v <- getData "DayEight.txt"
    print $ sum $ fmap formatLine v