module Main where

import DayOne
import Paths_aoc ( getDataFileName )

main :: IO ()
main = do
    filePath <- getDataFileName "DayOneData.txt"
    text <- readFile filePath
    print $ measureIncreases $ (read :: String -> Int) <$> lines text
