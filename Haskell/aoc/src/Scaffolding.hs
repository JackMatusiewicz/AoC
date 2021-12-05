module Scaffolding where

import Paths_aoc ( getDataFileName )

getData :: String -> IO [String]
getData fileName = do
    filePath <- getDataFileName fileName
    text <- readFile filePath
    return $ lines text
