module DayOne(solvePartOne, solvePartTwo) where
import Paths_aoc ( getDataFileName )

measureIncreases :: [Int] -> Int
measureIncreases depths =
    length $ filter (uncurry (>)) $ zip (drop 1 depths) depths

solvePartOne :: IO ()
solvePartOne = do
    filePath <- getDataFileName "DayOneData.txt"
    text <- readFile filePath
    print $ measureIncreases $ (read :: String -> Int) <$> lines text

slidingWindow :: [Int] -> [Int]
slidingWindow (a:b:c:d) = (a+b+c) : slidingWindow (b:c:d)
slidingWindow _ = []

measureSlidingWindowIncreases :: [Int] -> Int
measureSlidingWindowIncreases = measureIncreases . slidingWindow

solvePartTwo :: IO ()
solvePartTwo = do
    filePath <- getDataFileName "DayOneData.txt"
    text <- readFile filePath
    print $ measureSlidingWindowIncreases $ (read :: String -> Int) <$> lines text