module DayOne(solvePartOne, solvePartTwo) where
import Scaffolding

measureIncreases :: [Int] -> Int
measureIncreases depths =
    length $ filter (uncurry (>)) $ zip (drop 1 depths) depths

solvePartOne :: IO ()
solvePartOne = do
    lines <- getData "DayTwoData.txt"
    print $ measureIncreases $ (read :: String -> Int) <$> lines

slidingWindow :: [Int] -> [Int]
slidingWindow (a:b:c:d) = (a+b+c) : slidingWindow (b:c:d)
slidingWindow _ = []

measureSlidingWindowIncreases :: [Int] -> Int
measureSlidingWindowIncreases = measureIncreases . slidingWindow

solvePartTwo :: IO ()
solvePartTwo = do
    lines <- getData "DayTwoData.txt"
    print $ measureSlidingWindowIncreases $ (read :: String -> Int) <$> lines