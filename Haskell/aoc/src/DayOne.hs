module DayOne where
import Data.Foldable (Foldable(foldr'))
import Data.Maybe

measureIncreases :: [Int] -> Int
measureIncreases depths =
    length $ filter (uncurry (>)) $ zip (drop 1 depths) depths