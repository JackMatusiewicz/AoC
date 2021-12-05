module DayThree where

import Data.Bits
import Scaffolding
import Data.Char (digitToInt)
import Data.List (transpose)

newtype BitCount = BitCount Int deriving(Eq, Show)
newtype Gamma = Gamma Int deriving(Eq, Show)
newtype Epsilon = Epsilon Int deriving(Eq, Show)

-- Returns the length of the longest input string
longestInput :: [String] -> BitCount
longestInput = BitCount . foldr (max . length) 0

-- Epsilon is just the xor of Gamma.
getEpsilon :: BitCount -> Gamma -> Epsilon
getEpsilon (BitCount b) (Gamma g) = Epsilon $ xor g (2^b - 1)

-- Converts a list of digits (big endian) into a number
toInt :: [Int] -> Int
toInt = fst . foldr (\ a (s, n) -> ((a * 2 ^ n) + s, n + 1)) (0,0)

-- Picks either 0 or 1, depending on which shows up most
mode :: [Int] -> Int
mode xs = if (length $ filter (==1) xs) > (length $ filter (==0) xs) then 1 else 0

calculateGamma :: [[Int]] -> Gamma
calculateGamma =
    Gamma . toInt . fmap mode . transpose

solvePartOne :: IO ()
solvePartOne = do
    lines <- getData "DayThreeData.txt"
    let bitCount = longestInput lines in
        let (Gamma g) = calculateGamma ((fmap . fmap) digitToInt lines) in
            let (Epsilon e) = getEpsilon bitCount (Gamma g) in
                print $ g * e

-- Part two

-- We start with the first digit, filter on most (or least) common,
-- we then repeat this until we only have a single number left, that is the chosen number.
-- most = Oxygen generator ration
-- least = co2 scrubber rating

mode' :: [Int] -> (Int, Int)
mode' xs = if (length $ filter (==1) xs) >= (length $ filter (==0) xs) then (1,0) else (0,1)

findValue :: ((Int, Int) -> Int) -> BitCount -> [[Int]] -> [Int]
findValue f (BitCount b) xs = go b 0 xs (transpose xs) where
    go :: Int -> Int -> [[Int]] -> [[Int]] -> [Int]
    go _ _ [x] _ = x
    go max i xs transposedXs =
        let mode = f $ mode' (transposedXs !! i) in
            let remainingXs = filter (\ x -> x !! i == mode) xs in
                go b (i+1) remainingXs (transpose remainingXs)

solvePartTwo :: IO ()
solvePartTwo = do
    lines <- getData "DayThreeData.txt"
    let bitCount = longestInput lines in
        let numbersAsDigits = (fmap . fmap) digitToInt lines in
            let o2 = findValue fst bitCount numbersAsDigits in
                let co2 = findValue snd bitCount numbersAsDigits in
                    print $ toInt o2 * toInt co2