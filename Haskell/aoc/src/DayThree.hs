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