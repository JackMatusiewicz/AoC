module DayThree where

import Data.Bits

newtype BitCount = BitCount Int deriving(Eq, Show)
newtype Gamma = Gamma Int deriving(Eq, Show)
newtype Epsilon = Epsilon Int deriving(Eq, Show)

-- Returns the length of the longest input string
longestInput :: [String] -> BitCount
longestInput = BitCount . foldr (max . length) 0

getEpsilon :: BitCount -> Gamma -> Epsilon
getEpsilon (BitCount b) (Gamma g) = Epsilon $ xor g (2^b - 1)

calculateGamma :: BitCount -> [Int] -> Gamma
calculateGamma _ _ = error "todo"