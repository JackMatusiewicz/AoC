{-# LANGUAGE DeriveGeneric #-}

module DayTwo(
    solvePartOne,
    solvePartTwo
) where

--Turns out the Json was overkill but it was interesting to bring 3rd party deps into a Haskell project.

import Data.Aeson.Types (FromJSON)
import GHC.Generics
import Data.Aeson (ToJSON, encode)
import Paths_aoc ( getDataFileName )
import Data.Char (toUpper)

data Command =
    Forward Int
    | Up Int
    | Down Int
    deriving(Show, Generic, Read)

instance FromJSON Command
instance ToJSON Command

newtype Depth = Depth Int deriving (Eq, Show)
newtype Position = Position Int deriving(Eq, Show)

newtype SubmarineState =
    SubmarineState (Depth, Position) deriving (Eq, Show)

updateState :: SubmarineState -> Command -> SubmarineState
updateState (SubmarineState (Depth d, Position p)) c =
    case c of
        Forward n -> SubmarineState (Depth d, Position $ p + n)
        Up n -> SubmarineState (Depth $ d - n, Position p)
        Down n -> SubmarineState (Depth $ d + n, Position p)

calculateState :: SubmarineState -> [Command] -> SubmarineState
calculateState = foldl updateState

initialSubmarineState :: SubmarineState
initialSubmarineState = SubmarineState (Depth 0, Position 0)

capitalise :: String -> String
capitalise c = (toUpper . head) c : tail c

solvePartOne :: IO ()
solvePartOne = do
    filePath <- getDataFileName "DayTwoData.txt"
    text <- readFile filePath
    let SubmarineState (Depth d, Position p) =
            calculateState initialSubmarineState
            $ (read :: String -> Command)
            <$> reverse (capitalise <$> lines text) in
        print (d * p)

newtype Aim = Aim Int deriving(Eq, Show)
newtype SubmarineStateB = SubmarineStateB (Depth, Position, Aim) deriving(Eq, Show)

updateState' :: SubmarineStateB -> Command -> SubmarineStateB
updateState' (SubmarineStateB (Depth d, Position p, Aim a)) c =
    case c of
        Forward n -> SubmarineStateB (Depth $ d + (a * n), Position $ p + n, Aim a)
        Up n -> SubmarineStateB (Depth d, Position p, Aim $ a - n)
        Down n -> SubmarineStateB (Depth d, Position p, Aim $ a + n)

calculateState' :: SubmarineStateB -> [Command] -> SubmarineStateB
calculateState' = foldl updateState'

initialSubmarineBState = SubmarineStateB (Depth 0, Position 0, Aim 0)

solvePartTwo :: IO ()
solvePartTwo = do
    filePath <- getDataFileName "DayTwoData.txt"
    text <- readFile filePath
    let SubmarineStateB (Depth d, Position p, _) =
            calculateState' initialSubmarineBState
            $ (read :: String -> Command)
            <$> (capitalise <$> lines text) in
        print (d * p)