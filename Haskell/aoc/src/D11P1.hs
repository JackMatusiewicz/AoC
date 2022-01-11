module D11P1(solve) where

import Data.Char(digitToInt)
import qualified Data.Matrix as DM
import qualified Data.Foldable as DF
import Scaffolding

-- | Datatype modelling the state of an octopus.
--
-- It is either charging (with a value of 0-9) and when it finally reaches above level 9 it flashes.
data Octopus =
    Charging Int
    | Flashed
    deriving(Eq, Ord, Show)

-- | Datatype modelling the octopi on the cavern floor.
newtype Cavern = Cavern (DM.Matrix Octopus)
    deriving (Eq, Show)

newtype Delta = Delta (Int, Int) deriving (Eq, Ord, Show)

-- | A set of coordinate deltas that when fmapped to a position will return
-- all possible neighbouring cells.
adjacentDeltas :: [Delta]
adjacentDeltas =
    fmap Delta [(x,y) | x <- [-1..1], y <- [-1..1], (x,y) /= (0,0)]

unwrapCavern :: Cavern -> DM.Matrix Octopus
unwrapCavern (Cavern c) = c

-- | Adds a delta to a position and returns the new position.
addDelta :: (Int, Int) -> Delta -> (Int, Int)
addDelta (a,b) (Delta (c,d)) = (a + b, c + d)

format :: String -> [Int]
format = fmap digitToInt

-- | Given the lines of the input, constructs a cavern object
-- representing the octopi and their light levels.
makeCavern :: [String] -> Cavern
makeCavern = Cavern . DM.fromLists . fmap (fmap Charging . format)

-- | Increments an octopus' power level.
--
-- If the octopus has already flashed, it will not increment it more.
incrementLevel :: Octopus -> Octopus
incrementLevel (Charging n) = Charging $ n + 1
incrementLevel c = c

-- | Resets an octopus, setting its power level to 0 if it has flashed.
resetOctopus :: Octopus -> Octopus
resetOctopus Flashed = Charging 0
resetOctopus c = c

-- | Increments all octopi power levels by 1.
tickPartOne :: Cavern -> Cavern
tickPartOne = Cavern . fmap incrementLevel . unwrapCavern

-- | Returns all valid positions in a matrix
allCoords :: DM.Matrix a -> [(Int, Int)]
allCoords m = [(x,y) | x <- [1 .. DM.nrows m], y <- [1 .. DM.ncols m]]

-- | Flashes any octopi that have a power level greater than 9.
--
-- It then applies the levels to neighbours and repeats until there are no changes.
tickPartTwo :: Cavern -> Cavern
tickPartTwo c =
    let ac = (allCoords $ unwrapCavern c)
    in go (filter (shouldFlash c) ac) ac c
    where
        go :: [(Int, Int)] -> [(Int, Int)] -> Cavern -> Cavern
        go [] _ c = c
        go flashed allCoords c = undefined

        shouldFlash :: Cavern -> (Int, Int) -> Bool
        shouldFlash (Cavern c) p =
            case c DM.! p of
                Flashed -> False
                Charging n -> n > 9

-- | Resets the power levels of any octopi that have flashed this step.
tickPartThree :: Cavern -> Cavern
tickPartThree = Cavern . fmap resetOctopus . unwrapCavern

-- | Applies all ticks to the cavern.
allTicks :: Cavern -> Cavern
allTicks = tickPartThree . tickPartTwo . tickPartOne

-- | Will count the number of flashes that occured in the previous tick.
flashesFromPreviousTick :: Cavern -> Int
flashesFromPreviousTick = DF.foldl' (\s a -> s + didFlash a) 0 . unwrapCavern
    where
        didFlash :: Octopus -> Int
        didFlash (Charging 0) = 1
        didFlash _ = 0

flashesAfterNTicks :: Int -> Cavern -> Int
flashesAfterNTicks n = go n 0
    where
        go :: Int -> Int -> Cavern -> Int
        go 0 acc _ = acc
        go n acc c =
            let uc = allTicks c
            in go (n-1) (acc + flashesFromPreviousTick uc) uc

solve :: IO ()
solve = do
    v <- getData "DayEleven.txt"
    print $ show $ flashesAfterNTicks 100 $ makeCavern v