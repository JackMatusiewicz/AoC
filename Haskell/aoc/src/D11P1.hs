module D11P1(solve) where

import Data.Char(digitToInt)
import qualified Data.Matrix as DM
import qualified Data.Foldable as DF
import Scaffolding

import System.IO.Unsafe(unsafePerformIO)
import Control.DeepSeq(deepseq)

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
addDelta :: Delta -> (Int, Int) -> (Int, Int)
addDelta (Delta (c,d)) (a,b) = (a + c, b + d)

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
        go flashed allCoords c =
            let flashedCavern = DF.foldl' setFlashed c flashed
                allNeighbours = addDelta <$> adjacentDeltas <*> flashed
                (updated2, toFlash) = DF.foldl' updateIncrement (flashedCavern, []) allNeighbours
            in go toFlash allCoords updated2

        -- | Updates an octopus that is adjacent to a flasher, will also add them to a list
        -- of "to flash" if it exceeds the threshold.
        updateIncrement :: (Cavern, [(Int, Int)]) -> (Int, Int) -> (Cavern, [(Int, Int)])
        updateIncrement (c, toFlash) p =
            let (nc, addFlash) = updatePosition c p
            in (nc, if addFlash then p:toFlash else toFlash)

        shouldFlash :: Cavern -> (Int, Int) -> Bool
        shouldFlash (Cavern c) p =
            case c DM.! p of
                Flashed -> False
                Charging n -> n > 9

        -- | Updates the charging value at a particular cell.
        --
        -- It returns the updated cavern and whether or not this position
        -- itself needs to flash.
        updatePosition :: Cavern -> (Int,Int) ->  (Cavern, Bool)
        updatePosition (Cavern c) (x,y) =
            case DM.safeGet x y c of
                Nothing -> (Cavern c, False)
                Just Flashed -> (Cavern c, False)
                Just (Charging n) ->
                    let uc = Cavern $ DM.setElem (Charging $ n + 1) (x,y) c
                    in (uc, n == 9)

        setFlashed :: Cavern -> (Int, Int) -> Cavern
        setFlashed (Cavern c) (x,y) =
            case DM.safeGet x y c of
                Nothing -> Cavern c
                _ -> Cavern $ DM.setElem Flashed (x,y) c

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

toInt :: Octopus -> Int
toInt (Charging n) = n
toInt Flashed = error "no"

flashesAfterNTicks :: Int -> Cavern -> Int
flashesAfterNTicks n = go n 0
    where
        go :: Int -> Int -> Cavern -> Int
        go 0 acc _ = acc
        go n acc c =
            let uc = allTicks c
            in go (n-1) (acc + flashesFromPreviousTick uc) uc

 -- PART TWO SPECIFIC CODE
 -- Before the third part of the tick, count the number of octopi that have flashed.

ticksUntilAllFlash :: Cavern -> Int
ticksUntilAllFlash (Cavern c) =
    go 0 (DM.nrows c * DM.ncols c) (flashesFromPreviousTick (Cavern c)) (Cavern c)
    where
        go :: Int -> Int -> Int -> Cavern -> Int
        go iter tot lastFlashed c
            | tot == lastFlashed = iter
            | otherwise =
                let uc = allTicks c
                in go (iter + 1) tot (flashesFromPreviousTick uc) uc

solve :: IO ()
solve = do
    v <- getData "DayEleven.txt"
    print $ show $ ticksUntilAllFlash $ makeCavern v