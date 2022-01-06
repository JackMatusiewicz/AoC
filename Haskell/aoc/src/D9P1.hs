module D9P1(solve) where

import Scaffolding
import qualified Data.Matrix as DM
import Data.Maybe (fromMaybe)
import Data.Char (digitToInt)

-- | All four directions around a cell.
allDirections = [(1,0), (-1,0), (0,1), (0,-1)]

sumCoords :: (Num a, Num b) => (a,b) -> (a,b) -> (a,b)
sumCoords (a,b) (c,d) = (a + c, b + d)

-- | Given an (x,y) point in a matrix, determine if
-- it is the lowest point compared to its adjacents.
isLowestPoint :: (Int, Int) -> DM.Matrix Int -> Bool
isLowestPoint (x,y) m =
    and $ checkItem (m DM.! (x,y)) m . sumCoords (x,y) <$> allDirections
    where
        checkItem :: Int -> DM.Matrix Int -> (Int, Int) ->  Bool
        checkItem v m (x,y) = maybe True (> v) $ DM.safeGet x y m

-- | Gets all possible coordinates from a matrix
getAllCoords :: DM.Matrix Int -> [(Int, Int)]
getAllCoords m = (,) <$> [1 .. DM.nrows m] <*> [1 .. DM.ncols m]

-- | Finds the sum of all of the lowest points in the matrix.
findSumOfLowestPoints :: [(Int, Int)] -> DM.Matrix Int -> Int
findSumOfLowestPoints points m = sum $ (+) 1 . (m DM.! ) <$> filter (`isLowestPoint` m) points

solvePuzzle :: DM.Matrix Int -> Int
solvePuzzle m = findSumOfLowestPoints (getAllCoords m) m


-- | Simple helper function to convert
-- the input into a matrix of ints.
toGrid :: [String] -> DM.Matrix Int
toGrid = DM.fromLists . (fmap . fmap) digitToInt

solve :: IO ()
solve = do
    v <- getData "DayNine.txt"
    print $ (show . solvePuzzle . toGrid) v