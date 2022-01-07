module D9P2(solve) where

import qualified Data.Matrix as DM
import Data.Maybe (fromMaybe, isNothing, catMaybes)
import Data.Char (digitToInt)
import qualified Data.List as DL
import qualified Data.Set as DS
import Scaffolding(getData)
import qualified Data.PQueue.Min as PQ

-- Very handy for debugging, unsafe for unsafePerformIO and deepseq to force evaluation to normal form.
import System.IO.Unsafe(unsafePerformIO)
import Control.DeepSeq(deepseq)

-- | A point to check in the basin algorithm
-- the first Int is the manhattan distance between
data PointToCheck = PointToCheck Int (Int,Int) deriving(Eq, Show)

instance Ord PointToCheck where
    compare (PointToCheck a _) (PointToCheck b _) = compare a b


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
        checkItem v m = maybe True (> v) . (flip $ uncurry DM.safeGet) m

-- | Gets all possible coordinates from a matrix
getAllCoords :: DM.Matrix Int -> [(Int, Int)]
getAllCoords m = (,) <$> [1 .. DM.nrows m] <*> [1 .. DM.ncols m]

-- | Finds the coordinates of all lowest points.
findLowestPointCoords :: DM.Matrix Int -> [(Int, Int)] -> [(Int, Int)]
findLowestPointCoords m = filter (`isLowestPoint` m)

manhattanDistance :: Num a => (a,a) -> (a,a) -> a
manhattanDistance (a,b) (c,d) = abs (a-c) + abs (b-d)

-- | For a given lowest point, find all of the points in its basin
findBasin :: DM.Matrix Int -> (Int, Int) -> DS.Set (Int, Int)
findBasin m o = go DS.empty DS.empty m (PQ.insert (PointToCheck 0 o) PQ.empty)
    where
        go :: DS.Set (Int, Int) -> DS.Set (Int, Int) -> DM.Matrix Int -> PQ.MinQueue PointToCheck -> DS.Set (Int, Int)
        go visited basin m pq =
            case PQ.minView pq of
                Nothing -> basin
                (Just (PointToCheck _ h, newPq)) ->
                    -- If we've already been to the the cell or it isn't in the matrix, bail
                    if DS.member h visited || isInvalidBasinValue (uncurry DM.safeGet h m) then
                        go visited basin m newPq
                    else
                    -- get all neighbours we haven't seen before.
                    let neighbours = filter (not . flip DS.member visited) $ sumCoords h <$> allDirections
                        smallerNeighbours = or $ checkItem (m DM.! h) m <$> neighbours
                    in
                        if smallerNeighbours then
                            go visited basin m newPq
                        else
                        go
                            (DS.insert h visited)
                            (DS.insert h basin)
                            m
                            (PQ.union newPq (PQ.fromList $ fmap mkPointToCheck neighbours))

        checkItem :: Int -> DM.Matrix Int -> (Int, Int) ->  Bool
        checkItem v m = maybe False (< v) . (flip $ uncurry DM.safeGet) m

        isInvalidBasinValue :: Maybe Int -> Bool
        isInvalidBasinValue Nothing = True
        isInvalidBasinValue (Just 9) = True
        isInvalidBasinValue _ = False

        mkPointToCheck :: (Int, Int) -> PointToCheck
        mkPointToCheck v = PointToCheck (manhattanDistance o v) v


solvePuzzle :: DM.Matrix Int -> Int
solvePuzzle m =
    product
        . take 3
        . DL.sortOn negate
    $ DS.size
        . findBasin m
    <$> findLowestPointCoords m (getAllCoords m)

-- | Simple helper function to convert
-- the input into a matrix of ints.
toGrid :: [String] -> DM.Matrix Int
toGrid = DM.fromLists . (fmap . fmap) digitToInt

solve :: IO ()
solve = do
    v <- getData "DayNine.txt"
    print $ (show . solvePuzzle . toGrid) v