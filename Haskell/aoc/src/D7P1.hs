module D7P1(solve) where

import Scaffolding
import Data.List.Split(splitOn)
import Control.Monad(join)
import qualified Data.Map as DM
import Data.Array
import Data.Foldable
import Data.Functor(($>))

newtype FleetMoveDistance = FleetMoveDistance Int deriving(Eq, Show)
newtype NewPosition = NewPosition Int deriving(Eq, Ord, Show)

newtype ResultCache =
    ResultCache (DM.Map NewPosition FleetMoveDistance) deriving(Eq, Show)

newtype Range = Range(Int, Int) deriving(Eq, Ord, Show)

-- | Describes the current value on the curve.
data Point =
    Increasing
    | Decreasing
    -- | This means we have found the point we are looking for.
    | Inflexion
    deriving(Eq, Show)

-- | Attempts to find a value in the result cache.
--
-- If a value is not present then it calls the calculation function and populates the cache.
getValueFromCache :: ResultCache
    -> NewPosition
    -> (() -> FleetMoveDistance)
    -> (ResultCache, FleetMoveDistance)
getValueFromCache (ResultCache rc) np calc =
    if DM.member np rc then
        (ResultCache rc, rc DM.! np)
    else
        let v = calc ()
        in (ResultCache $ DM.insert np v rc, v)

-- | Finds the minimum and maximum elements of the input list.
-- Will return nothing if an empty list is provided.
findRange :: [Int] -> Maybe Range
findRange = go Nothing
    where
        go :: Maybe Range -> [Int] -> Maybe Range
        go x [] = x
        go Nothing (h:t) =
            go (Just $ Range (h,h)) t
        go (Just (Range (a,b))) (h:t) =
            go (Just $ Range (min a h, max b h)) t

calculateMoveCost :: NewPosition -> Array Int Int -> FleetMoveDistance
calculateMoveCost (NewPosition x) =
    FleetMoveDistance . foldl' (\s a -> s + abs (a - x)) 0

-- | This doesn't account for things being equal. For now I am assuming that
-- is not possible.
calculateDirection :: (FleetMoveDistance, FleetMoveDistance, FleetMoveDistance) -> Point
calculateDirection (FleetMoveDistance x, FleetMoveDistance y, FleetMoveDistance z)
    | x > y && y  > z = Decreasing
    | x < y && y < z = Increasing
    | otherwise = Inflexion

-- Turns out this was probably massively overkill for the dataset... at least it was fun!
findPositionToMove :: ResultCache
    -> Range
    -> Array Int Int
    -> Range
    -> Maybe FleetMoveDistance
findPositionToMove rc (Range (first, last)) xs (Range (a,b))
    | a > b = Nothing
    | otherwise =
        let mp = div (b + a) 2
            (rc1, nMinusOne) =
                getValueFromCache
                    rc
                    (NewPosition $ wrap first last (mp-1))
                    (\() -> calculateMoveCost (NewPosition $ wrap first last (mp-1)) xs)
            (rc2, n) =
                getValueFromCache
                    rc1
                    (NewPosition mp)
                    (\() -> calculateMoveCost (NewPosition mp) xs)
            (rc3, nPlusOne) =
                getValueFromCache
                    rc2
                    (NewPosition $ wrap first last (mp+1))
                    (\() -> calculateMoveCost (NewPosition $ wrap first last (mp+1)) xs)
            direction = calculateDirection (nMinusOne, n, nPlusOne)
            in
            case direction of
                Inflexion -> Just n
                Increasing -> findPositionToMove rc3 (Range (first, last)) xs (Range (a,mp-1))
                Decreasing -> findPositionToMove rc3 (Range (first, last)) xs (Range (mp+1,b))
    where
        wrap :: Int -> Int -> Int -> Int
        wrap min max b
            | b > max = min
            | b < min = max
            | otherwise = b

formatInput :: [String] -> (Array Int Int, Maybe Range)
formatInput =
    (\a -> (listArray (0, length a - 1) a, findRange a))
    . join
    . fmap (fmap read . splitOn ",")

todo :: (a, Maybe b) -> (a,b)
todo (a, Just v) = (a,v)
todo _ = error "Unreachable"

solve :: IO ()
solve = do
    v <- getData "DaySeven.txt"
    let x = (\(a,b) -> findPositionToMove (ResultCache DM.empty) b a b) =<< sequence (formatInput v)
        in print x