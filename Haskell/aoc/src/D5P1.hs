module D5P1 (solve) where

import Scaffolding
import Data.Set(Set, notMember, fromList)
import Utils(chunks)
import Data.List (elemIndex)
import Data.Bifunctor
import Control.Monad((>=>))
import GHC.Base (Applicative(liftA2))
import Data.Char(isNumber)
import qualified Data.Map as DM
import Data.Foldable

newtype Point = Point (Int, Int) deriving(Eq, Ord, Show)

newtype Grid = Grid (DM.Map Point Int) deriving(Eq, Show)

removeArrows :: String -> String
removeArrows = filter (`notMember` fromList "->")

onlyDigits :: String -> String
onlyDigits = filter isNumber

-- | We have a string of something similar to "0,9"
--
-- This will split it on the ',', remove the ',' from the output
-- and then convert to valid numbers and then to a Point.
parsePoint :: String -> Maybe Point
parsePoint xs =
    Point . bimap (read . onlyDigits) (read . onlyDigits)
    <$> (flip splitAt xs <$> elemIndex ',' xs)

tupleUp :: [a] -> Maybe (a,a)
tupleUp [a,b] = Just (a,b)
tupleUp _ = Nothing

formatInput :: String -> Maybe (Point, Point)
formatInput =
    fmap head
    . sequence
    . fmap (>>= tupleUp)
    . fmap sequence
    . (fmap . fmap) parsePoint
    . chunks 2
    . words
    . removeArrows

ignoreDiagonals :: [(Point, Point)] -> [(Point, Point)]
ignoreDiagonals = filter (\ (Point (x1, y1), Point (x2, y2)) -> x1 == x2 || y1 == y2)

-- | Updates the grid with the specified point.
update :: Point -> Grid -> Grid
update pt (Grid m)
    | DM.member pt m = Grid $ DM.adjust (+1) pt m
    | otherwise = Grid $ DM.insert pt 1 m

generateLine :: (Point, Point) -> [Point]
generateLine (a,b) =
    let (Point (sx, sy), Point (ex, ey)) = order a b
    in [Point(x,y) | x <- [sx .. ex], y <- [sy .. ey]]
    where
        order :: Point -> Point -> (Point, Point)
        order a b
            | a < b = (a,b)
            | otherwise = (b,a)

fillGrid :: [Point] -> Grid
fillGrid = foldl' (flip update) (Grid DM.empty)

findPointsGreaterThanOne :: Grid -> Int
findPointsGreaterThanOne (Grid m) =
    DM.size $ DM.filter (>1) m

solve :: IO ()
solve = do
    v <- getData "DayFive.txt"
    print
        $ show
        $ (findPointsGreaterThanOne . fillGrid . (>>= generateLine))
            . ignoreDiagonals
            <$> traverse formatInput v