{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}

module D4P1
(
    solve
) where

import Data.Vector (Vector)
import Data.Matrix
import Data.Foldable as Df
import Scaffolding
import Data.List(partition)
import Data.Semigroup

{-
So, a bingo board is a 5x5 grid of numbers.
Numbers are picked, and matching numbers on boards are marked

If a board has a full row _or_ a full column, then it is a winning board.

In the case of a winning board:
Start by finding the sum of all unmarked numbers on that board;
Then, multiply that sum by the number that was just called when the board won.

The first line of the input is a comma separated list of numbers: the numbers that are picked.
Then there is an empty line.

What follows is a set of 5 lines, representing a board, and then an empty line (which terminates the previous board)
However, the final board is followed by the EOF marker.

So, design:

A bingo system contains a list of picked numbers and a set of boards.
A bingo board has 25 cells
A cell is a number and either marked or unmarked
-}

data BingoCell = Marked Int | Unmarked Int deriving(Eq, Show)

newtype BingoBoard = BingoBoard (Matrix BingoCell) deriving(Eq, Show)

newtype Picked = Picked Int deriving(Eq, Show)

data BingoSystem = BingoSystem {
    picked :: [Picked],
    boards :: [BingoBoard],
    winningBoards :: [BingoBoard]
} deriving(Eq, Show)

{-
N.B There is an optimisation to be made here.
Rather than calculating the scores each time, we can store the number of marked values in each row and column, that way it's
far fewer checks each iteration.
-}

isWinningLine :: Vector BingoCell -> Bool
isWinningLine = foldr (\a -> (isMarked a &&)) True
    where
        isMarked :: BingoCell -> Bool
        isMarked c = case c of
            (Marked _) -> True
            _ -> False

-- | Marks all cells of a BingoBoard that have the same value
-- of the picked argument.
markBoard :: Picked -> BingoBoard -> BingoBoard
markBoard (Picked v) (BingoBoard xs) = BingoBoard $ fmap (go v) xs
    where
        go :: Int -> BingoCell -> BingoCell
        go v (Unmarked x)
            | x == v = Marked x
            | otherwise = Unmarked x
        go _ x = x

-- | Checks if a BingoBoard is in a winning state.
--
-- This is decided if any row or column (not diagonal) has all of its cells marked.
isWinningBoard :: BingoBoard -> Bool
isWinningBoard (BingoBoard x) =
    let checks = [flip getRow x, flip getCol x]
    in or $ isWinningLine <$> (checks <*> [1 .. 5])

-- | Adds a number that was picked to an existing BingoSystem, updating
-- all of the boards.
--
-- Will run checks to see if any boards are now in winning states.
addPickedNumber :: Picked -> BingoSystem -> BingoSystem
addPickedNumber p bs =
    let (winningBoards, updatedBoards) =
            partition isWinningBoard $ markBoard p <$> boards bs
    in BingoSystem {
        picked = p : picked bs,
        boards = updatedBoards,
        winningBoards = winningBoards
    }

-- | Converts a set of matrices into a BingoSystem.
-- Will return nothing if any of the matrices are not 5x5.
makeSystem :: [Matrix Int] -> Maybe BingoSystem
makeSystem xs = mk <$> traverse makeBoard xs
    where
        makeBoard :: Matrix Int -> Maybe BingoBoard
        makeBoard i =
            if nrows i == 5 && ncols i == 5 then
                Just $ BingoBoard (Unmarked <$> i)
            else Nothing

        mk :: [BingoBoard] -> BingoSystem
        mk b = BingoSystem { picked = [], winningBoards = [], boards = b }

-- | Chunks a list into a list of lists where each sub list
-- has at most n elements.
chunks :: Int -> [a] -> [[a]]
chunks n xs = take n xs : chunks n (drop n xs)

split :: Char -> String -> [String]
split c xs = go [] xs
    where
        go :: String -> String -> [String]
        go _ [] = []
        go v (x:xs)
            | x == c = reverse v : go [] xs
            | otherwise = go (x:v) xs

fromInput :: [String] -> Maybe ([Picked], BingoSystem)
fromInput lines =
    let (picked, boards) = (head lines, chunks 5 $ tail lines)
    in
        (toPicked picked,) <$> (makeSystem . toBoards) boards
    where
        toPicked :: String -> [Picked]
        toPicked = fmap (Picked . (read :: String -> Int)) . split ','

        toBoards :: [[String]] -> [Matrix Int]
        toBoards =
            fmap (fromLists . fmap (fmap (read :: String -> Int) . split ' '))

findWinningBoard :: [Picked] -> BingoSystem -> Maybe (Picked, BingoBoard)
findWinningBoard [] _ = Nothing
findWinningBoard (p:ps) sys = go (addPickedNumber p sys)
    where
        go :: BingoSystem -> Maybe (Picked, BingoBoard)
        go sys
         | not (null (winningBoards sys)) =
             (Just . (p,) . head . winningBoards) sys
         | otherwise = findWinningBoard ps sys

calculate :: Picked -> BingoBoard -> Int
calculate (Picked p) (BingoBoard b) =
    (p*) $ getSum $ foldMap get b
    where
        get :: BingoCell -> Sum Int
        get (Marked x) = Sum { getSum = x }
        get (Unmarked _) = mempty


solve :: IO ()
solve = do
    v <- getData "DayFour.txt"
    print $ uncurry calculate <$> (fromInput v >>= uncurry findWinningBoard)