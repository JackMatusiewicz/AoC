module D4P1
(

) where

import Data.Vector (Vector)
import Data.Matrix
import Data.Foldable as Df

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

data BingoCell = Marked Int | Unmarked Int

newtype BingoBoard = BingoBoard (Matrix BingoCell)

data BingoSystem = BingoSystem { picked :: [Picked], boards :: [BingoBoard] }

newtype Picked = Picked Int

{-
N.B There is an optimisation to be made here.
Rather than calculating the scores each time, we can store the number of marked values in each row and column, that way it's
far fewer checks each iteration.
-}

isMarked :: BingoCell -> Bool
isMarked c = case c of
    (Marked _) -> True
    _ -> False

isWinningLine :: Vector BingoCell -> Bool
isWinningLine = foldr (\a s -> isMarked a && s) True

markBoard :: Picked -> BingoBoard -> BingoBoard
markBoard (Picked v) (BingoBoard xs) = BingoBoard $ fmap (go v) xs
    where
        go :: Int -> BingoCell -> BingoCell
        go v (Unmarked x)
            | x == v = Marked x
            | otherwise = Unmarked x
        go _ x = x

isWinningBoard :: BingoBoard -> Bool
isWinningBoard (BingoBoard x) =
    let checks = [flip getRow x, flip getCol x]
    in or $ isWinningLine <$> (checks <*> [1 .. 5])

findWinningBoards :: BingoSystem -> [BingoBoard]
findWinningBoards = filter isWinningBoard . boards

addPickedNumber :: Picked -> BingoSystem -> BingoSystem
addPickedNumber p bs =
    BingoSystem {
        picked = p : picked bs,
        boards = markBoard p <$> boards bs
    }