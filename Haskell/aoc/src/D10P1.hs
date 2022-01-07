module D10P1(solve) where

import Scaffolding
import Data.Foldable(foldl')
import qualified Data.List as DL

-- | The available bracket types for the puzzle.
data BracketType =
    Parens
    | Bracket
    | Angular
    | Square deriving(Eq, Ord, Show)

-- | A datatype representing a specific open or close bracket in the input.
data Bracket =
    Open BracketType
    | Close BracketType
    deriving (Eq, Ord, Show)

-- | Returns the bracket type for a given Bracket.
getBracketType :: Bracket -> BracketType
getBracketType (Open a) = a
getBracketType (Close a) = a

-- | A syntax error in a line
--
-- First bracket is the expected bracket (Nothing if there was an excess of close brackets).
--
-- Second bracket is the actual bracket.
data SyntaxError =
    BracketMismatch (Maybe BracketType) BracketType
    deriving(Eq, Ord, Show)

-- | A stack containing all brackets seen on a line so far.
newtype SeenBrackets = SeenBrackets [Bracket] deriving(Eq, Ord, Show)

mkBracket :: Char -> Bracket
mkBracket '(' = Open Parens
mkBracket ')' = Close Parens
mkBracket '{' = Open Bracket
mkBracket '}' = Close Bracket
mkBracket '<' = Open Angular
mkBracket '>' = Close Angular
mkBracket '[' = Open Square
mkBracket ']' = Close Square
mkBracket _ = error "Not a valid symbol"

-- | Checks if the provided brackets are a matching pair
--
-- The first parameter should be the bracket that appears first in the input.
isMatchingPair :: Bracket -> Bracket -> Bool
isMatchingPair (Open a) (Close b) = a == b
isMatchingPair _ _ = False

-- | Applies a bracket to the SeenBrackets
-- 
-- If it's an open bracket it's added to the stack
-- 
-- If it's a close bracket it will either return the stack with
-- the last item removed if it's an open bracket of the same type
-- or nothing if there is a mismatch
move :: SeenBrackets -> Bracket -> Either SyntaxError SeenBrackets
move (SeenBrackets t) (Open x) = Right $ SeenBrackets $ Open x : t
move (SeenBrackets []) (Close a) = Left $ BracketMismatch Nothing a
move (SeenBrackets (h:t)) x =
    if isMatchingPair h x then Right (SeenBrackets t)
    else Left $ BracketMismatch (Just $ getBracketType h) $ getBracketType x

-- | Takes a line of brackets and
-- checks to see if it is a valid line.
--
-- If nothing is returned then the line is invalid.
--
-- If a value is returned then the line is either valid or incomplete.
consumeLine :: [Bracket] -> Either SyntaxError SeenBrackets
consumeLine = foldl' (\s a -> s >>= flip move a) (Right $ SeenBrackets [])

-- | Calculate the score for a syntax error
scoreSyntaxError :: SyntaxError -> Int
scoreSyntaxError (BracketMismatch _ Parens) = 3
scoreSyntaxError (BracketMismatch _ Square) = 57
scoreSyntaxError (BracketMismatch _ Bracket) = 1197
scoreSyntaxError (BracketMismatch _ Angular) = 25137

-- | Takes a list of eithers and returns a list of all of the Left values.
catLeft :: [Either a b] -> [a]
catLeft ((Left a) : t) = a : catLeft t
catLeft (_ : t) = catLeft t
catLeft [] = []

-- | For a given input, calculate the total score of all of the corrupted lines.
calculateScoreOfInput :: [[Bracket]] -> Int
calculateScoreOfInput =  sum . fmap scoreSyntaxError . catLeft . fmap consumeLine

-- | Construct the list of bracket symbols from a string.
format :: String -> [Bracket]
format = fmap mkBracket

-- PART TWO SPECIFIC CODE

-- | Returns True if a given SeenBrackets stack represents an
-- incomplete line.
isIncompleteLine :: SeenBrackets -> Bool
isIncompleteLine (SeenBrackets d) = not $ null d

-- | Takes a list of eithers and returns a list of all of the Right values.
catRight :: [Either a b] -> [b]
catRight [] = []
catRight ((Right b) : t) = b : catRight t
catRight (_ : t) = catRight t

-- | Gets all incomplete lines from a set of parsed inputs.
getIncompleteLines :: [Either a SeenBrackets] -> [SeenBrackets]
getIncompleteLines = filter isIncompleteLine . catRight

-- | For a given SeenBrackets, return the list of brackets that completes the line.
findMissingLineSegment :: SeenBrackets -> [Bracket]
findMissingLineSegment (SeenBrackets d) = go d
    where
        go :: [Bracket] -> [Bracket]
        go [] = []
        go ((Open a) : t) = Close a : go t
        go ((Close a) : t) = Open a : go t

-- | The scoring system for bracket types in part two.
calculatePartTwoBracket :: BracketType -> Int
calculatePartTwoBracket Parens = 1
calculatePartTwoBracket Square = 2
calculatePartTwoBracket Bracket = 3
calculatePartTwoBracket Angular = 4

-- | For a given missing segment, calculate the total
-- score of the segment.
calculateScoreOfMissingSegment :: [Bracket] -> Int
calculateScoreOfMissingSegment =
    foldl' (\s a -> s * 5 + a) 0
    . fmap (calculatePartTwoBracket . getBracketType)

solvePartTwo :: [[Bracket]] -> Int
solvePartTwo =
    (\a -> middleScore (div (length a) 2) a)
    . DL.sort
    . fmap (calculateScoreOfMissingSegment . findMissingLineSegment)
    . getIncompleteLines
    . fmap consumeLine

    where
        middleScore :: Int -> [Int] -> Int
        middleScore 0 [] = error "Impossible"
        middleScore n [] = error "Impossible"
        middleScore 0 (h:t) = h
        middleScore n (h:t) = middleScore (n-1) t

solve :: IO ()
solve = do
    v <- getData "DayTen.txt"
    print $ solvePartTwo $ fmap format v