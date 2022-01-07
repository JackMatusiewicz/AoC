module D10P1(solve) where

import Scaffolding
import Data.Foldable(foldl')

data BracketType =
    Parens
    | Bracket
    | Angular
    | Square deriving(Eq, Ord, Show)

data Bracket =
    Open BracketType
    | Close BracketType
    deriving (Eq, Ord, Show)

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

format :: [Char] -> [Bracket]
format = fmap mkBracket

isMatchingPair :: Bracket -> Bracket -> Bool
isMatchingPair (Open Parens) (Close Parens) = True
isMatchingPair (Open Angular) (Close Angular) = True
isMatchingPair (Open Bracket) (Close Bracket) = True
isMatchingPair (Open Square) (Close Square) = True
isMatchingPair _ _ = False

-- | Applies a bracket to the SeenBrackets
-- 
-- If it's an open bracket it's added to the stack
-- 
-- If it's a close bracket it will either return the stack with
-- the last item removed if it's an open bracket of the same type
-- or nothing if there is a mismatch
move :: SeenBrackets -> Bracket -> Maybe SeenBrackets
move (SeenBrackets t) (Open x) = Just $ SeenBrackets $ Open x : t
move (SeenBrackets []) (Close _) = Nothing
move (SeenBrackets (h:t)) x =
    if isMatchingPair x h then Just (SeenBrackets t) else Nothing

-- | Takes a line of brackets and
-- checks to see if it is a valid line.
--
-- If nothing is returned then the line is invalid.
--
-- If a value is returned then the line is either valid or incomplete.
consumeLine :: [Bracket] -> Maybe SeenBrackets
consumeLine = foldl' (\s a -> s >>= flip move a) (Just $ SeenBrackets [])

isIncompleteLine :: [Bracket] -> Bool
isIncompleteLine = undefined

solve :: IO ()
solve = do
    v <- getData "DayTen.txt"
    print "no"