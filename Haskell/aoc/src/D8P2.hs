module D8P2(solve) where

import Scaffolding
import Data.List.Split (splitOn)
import qualified Data.Map as DM
import qualified Data.Set as DS
import Data.Foldable(foldl')
import Data.Bifunctor(bimap)
import Data.Char(toUpper)
import Data.List(sortOn)
import Utils

-- | Simple encoding of the different wires.
data Wire = A | B | C | D | E | F | G deriving(Eq, Ord, Show)

-- | Simple encoding of the possible digits
data Digit =
    Zero
    | One
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    deriving(Eq, Ord, Show)

-- | Represents the wires used for specific digits.
newtype Mapping =
    Mapping (DM.Map Digit (DS.Set Wire))
    deriving (Eq, Show)

-- | An input pattern that will represent a number.
newtype Pattern = Pattern (DS.Set Wire) deriving(Eq, Ord, Show)

data Input = Input { patterns :: [Pattern], chosenDigits :: [Pattern] }
    deriving(Eq, Show)

mkInput :: [Pattern] -> [Pattern] -> Input
mkInput pat cho = Input { patterns = pat, chosenDigits = cho }

-- | Convert a character to a Wire.
--
-- Will fail if outside the range of [A-G]
mkWire :: Char -> Wire
mkWire 'A' = A
mkWire 'B' = B
mkWire 'C' = C
mkWire 'D' = D
mkWire 'E' = E
mkWire 'F' = F
mkWire 'G' = G
mkWire s = error $ show s ++ " is not a valid Wire."

-- | The correct mapping for digits.
--
-- This isn't actually needed anywhere but saves me from flicking
-- back to the advent page.
actualDigitMapping =
    Mapping
        $ foldl'
            (flip (uncurry DM.insert))
            DM.empty
            [
                (Zero, DS.fromList [A,B,C,E,F,G]),
                (One, DS.fromList [C,F]),
                (Two, DS.fromList [A,C,D,E,G]),
                (Three, DS.fromList [A,C,D,F,G]),
                (Four, DS.fromList [B,C,D,F]),
                (Five, DS.fromList [A,B,D,F,G]),
                (Six, DS.fromList [A,B,D,F,G,E]),
                (Seven, DS.fromList [A,C,F]),
                (Eight, DS.fromList [A,B,C,D,E,F,G]),
                (Nine, DS.fromList [A,B,C,D,F,G])
            ]

-- | Converts a single line of input into the expected input for that screen.
formatLine :: String -> Maybe Input
formatLine = fmap (uncurry mkInput . bimap toPatterns toPatterns) . toTuple . splitOn "|"
    where
        toTuple :: [String] -> Maybe (String, String)
        toTuple [a,b] = Just (a,b)
        toTuple _ = Nothing

        toPatterns :: String -> [Pattern]
        toPatterns = fmap (Pattern . DS.fromList . fmap mkWire) . words . fmap toUpper

-- | Finds 1,4,7 and 8 from the input patterns.
--
-- Returns a map containing 1,4,7, and 8 and also a set of the
-- remaining patterns.
findUniquePieces :: DS.Set Pattern -> (DM.Map Digit Pattern, DS.Set Pattern)
findUniquePieces = go
    where
        go :: DS.Set Pattern -> (DM.Map Digit Pattern, DS.Set Pattern)
        go =
            foldl'
                (\s a ->
                    either
                        (\x -> rmap (DS.insert x) s)
                        (\b -> lmap (uncurry DM.insert b) s) a)
                (DM.empty, DS.empty)
            . fmap matchUniquePattern
            . DS.toList

        matchUniquePattern :: Pattern -> Either Pattern (Digit, Pattern)
        matchUniquePattern (Pattern wires)
            | DS.size wires == 2 = Right (One, Pattern wires)
            | DS.size wires == 3 = Right (Seven, Pattern wires)
            | DS.size wires == 4 = Right (Four, Pattern wires)
            | DS.size wires == 7 = Right (Eight, Pattern wires)
            | otherwise = Left $ Pattern wires

-- | Given a Map of unique digit to pattern mappings and a Set of remining patterns, finish
-- the mapping.
findNonUniquePieces :: DM.Map Digit Pattern -> DS.Set Pattern -> DM.Map Digit Pattern
findNonUniquePieces m = foldl' insert m . sortOn (\(Pattern w) -> negate $ DS.size w). DS.toList
    where
        insert :: DM.Map Digit Pattern -> Pattern -> DM.Map Digit Pattern
        insert m p
            | wiresUsed p == 5 && contained (m DM.! One) p && contained (m DM.! Seven) p
                = DM.insert Three p m
            | wiresUsed p == 6 && contained (m DM.! Four) p && contained (m DM.! Seven) p
                = DM.insert Nine p m
            | wiresUsed p == 6 && contained (m DM.! Seven) p
                = DM.insert Zero p m
            | wiresUsed p == 6 && not (contained (m DM.! One) p)
                = DM.insert Six p m
            | wiresUsed p == 5 && contained p (m DM.! Six)
                = DM.insert Five p m
            | wiresUsed p == 5 && not (contained p (m DM.! Six))
                = DM.insert Two p m
            | otherwise = error $ "Unable to find a digit for " ++ show p

        -- | Checks if the first pattern is contained in the second.
        contained :: Pattern -> Pattern -> Bool
        contained (Pattern a) (Pattern b) = DS.isSubsetOf a b

        wiresUsed (Pattern w) = DS.size w

-- | Given a set of input patterns. Find the digit mapping
-- using the helper functions: findUniquePieces and findNonUniquePieces
findDigitMapping :: DS.Set Pattern -> DM.Map Digit Pattern
findDigitMapping = uncurry findNonUniquePieces . findUniquePieces

-- | Reverses the Digit -> Pattern mapping, which is fine
-- as we know it is a bijection.
reverseMapping :: DM.Map Digit Pattern -> DM.Map Pattern Digit
reverseMapping = foldr (uncurry (flip DM.insert)) DM.empty . DM.toList

findDigits :: Input -> [Int]
findDigits input =
    let x = reverseMapping $ findDigitMapping $ DS.fromList $ patterns input
        in toInt . (x DM.!) <$> chosenDigits input

    where
        toInt :: Digit -> Int
        toInt Zero = 0
        toInt One = 1
        toInt Two = 2
        toInt Three = 3
        toInt Four = 4
        toInt Five = 5
        toInt Six = 6
        toInt Seven = 7
        toInt Eight = 8
        toInt Nine = 9

toNumber :: [Int] -> Int
toNumber = foldl' (\s a -> s * 10 + a) 0

solve :: IO ()
solve = do
    v <- getData "DayEight.txt"
    print $ sum . fmap (toNumber . findDigits) <$> traverse formatLine v