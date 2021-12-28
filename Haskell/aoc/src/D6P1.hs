module D6P1(solve) where

import Scaffolding
import Utils
import Data.List.Split(splitOn)
import Data.List(genericLength)

data LanternFish =
    Newborn Int
    | Mature Int
    deriving(Eq, Show)

tick :: LanternFish -> (LanternFish, Maybe LanternFish)
tick (Newborn 0) = (Mature 6, Just $ Newborn 8)
tick (Newborn x) = (Mature $ x - 1, Nothing)
tick (Mature 0) = (Mature 6, Just $ Newborn 8)
tick (Mature x) = (Mature $ x - 1, Nothing)

convert :: (LanternFish, Maybe LanternFish) -> [LanternFish]
convert (x, Just y) = [x,y]
convert y = [fst y]

iter :: Int -> [LanternFish] -> [LanternFish]
iter 0 xs = xs
iter n xs = iter (n-1) $ xs >>= convert . tick

formatInput :: [String] -> [LanternFish]
formatInput = (>>= fmap (Mature . read) . splitOn ",")

solve :: IO ()
solve = do
    v <- getData "DaySix.txt"
    print $ length (iter 256 $ formatInput v)