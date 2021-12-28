module D6P2(solve) where

import Scaffolding
import qualified Data.Map as DM
import Data.Foldable(foldl')
import Data.List.Split(splitOn)

newtype DaysUntilRepro =
    DaysUntilRepro Int deriving(Eq, Ord, Show)

newtype NumbersOfLanternfish =
    NumbersOfLanternfish Integer deriving(Eq, Show)

type ShoalMap = DM.Map DaysUntilRepro NumbersOfLanternfish

newtype Shoal =
    Shoal ShoalMap deriving(Eq, Show)

size :: Shoal -> Integer
size (Shoal s) = foldl' (\s (NumbersOfLanternfish a) -> s + a) 0 s

tick :: Shoal -> Shoal
tick (Shoal old) =
    Shoal
    $ DM.unionWith
        (\(NumbersOfLanternfish a) (NumbersOfLanternfish b) ->
            NumbersOfLanternfish $ a + b)
        (handleNonZero old)
        (handleZero old)
    where
        handleNonZero :: ShoalMap -> ShoalMap
        handleNonZero old =
            foldl'
                (\s a ->
                    DM.insert
                        (DaysUntilRepro $ a - 1)
                        (DM.findWithDefault (NumbersOfLanternfish 0) (DaysUntilRepro a) old)
                        s)
                DM.empty
                [8,7,6,5,4,3,2,1]

        handleZero :: ShoalMap -> ShoalMap
        handleZero sm =
            let reproducingFish =
                    DM.findWithDefault (NumbersOfLanternfish 0) (DaysUntilRepro 0) sm
            in DM.insert (DaysUntilRepro 6) reproducingFish
                $ DM.insert (DaysUntilRepro 8) reproducingFish DM.empty

iter :: Int -> Shoal -> Shoal
iter 0 x = x
iter n s = iter (n-1) $ tick s

makeShoal :: [Int] -> Shoal
makeShoal =
    Shoal
    . fmap NumbersOfLanternfish
    . foldl' (\s a -> DM.insertWith (+) (DaysUntilRepro a) 1 s) DM.empty

formatInput :: [String] -> Shoal
formatInput = makeShoal . (>>= fmap read . splitOn ",")

solve :: IO ()
solve =  do
    v <- getData "DaySix.txt"
    print $ size $ iter 256 $ formatInput v