module D12P1(solve) where

import qualified Data.Set as DS
import qualified Data.Map as DM
import qualified Data.Char as DC
import qualified Data.Foldable as DF
import Data.Maybe(fromMaybe)
import Scaffolding

newtype Name = Name String deriving(Eq, Ord, Show)

data Node =
    Node Name
    | SmallNode Name
    | Start
    | End
    deriving(Eq, Ord, Show)

data Graph =
    Graph {
        nodes :: DS.Set Node,
        edges :: DM.Map Node (DS.Set Node)
    }
    deriving(Eq, Ord, Show)

-- | Converts a string input into a specific node.
--
-- Ensures that the node string only contains letters.
parseNode :: String -> Maybe Node
parseNode c =
    if and $ fmap DC.isLetter c then Just $ go c
    else Nothing
    where
        go :: String -> Node
        go "start" = Start
        go "end" = End
        go n =
            (if DC.isUpper $ head n then Node else SmallNode) $ Name n

-- | Takes two nodes and adds them (and the edges between them) to
-- the graph.
add :: Node -> Node -> Graph -> Graph
add a b g =
    Graph {
        nodes = DF.foldl' (flip DS.insert) (nodes g) [a,b],
        edges = DF.foldl' (flip $ uncurry update) (edges g) [(a,b), (b,a)]
    }
    where
        update :: (Ord k, Ord a) => k -> a -> DM.Map k (DS.Set a) -> DM.Map k (DS.Set a)
        update k v m =
            let newV = maybe (DS.singleton v) (DS.insert v) (DM.lookup k m)
            in DM.insert k newV m

solve :: IO ()
solve = do
    v <- getData "DayTwelve.txt"
    print "no"
