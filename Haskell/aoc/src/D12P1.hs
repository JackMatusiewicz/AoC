module D12P1(solve) where

import qualified Data.Set as DS
import qualified Data.Map as DM
import qualified Data.Char as DC
import qualified Data.Foldable as DF
import Data.Maybe(fromMaybe, mapMaybe)
import Scaffolding
import qualified Data.Char
import Data.Bifunctor(bimap)
import Control.Monad (liftM2)

newtype Name = Name String deriving(Eq, Ord, Show)

data Node =
    Node Name
    | SmallNode Name
    | Start
    | End
    deriving(Eq, Ord, Show)

{-
Can visit Nodes as many times as you want.
SmallNode, Start, End can only be visited once.

However, when we're finding paths we need to avoid getting into an infinite loop.

Naive heuristic: visit big nodes n+1 times, where n is the number of edges available to them (to allow for the scenario where you go out to all edges, come back and
then finally leave.)

-}

data Graph =
    Graph {
        nodes :: DS.Set Node,
        edges :: DM.Map Node (DS.Set Node)
    }
    deriving(Eq, Ord, Show)

emptyGraph :: Graph
emptyGraph = Graph { nodes = DS.empty, edges = DM.empty }

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

-- Takes an input line and parses it into
-- the two strings representing the nodes.
splitLine :: String -> (String, String)
splitLine =
    toTuple
    . filter (not . null)
    . fmap (filter Data.Char.isAlphaNum)
    . words
    where
        toTuple :: [String] -> (String, String)
        toTuple [x,y] = (x,y)
        toTuple x = error $ show x

-- Generates the graph from the input lines.
generateInput :: [String] -> Graph
generateInput =
    DF.foldl' (\g (a,b) -> add a b g) emptyGraph
    . mapMaybe (biSequence . bimap parseNode parseNode . splitLine)
    where
        biSequence :: Monad f => (f a, f b) -> f (a,b)
        biSequence = uncurry $ liftM2 (,)

connectedTo :: Node -> Graph -> DS.Set Node
connectedTo n Graph { edges = es } = es DM.! n

findPaths :: Graph -> [[Node]]
findPaths g = go [Start] (DM.singleton Start 1) g
    where
        -- There's an infinite loop here. The cache must persist through all calls.
        go :: [Node] -> DM.Map Node Int -> Graph -> [[Node]]
        go [] _ _ = []
        go (End:t) _ _ = [End : t]
        go (h:t) seen g =
            let toVisit = DS.filter (canVisit g seen) $ connectedTo h g
            in
                if DS.size toVisit == 0
                then []
                else
                    DF.foldl' (++) []
                    $ (\n -> go (n:h:t) (updateCache seen h) g) <$> DS.toList toVisit

        updateCache :: DM.Map Node Int -> Node -> DM.Map Node Int
        updateCache cache n = DM.insertWith (+) n 1 cache

        canVisit :: Graph -> DM.Map Node Int -> Node -> Bool
        canVisit Graph { edges = es } seen n =
            maybe
                True
                (\seenCount ->
                    case n of
                        (SmallNode _) -> 1 > seenCount
                        Start -> 1 > seenCount
                        End -> 1 > seenCount
                        n -> DS.size (es DM.! n) > seenCount
                )
            $ DM.lookup n seen

solve :: IO ()
solve = do
    v <- getData "DayTwelve.txt"
    print $ length $ findPaths $ generateInput v
