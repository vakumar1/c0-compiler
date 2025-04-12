module Common.GraphsTest (
    graphs_test,
) where

import Test.Hspec
import qualified Data.Map as Map
import qualified Data.Set as Set

import Common.Graphs

createGraph :: Ord a => [a] -> [(a, a)] -> DirectedGraph a
createGraph nodes edges = 
    (\graph ->
        foldr
            (\(src, dest) interGraph -> addEdge src dest interGraph)
            graph
            edges
    )
    .
    (\graph ->
        foldr
            (\node interGraph -> addNode node interGraph)
            graph
            nodes
    )
    $
    emptyGraph

getAllSCCs :: TarjanResult a -> [Set.Set a]
getAllSCCs result = Map.elems (tarjanResultMapToSCC result)

verifyComponents :: Ord a => [Set.Set a] -> [Set.Set a] -> Bool
verifyComponents xs ys = 
    Set.fromList xs == Set.fromList ys

nodeToSCCIndex :: Ord a => a -> TarjanResult a -> Maybe Int
nodeToSCCIndex node tarjanResult = 
    foldr
        (\(sccIndex, scc) acc -> 
            if Set.member node scc
                then Just sccIndex
                else acc
        )
        Nothing
        (Map.toList . tarjanResultMapToSCC $ tarjanResult)

componentToSCCIndexMap :: Ord a => [Set.Set a] -> TarjanResult a -> Map.Map Int (Maybe Int)
componentToSCCIndexMap components tarjanResult = 
    foldr
        (\(index, component) interMap ->
            Map.insert
                index
                (nodeToSCCIndex (head . Set.toList $ component) tarjanResult)
                interMap
        )
        Map.empty
        (zip [0..] components)

verifyEdges :: Ord a => [Set.Set a] -> [(Int, Int)] -> TarjanResult a -> Bool
verifyEdges expectedComponents expectedEdges tarjanResult =
    let remappedComponents = componentToSCCIndexMap expectedComponents tarjanResult
        remappedExpectedEdges = 
            map
                (\(src, dest) -> 
                    case (Map.lookup src remappedComponents, Map.lookup dest remappedComponents) of
                        (Just (Just remappedSrc), Just (Just remappedDest)) ->
                            (remappedSrc, remappedDest)
                        _ ->
                            (-1, -1)
                )
                expectedEdges
        actualEdges = 
            concatMap
                (\(src, succs) -> map (\dest -> (src, dest)) (Set.toList succs))
                (Map.toList . graphSuccessors . tarjanResultGraph $ tarjanResult)
    in Set.fromList remappedExpectedEdges == Set.fromList actualEdges

graphs_test :: IO ()
graphs_test = hspec $ do
    describe "DirectedGraph" $ do
        describe "Basic Operations" $ do
            it "creates an empty graph" $ do
                let graph = emptyGraph :: DirectedGraph Int
                Set.null (graphNodes graph) `shouldBe` True
                Map.null (graphPredecessors graph) `shouldBe` True
                Map.null (graphSuccessors graph) `shouldBe` True
                
            it "adds a node to the graph" $ do
                let graph = addNode 1 emptyGraph
                graphNodes graph `shouldBe` Set.singleton 1
                
            it "adds an edge to the graph" $ do
                let graph = addEdge 1 2 (addNode 2 (addNode 1 emptyGraph))
                Map.lookup 1 (graphSuccessors graph) `shouldBe` Just (Set.singleton 2)
                Map.lookup 2 (graphPredecessors graph) `shouldBe` Just (Set.singleton 1)

    describe "Tarjan's Algorithm" $ do
        describe "Basic Cases" $ do

            it "handles a single node" $ do
                let graph = createGraph [1] []
                    result = tarjansAlgo 1 graph
                    components = [Set.singleton 1]
                verifyComponents components (getAllSCCs result) `shouldBe` True
                verifyEdges components [] result `shouldBe` True
                
            it "handles a self-loop" $ do
                let graph = createGraph [1] [(1, 1)]
                    result = tarjansAlgo 1 graph
                    components = [Set.singleton 1]
                verifyComponents components (getAllSCCs result) `shouldBe` True
                verifyEdges components [] result `shouldBe` True
        
        describe "Simple Structures" $ do
            it "handles a simple two-node graph without cycle" $ do
                let graph = createGraph [1, 2] [(1, 2)]
                    result = tarjansAlgo 1 graph
                    components = [Set.singleton 1, Set.singleton 2]
                verifyComponents components (getAllSCCs result) `shouldBe` True
                verifyEdges components [(0, 1)] result `shouldBe` True
                
            it "handles a simple two-node cycle" $ do
                let graph = createGraph [1, 2] [(1, 2), (2, 1)]
                    result = tarjansAlgo 1 graph
                    components = [Set.fromList [1, 2]]
                verifyComponents components (getAllSCCs result) `shouldBe` True
                verifyEdges components [] result `shouldBe` True

        describe "Complex Structures with DAG verification" $ do
            it "handles complex graph with multiple SCCs and verifies DAG structure" $ do
                let graph = createGraph [1, 2, 3, 4, 5, 6] 
                                       [(1, 2), (2, 3), (3, 1),  -- SCC 0
                                        (3, 4),                  -- 0 -> 1
                                        (4, 5), (5, 6), (6, 4)]  -- SCC 1
                    result = tarjansAlgo 1 graph
                    components = [Set.fromList [1, 2, 3], Set.fromList [4, 5, 6]]
                verifyComponents components (getAllSCCs result) `shouldBe` True
                verifyEdges components [(0, 1)] result `shouldBe` True
                
            it "handles a chain of SCCs and verifies DAG structure" $ do
                let graph = createGraph [1, 2, 3, 4, 5, 6] 
                                       [(1, 2), (2, 1),     -- SCC 0
                                        (2, 3),             -- 0 -> 1
                                        (3, 4), (4, 3),     -- SCC 1
                                        (4, 5),             -- 1 -> 2
                                        (5, 6), (6, 5)]     -- 2
                    result = tarjansAlgo 1 graph
                    components = [Set.fromList [1, 2], Set.fromList [3, 4], Set.fromList [5, 6]]
                verifyComponents components (getAllSCCs result) `shouldBe` True
                verifyEdges components [(0, 1), (1, 2)] result `shouldBe` True
                    
            it "handles complex DAG of SCCs" $ do
                let graph = createGraph [1, 2, 3, 4, 5, 6, 7, 8, 9] 
                                       [(1, 2), (2, 1),           -- SCC 0
                                        (1, 3),                   -- 0 -> 1
                                        (3, 4), (4, 3),           -- SCC 1
                                        (3, 5), (4, 5),           -- 1 -> 2
                                        (5, 6), (6, 5),           -- SCC 2
                                        (5, 7), (5, 8),           -- 2 -> 3, 2 -> 4
                                        (7, 9), (9, 7),           -- SCC 3
                                        (8, 9)]                   -- 4 -> 3
                    result = tarjansAlgo 1 graph
                    components = [Set.fromList [1, 2], 
                               Set.fromList [3, 4], 
                               Set.fromList [5, 6],
                               Set.fromList [7, 9],
                               Set.singleton 8]
                verifyComponents components (getAllSCCs result) `shouldBe` True
                verifyEdges components [(0, 1), (1, 2), (2, 3), (2, 4), (4, 3)] result `shouldBe` True
                
        describe "Special Cases" $ do
            it "handles the classic example from Tarjan's paper and verifies DAG structure" $ do
                let graph = createGraph ['A'..'H'] 
                                        [('A', 'B'), ('B', 'C'), ('B', 'E'), ('B', 'F'),
                                         ('C', 'D'), ('C', 'G'), ('D', 'C'), ('D', 'H'),
                                         ('E', 'A'), ('E', 'F'), ('F', 'G'), ('G', 'F'),
                                         ('H', 'G'), ('H', 'D')]
                    result = tarjansAlgo 'A' graph
                    components = [Set.fromList ['A', 'B', 'E'], 
                                Set.fromList ['C', 'D', 'H'],
                                Set.fromList ['F', 'G']]
                verifyComponents components (getAllSCCs result) `shouldBe` True
                verifyEdges components [(0, 1), (0, 2), (1, 2)] result `shouldBe` True
            
            it "handles a graph where the start node isn't part of a cycle" $ do
                let graph = createGraph [1, 2, 3, 4] [(1, 2), (2, 3), (3, 4), (4, 2)]
                    result = tarjansAlgo 1 graph
                    components = [Set.singleton 1, Set.fromList [2, 3, 4]]
                verifyComponents components (getAllSCCs result) `shouldBe` True
                verifyEdges components [(0, 1)] result `shouldBe` True

    describe "innerSCCSubOrdering" $ do
            
        it "ensures every element except one appears twice" $ do
            let sccMap = 
                    (Map.insert 0 (Set.fromList [1, 2, 3, 4])) .
                    (Map.insert 1 (Set.fromList [5, 6, 7])) $
                    Map.empty
                ordering0 = innerSCCSubOrdering 0 sccMap
                ordering1 = innerSCCSubOrdering 1 sccMap
                counts0 = foldr (\x acc -> Map.insertWith (+) x 1 acc) Map.empty ordering0
                counts1 = foldr (\x acc -> Map.insertWith (+) x 1 acc) Map.empty ordering1
                
            Map.size counts0 `shouldBe` 4
            Map.size counts1 `shouldBe` 3
            length (filter (==1) (Map.elems counts0)) `shouldBe` 1
            length (filter (==2) (Map.elems counts0)) `shouldBe` 3
            length (filter (==1) (Map.elems counts1)) `shouldBe` 1
            length (filter (==2) (Map.elems counts1)) `shouldBe` 2
