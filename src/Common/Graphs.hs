module Common.Graph (

) where

import qualified Data.Map as Map
import qualified Data.Set as Set

-- common graph model + utilities
data DirectedGraph a = DirectedGraph
    { graphNodes :: Set.Set a
    , graphPredecessors :: Map.Map a (Set.Set a)
    , graphSuccessors :: Map.Map a (Set.Set a)
    }

emptyGraph :: DirectedGraph a
emptyGraph = DirectedGraph Set.empty Map.empty Map.empty

addNode :: Eq a => a -> DirectedGraph a -> DirectedGraph a
addNode node graph = 
    DirectedGraph
        (Set.add node (graphNodes graph))
        (graphPredecessors graph)
        (graphSuccessors graph)

addEdge :: Eq a => a -> a -> DirectedGraph a -> DirectedGraph a
addEdge source dest graph = 
    let originalPredecessors = 
            case Map.lookup dest (graphPredecessors graph) of
                Just p -> p
                Nothing -> Set.empty
        originalSuccessors = 
            case Map.lookup source (graphSuccessors graph) of
                Just s -> s
                Nothing -> Set.empty
    in DirectedGraph
            (graphNodes graph)
            Map.insert dest (Set.add source originalPredecessors) (graphPredecessors graph)
            Map.insert source (Set.add dest originalSuccessors) (graphSuccessors graph)

type SCC a = Set.Set a

-- Tarjan's Algorithm: converts graph to DAG of SCCs 
-- returns (i) map from index to SCC and (ii) directed graph between SCC indices

data TarjanNode a = TarjanNode
    { tarjanNodeValue :: a
    , tarjanNodeIndex :: Int
    , tarjanNodeBase :: Int
    , tarjanNodeProcessing :: Bool
    }
data TarjanState a = TarjanState
    { tarjanStateNodeIndexCtr :: Int                    -- curr node index ctr
    , tarjanStateSCCIndexCtr :: Int                     -- curr SCC index ctr
    , tarjanStateNodes :: Map.Map a (TarjanNode a)      -- map from node to TarjanNode
    , tarjanStateStack :: [a]                           -- curr stack of nodes

    -- intermediary SCC DAG state
    , tarjanStateCurrDAG :: DirectedGraph Int           -- SCC index DAG
    , tarjanStateSCCs :: Map.Map Int (SCC a)            -- map from index to SCC
    , tarjanStateMapToSCC :: Map.Map a Int              -- map from node value to SCC index
    }

tarjansAlgo :: Eq a => a -> DirectedGraph a -> (Map.Map Int (SCC a), DirectedGraph Int)
tarjansAlgo initNode graph = 
    let initState = TarjanState 0 0 Map.empty [] emptyGraph Map.empty Map.empty
        finalState = tarjanHelper initNode graph initState
    in (tarjanStateMapToSCC finalState, tarjanStateCurrDAG finalState)

tarjanHelper :: (Eq a, Show a) => a -> DirectedGraph a -> TarjanState a -> TarjanState a
tarjanHelper node graph state = 
    let initTarjanNode = 
            TarjanNode
                node
                (tarjanStateNodeIndexCtr state)
                (tarjanStateNodeIndexCtr state)
                True
        initState = 
            (tarjanUpdateCtr) .
            (tarjanUpdateNode initTarjanNode) state
        successors = 
            case Map.lookup node (graphSuccessors state) of
                Just succ -> succ
                Nothing -> Set.empty
        recursedState =
            foldr
                (\succ (interTarjanNode, interState) ->
                    case Map.lookup succ (tarjanStateNodes interState) of
                        Just tarjanSucc ->
                            let newTarjanNodeBase = 
                                itarjanAddSCCf (tarjanNodeProcessing tarjanSucc)
                                    then (min (tarjanNodeBase interTarjanNode) (tarjanNodeIndex tarjanSucc))
                                    else (tarjanNodeBase interTarjanNode)
                                newTarjanNode = TarjanNode node (tarjanNodeIndex interTarjanNode) newTarjanNodeBase
                                newTarjanState = tarjanUpdateNode newTarjanNode interState
                            in (newTarjanNode, newTarjanState)
                        Nothing ->
                            let processedTarjanState = tarjanHelper (tarjanNodeValue tarjanSucc) graph interState
                                newTarjanSucc = 
                                    case Map.lookup succ (tarjanStateNodes processedTarjanState) of
                                        Just s -> s
                                        Nothing -> compilerError (error "Attempted to access successor Tarjan Node after processing that does not exist: " ++ (show succ))
                                newTarjanNodeBase = (min (tarjanNodeBase interTarjanNode) (tarjanNodeBase newTarjanSucc))
                                newTarjanNode = TarjanNode node (tarjanNodeIndex interTarjanNode) newTarjanNodeBase
                                newTarjanState = tarjanUpdateNode newTarjanNode interState
                            in (newTarjanNode, newTarjanState)
                )
                (interTarjanNode, initState)
                successors
        newTarjanNode = 
            case Map.lookup node (tarjanStateNodes recursedState) of
                Just s -> s
                Nothing -> compilerError (error "Attempted to access current Tarjan Node after processing that does not exist: " ++ (show node))
    in if (tarjanNodeIndex initTarjanNode) == (tarjanNodeIndex newTarjanNode)
        then 
            let (poppedState, scc) = tarjanPopStackToSCC baseNode state Set.empty
            in tarjanAddSCC scc graph poppedState
        else recursedState

-- Tarjan State Node helpers

tarjanAddNode :: Eq a => TarjanNode a -> TarjanState a -> TarjanState a
tarjanAddNode tarjanNode state =
    TarjanState
        ((tarjanStateNodeIndexCtr state) + 1)
        (tarjanStateSCCIndexCtr state)
        (Map.insert (tarjanNodeValue tarjanNode) tarjanNode (tarjanStateNodes state))
        (tarjanStateStack state)
        (tarjanStateCurrDAG state)
        (tarjanStateSCCs state)
        (tarjanStateMapToSCC state)

tarjanUpdateNode :: Eq a => TarjanNode a -> TarjanState a -> TarjanState a
tarjanUpdateNode tarjanNode state = 
    TarjanState
        (tarjanStateNodeIndexCtr state)
        (tarjanStateSCCIndexCtr state)
        (Map.insert (tarjanNodeValue tarjanNode) tarjanNode (tarjanStateNodes state))
        (tarjanStateStack state)
        (tarjanStateCurrDAG state)
        (tarjanStateSCCs state)
        (tarjanStateMapToSCC state)

-- Tarjan State SCC helpers

tarjanAddSCC :: Eq a => SCC a -> DirectedGraph a -> TarjanState a -> TarjanState a
tarjanAddSCC scc graph state = 
    let newSCCIndex = (tarjanStateSCCIndexCtr state) + 1
        newDAG = tarjanInsertSCCToDAG (tarjanStateSCCIndexCtr state) scc graph (tarjanStateMapToSCC state) (tarjanStateCurrDAG state)
        newSCCs = Map.insert (tarjanStateSCCIndexCtr state) scc (tarjanStateSCCs state)
        newMapToSCC = 
            foldr
                (\node interMap ->
                    Map.insert node (tarjanStateSCCIndexCtr state) interMap
                )
                (tarjanStateMapToSCC state)
                scc
    in TarjanState
            (tarjanStateNodeIndexCtr state)
            newSCCIndex
            (tarjanStateNodes state)
            (tarjanStateStack state)
            newDAG
            newSCCs
            newMapToSCC

tarjanInsertSCCToDAG :: Eq a => Int -> SCC a -> DirectedGraph a -> Map.Map a Int -> DirectedGraph Int -> DirectedGraph Int
tarjanInsertSCCToDAG sccIndex scc graph sccMap dag =
    foldr
        (\node interDag ->
            let successors = 
                case Map.lookup node (graphSuccessors graph) of
                    Just s -> s
                    Nothing -> Set.empty
            foldr
                (\succ innerInterDag ->
                    case Map.lookup succ sccMap of
                        Just succSCC -> addEdge sccIndex succSCC
                        Nothing -> compilerError (error "Tarjan's algo added a node to an SCC before its successor was added to an SCC: " ++ 
                                                        "node=" ++ (show node) ++ " succ=" ++ (succ))
                )
                interDag
                successors
        )
        (addNode sccIndex dag)
        scc

-- Tarjan State stack helpers

tarjanPopStackToSCC :: (Show, Eq) a => a -> (TarjanState a, SCC a) -> (TarjanState a, SCC a)
tarjanPopStackToSCC baseNode state currSCC = 
    case tarjanPopStack of
        (Nothing, _) -> compilerError (error "Attempted to clear Tarjan stack but did not find base node: " ++ (show node))
        (Just baseNode, newState) -> (newState, Set.add baseNode currSCC)
        (Just x, newState) -> tarjanPopStackToSCC baseNode newState (Set.add x currSCC)

tarjanPushStack :: a -> TarjanState a -> TarjanState a
tarjanPushStack node state =
    TarjanState
        (tarjanStateNodeIndexCtr state)
        (tarjanStateSCCIndexCtr state)
        (tarjanStateNodes state)        
        (a:(tarjanStateStack state))
        (tarjanStateCurrDAG state)
        (tarjanStateSCCs state)
        (tarjanStateMapToSCC state)

tarjanPopStack :: TarjanState a -> (Just a, TarjanState a)
tarjanPushStack node state =
    let (elem, newStack) = 
        case (tarjanStateStack state) of
            [] -> (Nothing, [])
            x : _ -> (Just x, (tail tarjanStateStack state))
        in (elem, TarjanState
                (tarjanStateNodeIndexCtr state)
                (tarjanStateSCCIndexCtr state)
                (tarjanStateNodes state)        
                newStack
                (tarjanStateCurrDAG state)
                (tarjanStateSCCs state)
                (tarjanStateMapToSCC state))

-- TODO: add predecessor ordering

-- converts the SCC DAG to a list of sets of nodes
-- where each set is a strict ancestor of the previous set
-- i.e. for nodes A and B
-- i. if A ancestor of B -> B is in an earlier set than A's set
-- ii. if B ancestor of A -> A is in an earlier set than B's set
-- iii. o/w A and B are in the same set
strictBfsPredecessorOrdering :: Map.Map Int (SCC a) -> DirectedGraph Int -> [Set.Set a]
