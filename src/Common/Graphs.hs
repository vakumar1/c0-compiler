module Common.Graphs (
    DirectedGraph (..),
    TarjanResult (..),
    emptyGraph,
    addNode,
    addEdge,
    SCC,
    tarjansAlgo,
    innerSCCSubOrdering,
) where

import Common.Errors
import Common.Constants

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Text.Show.Pretty as Pretty
import qualified Debug.Trace as Trace

-- common graph model + utilities

data DirectedGraph a = DirectedGraph
    { graphNodes :: Set.Set a
    , graphPredecessors :: Map.Map a (Set.Set a)
    , graphSuccessors :: Map.Map a (Set.Set a)
    }
    deriving (Show)

emptyGraph :: DirectedGraph a
emptyGraph = DirectedGraph Set.empty Map.empty Map.empty

addNode :: Ord a => a -> DirectedGraph a -> DirectedGraph a
addNode node graph = 
    DirectedGraph
        (Set.insert node (graphNodes graph))
        (graphPredecessors graph)
        (graphSuccessors graph)

addEdge :: Ord a => a -> a -> DirectedGraph a -> DirectedGraph a
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
            (Map.insert dest (Set.insert source originalPredecessors) (graphPredecessors graph))
            (Map.insert source (Set.insert dest originalSuccessors) (graphSuccessors graph))

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
    , tarjanStateNodes :: Map.Map a (TarjanNode a)      -- map from node to TarjanNode
    , tarjanStateStack :: [a]                           -- curr stack of nodes

    -- intermediary SCC DAG state
    , tarjanStateSCCIndexCtr :: Int                     -- curr SCC index ctr
    , tarjanStateCurrDAG :: DirectedGraph Int           -- SCC index DAG
    , tarjanStateDAGLeaves :: Set.Set Int               -- SCC indexes for DAG leaves
    , tarjanStateSCCs :: Map.Map Int (SCC a)            -- map from index to SCC
    , tarjanStateMapToSCC :: Map.Map a Int              -- map from node value to SCC index
    }
data TarjanResult a = TarjanResult
    { tarjanResultRootSCC :: Int                        -- root SCC index
    , tarjanResultLeaves :: Set.Set Int                 -- leaf SCC indices
    , tarjanResultGraph :: DirectedGraph Int            -- SCC index DAG
    , tarjanResultMapToSCC :: Map.Map Int (SCC a)       -- map from index to SCC
    }

tarjansAlgo :: (Ord a, Show a) => a -> DirectedGraph a -> TarjanResult a
tarjansAlgo initNode graph = 
    let initState = 
            TarjanState
                0
                Map.empty
                []
                0
                emptyGraph
                Set.empty
                Map.empty
                Map.empty
        finalState = tarjanHelper initNode graph initState
        rootSCC = 
            case Map.lookup initNode (tarjanStateMapToSCC finalState) of
                Just r -> r
                Nothing -> error . compilerError $ "Attempted to look up root node SCC after Tarjan's algo but root was never assigned an SCC: root=" ++ (show initNode)
        tarjanResult = 
            TarjanResult
                rootSCC
                (tarjanStateDAGLeaves finalState)
                (tarjanStateCurrDAG finalState)
                (tarjanStateSCCs finalState)
    in tarjanResult

tarjanHelper :: (Ord a, Show a) => a -> DirectedGraph a -> TarjanState a -> TarjanState a
tarjanHelper node graph state = 
    let tarjanIndex = tarjanStateNodeIndexCtr state
        initTarjanNode = 
            TarjanNode
                node
                tarjanIndex
                tarjanIndex
                True
        initState = 
            ((tarjanPushStack node) .
            (tarjanAddNode initTarjanNode))
            state
        successors = 
            case Map.lookup node (graphSuccessors graph) of
                Just succ -> succ
                Nothing -> Set.empty
        (recursedTarjanNode, recursedState) =
            foldr
                (\succ (interTarjanNode, interState) ->
                    case Map.lookup succ (tarjanStateNodes interState) of
                        Just tarjanSucc ->
                            let newTarjanNodeBase = 
                                    if (tarjanNodeProcessing tarjanSucc)
                                        then (min (tarjanNodeBase interTarjanNode) (tarjanNodeIndex tarjanSucc))
                                        else (tarjanNodeBase interTarjanNode)
                                newTarjanNode = 
                                    TarjanNode 
                                        node 
                                        tarjanIndex
                                        newTarjanNodeBase
                                        True
                                newTarjanState = tarjanUpdateNode newTarjanNode interState
                            in (newTarjanNode, newTarjanState)
                        Nothing ->
                            let processedTarjanState = tarjanHelper succ graph interState
                                newTarjanSucc = 
                                    case Map.lookup succ (tarjanStateNodes processedTarjanState) of
                                        Just s -> s
                                        Nothing -> error (compilerError ("Node=" ++ (show node) ++ " Attempted to access successor Tarjan Node after processing that does not exist: " ++ (show succ)))
                                newTarjanNodeBase = (min (tarjanNodeBase interTarjanNode) (tarjanNodeBase newTarjanSucc))
                                newTarjanNode = 
                                    TarjanNode 
                                        node 
                                        tarjanIndex
                                        newTarjanNodeBase
                                        True
                                newTarjanState = tarjanUpdateNode newTarjanNode processedTarjanState
                            in (newTarjanNode, newTarjanState)
                )
                (initTarjanNode, initState)
                successors
    in if (tarjanNodeIndex recursedTarjanNode) == (tarjanNodeBase recursedTarjanNode)
            then 
                let (poppedState, scc) = tarjanPopStackToSCC node (recursedState, Set.empty)
                in tarjanAddSCC scc graph poppedState
            else recursedState

-- Tarjan State Node helpers

tarjanAddNode :: Ord a => TarjanNode a -> TarjanState a -> TarjanState a
tarjanAddNode tarjanNode state =
    TarjanState
        ((tarjanStateNodeIndexCtr state) + 1)
        (Map.insert (tarjanNodeValue tarjanNode) tarjanNode (tarjanStateNodes state))
        (tarjanStateStack state)
        (tarjanStateSCCIndexCtr state)
        (tarjanStateCurrDAG state)
        (tarjanStateDAGLeaves state)
        (tarjanStateSCCs state)
        (tarjanStateMapToSCC state)

tarjanUpdateNode :: Ord a => TarjanNode a -> TarjanState a -> TarjanState a
tarjanUpdateNode tarjanNode state = 
    TarjanState
        (tarjanStateNodeIndexCtr state)
        (Map.insert (tarjanNodeValue tarjanNode) tarjanNode (tarjanStateNodes state))
        (tarjanStateStack state)
        (tarjanStateSCCIndexCtr state)
        (tarjanStateCurrDAG state)
        (tarjanStateDAGLeaves state)
        (tarjanStateSCCs state)
        (tarjanStateMapToSCC state)

-- Tarjan State SCC helpers

tarjanAddSCC :: (Ord a, Show a) => SCC a -> DirectedGraph a -> TarjanState a -> TarjanState a
tarjanAddSCC scc graph state = 
    let newSCCIndex = (tarjanStateSCCIndexCtr state) + 1
        newMapToSCC = 
            foldr
                (\node interMap ->
                    Map.insert node (tarjanStateSCCIndexCtr state) interMap
                )
                (tarjanStateMapToSCC state)
                scc
        (isLeaf, newDAG) = tarjanInsertSCCToDAG (tarjanStateSCCIndexCtr state) scc graph newMapToSCC (tarjanStateCurrDAG state)
        newLeaves = if isLeaf then Set.insert newSCCIndex (tarjanStateDAGLeaves state) else (tarjanStateDAGLeaves state)
        newSCCs = Map.insert (tarjanStateSCCIndexCtr state) scc (tarjanStateSCCs state)
    in TarjanState
            (tarjanStateNodeIndexCtr state)
            (tarjanStateNodes state)
            (tarjanStateStack state)
            newSCCIndex
            newDAG
            newLeaves
            newSCCs
            newMapToSCC

tarjanInsertSCCToDAG :: (Ord a, Show a) => Int -> SCC a -> DirectedGraph a -> Map.Map a Int -> DirectedGraph Int -> (Bool, DirectedGraph Int)
tarjanInsertSCCToDAG sccIndex scc graph nodeMapToSCC dag
    | debugGraphLogs && (Trace.trace 
        ("\n\ntarjanInsertSCCToDAG -- " ++
            "\nsccIndex=" ++ (Pretty.ppShow sccIndex) ++
            "\nscc=" ++ (Pretty.ppShow scc) ++
            "\ncurrDAG=" ++ (Pretty.ppShow dag)
        )
        False) = undefined
tarjanInsertSCCToDAG sccIndex scc graph nodeMapToSCC dag =
    foldr
        (\node (interIsLeaf, interDag) ->
            let successors = 
                    case Map.lookup node (graphSuccessors graph) of
                        Just s -> s
                        Nothing -> Set.empty
            in foldr
                    (\succ (innerInterIsLeaf, innerInterDag) ->
                        case Map.lookup succ nodeMapToSCC of
                            Just succSCC -> 
                                if sccIndex == succSCC
                                    then (innerInterIsLeaf, innerInterDag)
                                    else (False, addEdge sccIndex succSCC innerInterDag)
                            Nothing -> error (compilerError ("Tarjan's algo added a node to an SCC before itself/successor was added to an SCC: " ++ 
                                                            "node=" ++ (show node) ++ " nodeSCC=" ++ (show sccIndex) ++ 
                                                            " succ=" ++ (show succ)))
                    )
                    (interIsLeaf, interDag)
                    successors
        )
        (True, addNode sccIndex dag)
        scc

-- Tarjan State stack helpers

-- pops all nodes on top of stack into an SCC until reaching the base node
tarjanPopStackToSCC :: (Show a, Ord a) => a -> (TarjanState a, SCC a) -> (TarjanState a, SCC a)
tarjanPopStackToSCC baseNode (state, currSCC) = 
    case (tarjanPopStack state) of
        (Just x, newState) ->
            if baseNode == x
                then (newState, Set.insert baseNode currSCC)
                else tarjanPopStackToSCC baseNode (newState, Set.insert x currSCC)
        (Nothing, _) -> error (compilerError "Attempted to clear Tarjan stack but did not find base node: " ++ (show baseNode))

-- pops top node from stack and returns
-- and updates node state to not processing
tarjanPopStack :: (Show a, Ord a) => TarjanState a -> (Maybe a, TarjanState a)
tarjanPopStack state =
    case (tarjanStateStack state) of
        [] -> (Nothing, state)
        node : _ -> 
            let newStack = tail (tarjanStateStack state)
                newTarjanNode = 
                    case Map.lookup node (tarjanStateNodes state) of
                        Just tarjanNode ->
                            TarjanNode
                                (tarjanNodeValue tarjanNode) 
                                (tarjanNodeIndex tarjanNode) 
                                (tarjanNodeBase tarjanNode)
                                False
                        Nothing -> error (compilerError ("Attempted to update Tarjan Node after popping from stack that does not exist: node=" ++ (show node)))
            in (Just node,
                TarjanState
                    (tarjanStateNodeIndexCtr state)
                    (Map.insert node newTarjanNode (tarjanStateNodes state))
                    (tail . tarjanStateStack $ state)
                    (tarjanStateSCCIndexCtr state)
                    (tarjanStateCurrDAG state)
                    (tarjanStateDAGLeaves state)
                    (tarjanStateSCCs state)
                    (tarjanStateMapToSCC state))

tarjanPushStack :: a -> TarjanState a -> TarjanState a
tarjanPushStack node state =
    TarjanState
        (tarjanStateNodeIndexCtr state)
        (tarjanStateNodes state)        
        (node:(tarjanStateStack state))
        (tarjanStateSCCIndexCtr state)
        (tarjanStateCurrDAG state)
        (tarjanStateDAGLeaves state)
        (tarjanStateSCCs state)
        (tarjanStateMapToSCC state)

-- returns a sub-ordering of an SCC
-- where we repeat an arbitrary ordering of all but one of the elements of the SCC
-- e.g. the SCC {A, B, C} could return the ordering [A, B, C, A, B] or [C, A, B, A, C]
-- -> every element appears before and after every other element in the SCC
innerSCCSubOrdering :: (Ord a) => Int -> Map.Map Int (SCC a) -> [a]
innerSCCSubOrdering sccIndex sccMap = 
    let scc = 
            case Map.lookup sccIndex sccMap of
                Just s -> s
                Nothing -> error (compilerError ("Attempted to lookup SCC index for SCC that does not exist while generating subordering: index=" ++ (show sccIndex)))
        initOrdering = Set.toList scc
        doubleOrdering = initOrdering ++ (Set.toList (Set.delete (last initOrdering) scc))
    in doubleOrdering
