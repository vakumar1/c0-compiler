module Backend.RegAlloc (
    regAllocColoring,
)

where

import Model.Ir
import Common.Liveness

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

regAllocColoring :: FunctionIr -> Map.Map VariableIr Int
regAllocColoring fnIr =
    let ifg = constructIFG fnIr
        order = simplicialElimOrder ifg
     in greedyGraphColoring ifg order

-- IFG CONSTRUCTION

constructIFG :: FunctionIr -> IFG
constructIFG fnIr = 
    let (_, finalIFG) =
            foldl
                ( \(interLiveVars, interIFG) index ->
                    case Map.lookup index (functionIrBlocks fnIr) of
                        Just bb -> constructIFGBasicBlock (interLiveVars, interIFG) bb
                        Nothing -> (interLiveVars, interIFG)
                )
                (Set.empty, (IFG Set.empty Map.empty))
                (bfsPredecessors fnIr)
     in finalIFG

constructIFGBasicBlock :: (Set.Set VariableIr, IFG) -> BasicBlockIr -> (Set.Set VariableIr, IFG)
constructIFGBasicBlock (liveVars, initIFG) bb =
    let (liveVarsUpdatedComms, ifgUpdatedComms) = foldl constructIFGCommand (liveVars, initIFG) (bbIrCommands bb)
        (liveVarsUpdatedPhi, ifgUpdatedPhi) = constructIFGPhi (liveVarsUpdatedComms, ifgUpdatedComms) (bbIrPhiFn bb)
    in (liveVarsUpdatedPhi, ifgUpdatedPhi)

constructIFGPhi :: (Set.Set VariableIr, IFG) -> PhiFnIr -> (Set.Set VariableIr, IFG)
constructIFGPhi (liveVars, initIFG) phi = 
    let newLiveVars = updateLiveVarsPhi liveVars phi
        ifgUpdatedWithAsn = 
            foldr
                (\(var, varPredMap) interIFG ->
                    foldr
                        (\liveVar predInterIFG ->
                            if (var == liveVar)
                                then predInterIFG
                                else addEdgeToIFG (liveVar, var) predInterIFG
                        )
                        (addNodeToIFG var interIFG)
                        liveVars
                )
                initIFG
                (Map.toList phi)
    in (newLiveVars, ifgUpdatedWithAsn)

constructIFGCommand :: (Set.Set VariableIr, IFG) -> CommandIr -> (Set.Set VariableIr, IFG)
constructIFGCommand (liveVars, initIFG) comm =
    let newLiveVars = updateLiveVarsComm liveVars comm
        ifgUpdatedWithAsn =
            case (getAssignedVarsCommand comm) of
                Just asnVar ->
                    foldr
                        ( \liveVar interIFG ->
                            if (liveVar == asnVar)
                                then interIFG
                                else addEdgeToIFG (liveVar, asnVar) interIFG
                        )
                        (addNodeToIFG asnVar initIFG)
                        liveVars
                Nothing -> initIFG
    in (newLiveVars, ifgUpdatedWithAsn)

-- SIMPLICIAL ELIM ORDERING

simplicialElimOrder :: IFG -> [VariableIr]
simplicialElimOrder ifg =
    let initWeights =
            foldr
                (\node interWeights -> Map.insert node 0 interWeights)
                Map.empty
                (ifgNodes ifg)
     in reverse (simplicialElimOrderHelper ifg initWeights)

simplicialElimOrderHelper :: IFG -> Map.Map VariableIr Int -> [VariableIr]
simplicialElimOrderHelper ifg weights =
    if (Map.null weights)
        then []
        else
            let (maxVar, _) =
                    foldr
                        ( \(node, weight) (currNode, currWeight) ->
                            if (weight >= currWeight)
                                then (node, weight)
                                else (currNode, currWeight)
                        )
                        (head (Map.toList weights))
                        (tail (Map.toList weights))
                neighbors =
                    case Map.lookup maxVar (ifgEdges ifg) of
                        Just s -> s
                        Nothing -> Set.empty
                incrWeights =
                    foldr
                        ( \neighbor interWeights ->
                            case Map.lookup neighbor interWeights of
                                Just w -> Map.insert neighbor (w + 1) interWeights
                                Nothing -> interWeights
                        )
                        weights
                        neighbors
                removeWeights = Map.delete maxVar incrWeights
             in maxVar : (simplicialElimOrderHelper ifg removeWeights)

-- GREEDY GRAPH COLORING

greedyGraphColoring :: IFG -> [VariableIr] -> Map.Map VariableIr Int
greedyGraphColoring ifg order = greedyGraphColoringHelper ifg order Map.empty

greedyGraphColoringHelper :: IFG -> [VariableIr] -> Map.Map VariableIr Int -> Map.Map VariableIr Int
greedyGraphColoringHelper ifg order initColoring =
    case order of
        [] -> initColoring
        var : _ ->
            let neighbors =
                    case Map.lookup var (ifgEdges ifg) of
                        Just s -> s
                        Nothing -> Set.empty
                remainingColors =
                    foldr
                        ( \neighbor unusedColors ->
                            case Map.lookup neighbor initColoring of
                                Just c -> (List.delete c unusedColors)
                                Nothing -> unusedColors
                        )
                        [0 ..]
                        neighbors
                newColoring = Map.insert var (head remainingColors) initColoring
             in greedyGraphColoringHelper ifg (tail order) newColoring

-- IFG HELPERS

data IFG = IFG
    { ifgNodes :: Set.Set VariableIr
    , ifgEdges :: Map.Map VariableIr (Set.Set VariableIr)
    }
    deriving (Show)

addNodeToIFG :: VariableIr -> IFG -> IFG
addNodeToIFG var ifg =
    let newNodes = Set.insert var (ifgNodes ifg)
     in IFG newNodes (ifgEdges ifg)

addEdgeToIFG :: (VariableIr, VariableIr) -> IFG -> IFG
addEdgeToIFG (var1, var2) ifg =
    let neighbors1 =
            case Map.lookup var1 (ifgEdges ifg) of
                Just s -> s
                Nothing -> Set.empty
        newNeighbors1 = Set.insert var2 neighbors1
        neighbors2 =
            case Map.lookup var2 (ifgEdges ifg) of
                Just s -> s
                Nothing -> Set.empty
        newNeighbors2 = Set.insert var1 neighbors2
        newEdges = Map.insert var2 newNeighbors2 (Map.insert var1 newNeighbors1 (ifgEdges ifg))
     in IFG (ifgNodes ifg) newEdges
