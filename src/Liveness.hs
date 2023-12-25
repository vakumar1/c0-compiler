module Liveness (
    regAllocColoring,
)
where

import Ir

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

regAllocColoring :: FunctionIr -> Map.Map String Int
regAllocColoring fnIr =
    let ifg = constructIFG fnIr
        order = simplicialElimOrder ifg
     in greedyGraphColoring ifg order

-- IFG CONSTRUCTION

constructIFG :: FunctionIr -> IFG
constructIFG fnIr = constructIFGHelper fnIr (bfsPredecessors fnIr) Set.empty (IFG Set.empty Map.empty)

constructIFGHelper :: FunctionIr -> [Int] -> Set.Set String -> IFG -> IFG
constructIFGHelper fnIr blocks liveVars initIFG =
    let (_, finalIFG) =
            foldl
                ( \(interLiveVars, interIFG) index ->
                    case Map.lookup index (functionIrBlocks fnIr) of
                        Just bb -> constructIFGBasicBlock (interLiveVars, interIFG) bb
                        Nothing -> (liveVars, initIFG)
                )
                (liveVars, initIFG)
                blocks
     in finalIFG

constructIFGBasicBlock :: (Set.Set String, IFG) -> BasicBlockIr -> (Set.Set String, IFG)
constructIFGBasicBlock (liveVars, initIFG) bb =
    foldl constructIFGCommand (liveVars, initIFG) (bbIrCommands bb)

constructIFGCommand :: (Set.Set String, IFG) -> CommandIr -> (Set.Set String, IFG)
constructIFGCommand (liveVars, initIFG) comm =
    case comm of
        INIT_IR var -> (liveVars, initIFG)
        ASN_PURE_IR asnVar asnPure ->
            let liveVarsAddedUsed = Set.union liveVars (getUsedVarsPure asnPure)
                liveVarsRemovedAsn = Set.delete (variableIrName asnVar) liveVarsAddedUsed
                ifgAddedAsnEdges =
                    foldr
                        ( \liveVar interIFG ->
                            if (liveVar == (variableIrName asnVar))
                                then interIFG
                                else addEdgeToIFG (liveVar, (variableIrName asnVar)) interIFG
                        )
                        initIFG
                        liveVars
                ifgAddedAsnNode = addNodeToIFG (variableIrName asnVar) ifgAddedAsnEdges
             in (liveVarsRemovedAsn, ifgAddedAsnNode)
        ASN_IMPURE_IR asnVar asnImpure ->
            let liveVarsAddedUsed = Set.union liveVars (getUsedVarsImpure asnImpure)
                liveVarsRemovedAsn = Set.delete (variableIrName asnVar) liveVarsAddedUsed
                ifgAddedAsnEdges =
                    foldr
                        ( \liveVar interIFG ->
                            if (liveVar == (variableIrName asnVar))
                                then interIFG
                                else addEdgeToIFG (liveVar, (variableIrName asnVar)) interIFG
                        )
                        initIFG
                        liveVars
                ifgAddedAsnNode = addNodeToIFG (variableIrName asnVar) ifgAddedAsnEdges
             in (liveVarsRemovedAsn, ifgAddedAsnNode)
        GOTO_BB_IR _ -> (liveVars, initIFG)
        RET_PURE_IR retPure ->
            let liveVarsAddedUsed = Set.union liveVars (getUsedVarsPure retPure)
             in (liveVarsAddedUsed, initIFG)

getUsedVarsPure :: PureIr -> Set.Set String
getUsedVarsPure pure =
    case pure of
        PURE_BASE_IR base -> getUsedVarsPureBase base
        PURE_BINOP_IR (PureBinopIr _ _ base1 base2) -> Set.union (getUsedVarsPureBase base1) (getUsedVarsPureBase base2)
        PURE_UNOP_IR (PureUnopIr _ _ base) -> getUsedVarsPureBase base

getUsedVarsImpure :: ImpureIr -> Set.Set String
getUsedVarsImpure impure =
    case impure of
        IMPURE_BINOP_IR (ImpureBinopIr _ _ base1 base2) -> Set.union (getUsedVarsPureBase base1) (getUsedVarsPureBase base2)

getUsedVarsPureBase :: PureBaseIr -> Set.Set String
getUsedVarsPureBase base =
    case base of
        CONST_IR const -> Set.empty
        VAR_IR var -> Set.singleton (variableIrName var)

-- SIMPLICIAL ELIM ORDERING

simplicialElimOrder :: IFG -> [String]
simplicialElimOrder ifg =
    let initWeights =
            foldr
                (\node interWeights -> Map.insert node 0 interWeights)
                Map.empty
                (ifgNodes ifg)
     in reverse (simplicialElimOrderHelper ifg initWeights)

simplicialElimOrderHelper :: IFG -> Map.Map String Int -> [String]
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
                        ("", -1)
                        (Map.toList weights)
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

greedyGraphColoring :: IFG -> [String] -> Map.Map String Int
greedyGraphColoring ifg order = greedyGraphColoringHelper ifg order Map.empty

greedyGraphColoringHelper :: IFG -> [String] -> Map.Map String Int -> Map.Map String Int
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
    { ifgNodes :: Set.Set String
    , ifgEdges :: Map.Map String (Set.Set String)
    }
    deriving (Show)

addNodeToIFG :: String -> IFG -> IFG
addNodeToIFG var ifg =
    let newNodes = Set.insert var (ifgNodes ifg)
     in IFG newNodes (ifgEdges ifg)

addEdgeToIFG :: (String, String) -> IFG -> IFG
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
