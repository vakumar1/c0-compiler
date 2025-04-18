module Backend.RegAlloc (
    regAllocColoring,
)

where

import Model.Ir
import Model.Types
import Common.IrUtils
import Common.Liveness
import Common.Graphs
import Common.Errors
import Common.Constants

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Debug.Trace as Trace
import qualified Text.Show.Pretty as Pretty

regAllocColoring :: FunctionIr -> TarjanResult Int -> Coloring
regAllocColoring fnIr tarjanResult =
    let versionedLiveMap = livenessPass fnIr tarjanResult
        ifg = constructIFG fnIr versionedLiveMap
        order = simplicialElimOrder ifg
        stackVars = forceStackPass fnIr
        coloring = greedyGraphColoring ifg stackVars order
     in Coloring coloring stackVars

-- IFG CONSTRUCTION

-- undirected graph - uses a directed graph
-- and adds both directions for a given undirected edge

type IFG = DirectedGraph VariableIr

addNodeToIFG :: VariableIr -> IFG -> IFG
addNodeToIFG var ifg = addNode var ifg

addEdgeToIFG :: (VariableIr, VariableIr) -> IFG -> IFG
addEdgeToIFG (var1, var2) ifg = (addEdge var1 var2 (addEdge var2 var1 ifg))

-- construct interference graph between versioned variables
constructIFG :: FunctionIr -> LiveMap -> IFG
constructIFG fnIr versionedLiveMap
    | debugColoringLogs && (Trace.trace 
        ("\n\nconstructIFG -- " ++
            "\nliveMap=" ++ (Pretty.ppShow versionedLiveMap)
        )
        False) = undefined
constructIFG fnIr versionedLiveMap = 
    foldl
        (\interIFG bbIndex ->
            let successors = 
                    case Map.lookup bbIndex (graphSuccessors (functionIrCFG fnIr)) of
                        Just s -> s
                        Nothing -> Set.empty
                liveOutVars = 
                    foldr
                        (\succIndex interLiveOutVars ->
                            let succLiveInVars = 
                                    case Map.lookup succIndex versionedLiveMap of
                                        Just s -> s
                                        Nothing -> Set.empty
                            in Set.union interLiveOutVars succLiveInVars
                        )
                        Set.empty
                        successors
                bb = getBB fnIr bbIndex
            in constructIFGBasicBlock bb liveOutVars interIFG
        )
        emptyGraph
        [0..((length . functionIrBlocks $ fnIr) - 1)]

constructIFGBasicBlock :: BasicBlockIr -> Set.Set VariableIr -> IFG -> IFG
constructIFGBasicBlock bb liveVars ifg 
    | debugColoringLogs && (Trace.trace 
        ("\n\nconstructIFGBasicBlock -- " ++
            "\nbbIr=" ++ (Pretty.ppShow bb) ++
            "\nliveVars=" ++ (Pretty.ppShow liveVars)
        )
        False) = undefined
constructIFGBasicBlock bb liveVars ifg = 
    let (liveVarsUpdatedComms, ifgUpdatedComms) = foldl constructIFGCommand (liveVars, ifg) (bbIrCommands bb)
        (liveVarsUpdatedPhi, ifgUpdatedPhi) = constructIFGPhi (liveVarsUpdatedComms, ifgUpdatedComms) (bbIrPhiFn bb)
    in ifgUpdatedPhi

constructIFGPhi :: (Set.Set VariableIr, IFG) -> PhiFnIr -> (Set.Set VariableIr, IFG)
constructIFGPhi (liveVars, initIFG) phi = 
    let 
        -- update live vars by removing all assigned vars and inserting all used vars
        newLiveVars = updateLiveVarsPhi liveVars phi

        -- update IFG by adding edges between all assigned vars with
        -- all live vars and all used vars (excluding those from the same phi-fn)
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
                        (Set.difference (Set.union (getUsedVarsPhi phi) liveVars) (getUsedVarsPredMap varPredMap))
                )
                initIFG
                (Map.toList phi)
    in (newLiveVars, ifgUpdatedWithAsn)

constructIFGCommand :: (Set.Set VariableIr, IFG) -> CommandIr -> (Set.Set VariableIr, IFG)
constructIFGCommand (liveVars, initIFG) comm =
    let 
        -- update live vars by removing assigned var (if any) and inserting all used vars
        newLiveVars = updateLiveVarsComm liveVars comm

        -- update IFG by adding edges between assigned var and all live vars
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
                (graphNodes ifg)
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
                    case Map.lookup maxVar (graphSuccessors ifg) of
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

greedyGraphColoring :: IFG -> Set.Set VariableIr -> [VariableIr] -> Map.Map VariableIr Int
greedyGraphColoring ifg stackVars order = 
    let 
        -- alloc stack var colors independently
        (stackColoring, baseColor) = 
            foldr
                (\stackVar (interColoring, nextColor) ->
                    (Map.insert stackVar nextColor interColoring, nextColor + 1)
                )
                (Map.empty, 0)
                (Set.toList stackVars)

        -- alloc all other var colors together
        fullColoring = greedyGraphColoringHelper baseColor ifg stackVars order stackColoring
    in fullColoring

greedyGraphColoringHelper :: Int -> IFG -> Set.Set VariableIr -> [VariableIr] -> Map.Map VariableIr Int -> Map.Map VariableIr Int
greedyGraphColoringHelper baseColor ifg stackVars order initColoring =
    case order of
        [] -> initColoring
        var : _ ->
            if Set.member var stackVars
                -- skip duplicated stack vars
                then 
                    greedyGraphColoringHelper baseColor ifg stackVars (tail order) initColoring
                -- alloc color for all other vars
                else
                    let neighbors =
                            case Map.lookup var (graphSuccessors ifg) of
                                Just s -> s
                                Nothing -> Set.empty
                        remainingColors =
                            foldr
                                ( \neighbor unusedColors ->
                                    case Map.lookup neighbor initColoring of
                                        Just c -> (List.delete c unusedColors)
                                        Nothing -> unusedColors
                                )
                                [baseColor ..]
                                neighbors
                        selectedColor = head remainingColors
                        newColoring = Map.insert var selectedColor initColoring
                    in greedyGraphColoringHelper baseColor ifg stackVars (tail order) newColoring

-- REFERENCED VARS PASS
forceStackPass :: FunctionIr -> Set.Set VariableIr
forceStackPass fnIr =
    foldr
        (\index interVars -> 
            Set.union interVars (forceStackBBIr $ getBB fnIr index)
        )
        Set.empty
        [0..((length . functionIrBlocks $ fnIr) - 1)]

forceStackBBIr :: BasicBlockIr -> Set.Set VariableIr
forceStackBBIr bbIr =
    foldr
        (\commIr interVars ->
            case forceStackCommIr commIr of
                Just var -> Set.insert var interVars
                Nothing -> interVars
        )
        Set.empty
        (bbIrCommands bbIr)

forceStackCommIr :: CommandIr -> Maybe VariableIr
forceStackCommIr commIr = 
    case commIr of
        -- select all declared non-atomic vars
        INIT_IR varIr ->
            case (variableIrType varIr) of
                ARRAY_TYPE _ _ -> Just varIr
                STRUCT_TYPE _ -> Just varIr
                _ -> Nothing
        -- select all referenced vars
        ASN_PURE_IR _ _ (PURE_UNOP_IR (PureUnopIr REF_IR _ puB)) ->
            case puB of
                VAR_IR var -> Just var
                _ -> error . compilerError $ ("Created reference unop on non-variable pure base=" ++ (show puB))
        _ -> Nothing
