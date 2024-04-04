module Common.Liveness (
    LiveMap,
    livenessPass,
    updateLiveVarsPhi,
    getUsedVarsPredMap,
    updateLiveVarsComm,
    getUsedVarsCommand,
    getAssignedVarsCommand,
)
where

import Model.Ir
import Common.Graphs
import Common.Errors

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Debug.Trace as Trace

-- map from bb/SCC index to base (i.e., SSAId = 0) variables that are live-in at start of bb/SCC
type LiveMap = Map.Map Int (Set.Set VariableIr)

-- LIVENESS PASS ON CFG -> returns map of (base) variables that are live-in for each BB

livenessPass :: FunctionIr -> (Int, Set.Set Int, DirectedGraph Int, Map.Map Int (SCC Int)) -> LiveMap
livenessPass fnIr (root, leaves, dag, sccMap) = 
    -- TODO: replace 2 with correctly passed scc dag root
    let sccLiveMap = livenessPassSCCHelper root fnIr dag sccMap Map.empty
        bbLiveMap = livenessPassDeaggregateSCCToBBHelper sccMap sccLiveMap
    in bbLiveMap

-- constructs the SCC liveness map - i.e., a map from the SCC index to
-- the set of variables that are live-in to the SCC
livenessPassSCCHelper :: Int -> FunctionIr -> DirectedGraph Int -> Map.Map Int (SCC Int) -> LiveMap -> LiveMap
-- livenessPassSCCHelper sccIndex fnIr dag sccMap sccLiveMap
--     | Trace.trace 
--         ("\n\nlivenessPassSCCHelper -- " ++
--             "\nsccIndex=" ++ (show sccIndex) ++
--             "\ncurrSCCLiveMap=" ++ (show sccLiveMap) ++
--             "\ndag=" ++ (show dag)
--         )
--         False = undefined
livenessPassSCCHelper sccIndex fnIr dag sccMap sccLiveMap = 
    let scc = 
            case Map.lookup sccIndex sccMap of
                Just s -> s
                Nothing -> error . compilerError $ "Attempted to access scc during liveMap construction that does not exist: sccIndex=" ++ (show sccIndex)
        succSCCIndices = 
            case Map.lookup sccIndex (graphSuccessors dag) of
                Just s -> s
                Nothing -> Set.empty
        (liveOutVars, recursedSCCLiveMap) = 
            foldr
                (\succSCCIndex (interLiveOutVars, interSCCLiveMap) ->
                    let newSCCLiveMap = 
                            case Map.lookup succSCCIndex interSCCLiveMap of
                                Just l -> interSCCLiveMap
                                Nothing -> livenessPassSCCHelper succSCCIndex fnIr dag sccMap interSCCLiveMap
                        succLiveInVars = 
                            case Map.lookup succSCCIndex newSCCLiveMap of
                                Just l -> l
                                Nothing -> error (compilerError ("Liveness pass on successor failed to update live-in vars in SCC liveMap: succSCC=" ++ (show succSCCIndex)))
                        newLiveOutVars = Set.union interLiveOutVars succLiveInVars
                    in (newLiveOutVars, newSCCLiveMap)
                )
                (Set.empty, sccLiveMap)
                succSCCIndices
        initSCCLiveInVars = 
            case Map.lookup sccIndex sccLiveMap of
                Just l -> l
                Nothing -> Set.empty
        innerBBs = 
            map
                (\index ->
                    case Map.lookup index (functionIrBlocks fnIr) of
                        Just bb -> bb
                        Nothing -> error (compilerError ("Attempted to access basic block during liveness pass that does not exist: bbIndex=" ++ (show index)))
                )
                (Set.toList scc)
        (finalCumulativeLiveInVars, finalLiveOutVars) = 
            foldl
                (\(interCumulativeSCCLiveInVars, interLiveOutVars) bb ->
                    let newLiveOutVars = bbLivenessPass interLiveOutVars bb
                        newCumulativeSCCLiveInVars = Set.union interCumulativeSCCLiveInVars newLiveOutVars
                    in (newCumulativeSCCLiveInVars, newLiveOutVars)
                )
                (initSCCLiveInVars, liveOutVars)
                innerBBs
    in Map.insert sccIndex finalCumulativeLiveInVars recursedSCCLiveMap

-- deaggregates the SCC live map to a BB live map by
-- directly replicating the live-in variables for the SCC to each of its BBs
livenessPassDeaggregateSCCToBBHelper :: Map.Map Int (SCC Int) -> LiveMap -> LiveMap
livenessPassDeaggregateSCCToBBHelper sccMap sccLiveMap
    | Trace.trace 
        ("\n\nlivenessPassDeaggregateSCCToBBHelper -- " ++
            "\nsccLiveMap=" ++ (show sccLiveMap)
        )
        False = undefined
livenessPassDeaggregateSCCToBBHelper sccMap sccLiveMap = 
    let bbLiveMapNestedList = map (replicateSCCLiveInVarsToBB sccMap sccLiveMap) (Map.toList sccLiveMap)
    in (Map.fromList . concat) bbLiveMapNestedList

replicateSCCLiveInVarsToBB :: Map.Map Int (SCC Int) -> LiveMap -> (Int, Set.Set VariableIr) -> [(Int, Set.Set VariableIr)]
replicateSCCLiveInVarsToBB sccMap sccLiveMap (sccIndex, liveInVars) = 
    let bbIndices = 
            case Map.lookup sccIndex sccMap of
                Just s -> s
                Nothing -> error (compilerError ("Attempted to lookup SCC index during liveMap deaggregation that does not exist: sccIndex=" ++ (show sccIndex)))
    in map (\bbIndex -> (bbIndex, liveInVars)) (Set.toList bbIndices)

bbLivenessPass :: Set.Set VariableIr -> BasicBlockIr -> Set.Set VariableIr
bbLivenessPass liveVars bb
    | Trace.trace 
        ("\n\nbbLivenessPass -- " ++
            "\nbbIr=" ++ (show bb) ++
            "\nliveVars=" ++ (show liveVars)
        )
        False = undefined
bbLivenessPass liveVars bb = 
    foldl updateLiveVarsComm liveVars (bbIrCommands bb)

-- PHI FN LIVENESS ANALYSIS

updateLiveVarsPhi :: Set.Set VariableIr -> PhiFnIr -> Set.Set VariableIr
updateLiveVarsPhi liveVars phi = 
    foldr 
        (\(var, varPredMap) interLiveVars -> 
            Set.delete var (Set.union (getUsedVarsPredMap varPredMap) interLiveVars)
        )
        liveVars
        (Map.toList phi)

getUsedVarsPredMap :: Map.Map Int VariableIr -> Set.Set VariableIr
getUsedVarsPredMap predMap = 
    foldr
        (\(_, argVar) interLiveVars -> 
            Set.insert argVar interLiveVars
        )
        Set.empty
        (Map.toList predMap)

-- COMMAND LIVENESS ANALYSIS

updateLiveVarsComm :: Set.Set VariableIr -> CommandIr -> Set.Set VariableIr
updateLiveVarsComm liveVars comm = 
    let liveVarsAddedUsed = Set.union liveVars (getUsedVarsCommand comm)
        liveVarsRemovedAsn = 
            case (getAssignedVarsCommand comm) of
                Just var -> Set.delete var liveVarsAddedUsed
                Nothing -> liveVarsAddedUsed
    in liveVarsRemovedAsn

getUsedVarsCommand :: CommandIr -> Set.Set VariableIr
getUsedVarsCommand comm = 
    case comm of
        INIT_IR var -> 
            Set.empty
        ASN_PURE_IR asnVar asnPure ->
            getUsedVarsPure asnPure
        ASN_IMPURE_IR asnVar asnImpure ->
            getUsedVarsImpure asnImpure
        GOTO_BB_IR _ ->
            Set.empty
        SPLIT_BB_IR splitPure _ _ ->
            getUsedVarsPure splitPure
        RET_PURE_IR retPure ->
            getUsedVarsPure retPure

getAssignedVarsCommand :: CommandIr -> Maybe VariableIr
getAssignedVarsCommand comm = 
    case comm of
        INIT_IR var ->
            Nothing
        ASN_PURE_IR asnVar asnPure ->
            Just asnVar
        ASN_IMPURE_IR asnVar asnImpure ->
            Just asnVar
        GOTO_BB_IR _ ->
            Nothing
        SPLIT_BB_IR _ _ _ ->
            Nothing
        RET_PURE_IR retPure ->
            Nothing

getUsedVarsPure :: PureIr -> Set.Set VariableIr
getUsedVarsPure pure =
    case pure of
        PURE_BASE_IR base -> getUsedVarsPureBase base
        PURE_BINOP_IR (PureBinopIr _ _ base1 base2) -> Set.union (getUsedVarsPureBase base1) (getUsedVarsPureBase base2)
        PURE_UNOP_IR (PureUnopIr _ _ base) -> getUsedVarsPureBase base

getUsedVarsImpure :: ImpureIr -> Set.Set VariableIr
getUsedVarsImpure impure =
    case impure of
        IMPURE_BINOP_IR (ImpureBinopIr _ _ base1 base2) -> Set.union (getUsedVarsPureBase base1) (getUsedVarsPureBase base2)

getUsedVarsPureBase :: PureBaseIr -> Set.Set VariableIr
getUsedVarsPureBase base =
    case base of
        CONST_IR const -> Set.empty
        VAR_IR var -> Set.singleton var
