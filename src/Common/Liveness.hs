module Common.Liveness (
    LiveMap,
    Coloring (..),
    livenessPass,
    updateLiveVarsPhi,
    getAssignedVarsPhi,
    getUsedVarsPhi,
    getUsedVarsPredMap,
    updateLiveVarsComm,
    getUsedVarsCommand,
    getAssignedVarsCommand,
)
where

import Model.Ir
import Model.Types
import Common.IrUtils
import Common.Graphs
import Common.Errors
import Common.Constants
import Common.Aliasing

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe

import qualified Text.Show.Pretty as Pretty
import qualified Debug.Trace as Trace

-- map from bb/SCC index to base (i.e., SSAId = 0) variables that are live-in at start of bb/SCC
type LiveMap = Map.Map Int (Set.Set VariableIr)

-- map from variable to a color (i.e. an Int) + mandatory stack colors
data Coloring = Coloring
    { coloringMap :: Map.Map VariableIr Int
    , coloringStackVars :: Set.Set VariableIr
    }
    deriving Show

-- LIVENESS PASS ON CFG -> returns map of (base) variables that are live-in for each BB

livenessPass :: FunctionIr -> TarjanResult Int -> AliasingCtx -> LiveMap
livenessPass fnIr tarjanResult aliasingCtx 
    | debugLivenessLogs && (Trace.trace 
        ("\n\nlivenessPass -- " ++
            "\nCFG=" ++ (Pretty.ppShow . functionIrCFG $ fnIr) ++
            "\nBBs=" ++ (Pretty.ppShow . functionIrBlocks $ fnIr) ++
            "\naliasingCtx=" ++ (Pretty.ppShow aliasingCtx)
        )
        False) = undefined
livenessPass fnIr tarjanResult aliasingCtx = 
    let (_, liveMap) = 
            livenessPassSCCHelper
                (tarjanResultRootSCC tarjanResult)
                fnIr
                tarjanResult
                aliasingCtx
                (Set.empty, Map.empty)
    in liveMap

-- constructs the liveness map - i.e., a map from the BB index to
-- the set of variables that are live-in to the BB for each BB in SCC
livenessPassSCCHelper :: Int -> FunctionIr -> TarjanResult Int -> AliasingCtx -> (Set.Set Int, LiveMap) -> (Set.Set Int, LiveMap)
livenessPassSCCHelper sccIndex fnIr tarjanResult aliasingCtx (visitedSCCs, liveMap)
    | debugLivenessLogs && (Trace.trace 
        ("\n\nlivenessPassSCCHelper -- " ++
            "\nsccIndex=" ++ (Pretty.ppShow sccIndex) ++
            "\ncurrSCCLiveMap=" ++ (Pretty.ppShow liveMap) ++
            "\ndag=" ++ (Pretty.ppShow (tarjanResultGraph tarjanResult)) ++
            "\naliasingCtx=" ++ (Pretty.ppShow aliasingCtx)
        )
        False) = undefined
livenessPassSCCHelper sccIndex fnIr tarjanResult aliasingCtx (visitedSCCs, liveMap) = 
    let 
        -- collect the successors of the SCC and for each
        -- apply livenessPassSCCHelper to populate the liveMap if the SCC has not been seen yet
        scc = 
            case Map.lookup sccIndex (tarjanResultMapToSCC tarjanResult) of
                Just s -> s
                Nothing -> error . compilerError $ "Attempted to access scc during liveMap construction that does not exist: sccIndex=" ++ (show sccIndex)
        succSCCIndices = 
            case Map.lookup sccIndex (graphSuccessors . tarjanResultGraph $ tarjanResult) of
                Just s -> s
                Nothing -> Set.empty
        (recursedVisitedSCCs, recursedLiveMap) = 
            foldr
                (\succSCCIndex (interVisitedSCCs, interLiveMap) ->
                    if Set.member succSCCIndex interVisitedSCCs
                        then (interVisitedSCCs, interLiveMap)
                        else livenessPassSCCHelper succSCCIndex fnIr tarjanResult aliasingCtx (interVisitedSCCs, interLiveMap)
                )
                (Set.insert sccIndex visitedSCCs, liveMap)
                succSCCIndices
        
        -- populate the liveMap with a map from each BB in the SCC
        -- to the set of vars that are live-in to the BB
        innerBBs = 
            map
                (getBB fnIr)
                (Set.toList scc)

        finalLiveMap = livenessPassSaturationHelper sccIndex fnIr aliasingCtx innerBBs recursedLiveMap
    in (recursedVisitedSCCs, finalLiveMap)

-- updates live in vars for each basic block and recursively calls itself
-- until live in vars reach saturation
livenessPassSaturationHelper :: Int -> FunctionIr -> AliasingCtx -> [BasicBlockIr] -> LiveMap -> LiveMap
livenessPassSaturationHelper sccIndex fnIr aliasingCtx bbs liveMap
    | debugLivenessLogs && (Trace.trace 
        ("\n\nlivenessPassSaturationHelper -- " ++
            "\nsccIndex=" ++ (Pretty.ppShow sccIndex) ++ 
            "\nbbs=" ++ (Pretty.ppShow . (map bbIndex) $ bbs) ++
            "\ncurrSCCLiveMap=" ++ (Pretty.ppShow liveMap) ++
            "\naliasingCtx=" ++ (Pretty.ppShow aliasingCtx)
        )
        False) = undefined
livenessPassSaturationHelper sccIndex fnIr aliasingCtx bbs liveMap = 
    let (finalLiveMapUpdated, finalLiveMap) = 
            foldr
                (\bb (interLiveMapUpdated, interLiveMap) ->
                    let 
                        -- collect the set of live-out vars as the union of 
                        -- the set of all live-in vars of all successor BBs (if they have been populated at this point)
                        succBbs = 
                            case Map.lookup (bbIndex bb) (graphSuccessors . functionIrCFG $ fnIr) of
                                Just s -> s
                                Nothing -> Set.empty
                        liveOutVars = 
                            foldr
                                (\succBbIndex interLiveOutVars ->
                                    case Map.lookup succBbIndex interLiveMap of
                                        Just l -> Set.union interLiveOutVars l
                                        Nothing -> interLiveOutVars
                                )
                                Set.empty
                                succBbs

                        -- apply the liveness pass to the set of live-out vars
                        -- and update the current set of live-in vars
                        -- record whether the set of live-in vars grew
                        initLiveInVars = 
                            case Map.lookup (bbIndex bb) interLiveMap of
                                Just l -> l
                                Nothing -> Set.empty
                        newLiveInVars = bbLivenessPass liveOutVars aliasingCtx fnIr bb
                        newLiveMap = Map.insert (bbIndex bb) newLiveInVars interLiveMap
                        newLiveMapUpdated = interLiveMapUpdated || ((Set.size initLiveInVars) /= (Set.size newLiveInVars))
                    in (newLiveMapUpdated, newLiveMap)
                )
                (False, liveMap)
                bbs
    in if finalLiveMapUpdated
            then livenessPassSaturationHelper sccIndex fnIr aliasingCtx bbs finalLiveMap
            else finalLiveMap

bbLivenessPass :: Set.Set VariableIr -> AliasingCtx -> FunctionIr -> BasicBlockIr -> Set.Set VariableIr
bbLivenessPass liveVars aliasingCtx fnIr bb
    | debugLivenessLogs && (Trace.trace 
        ("\n\nbbLivenessPass -- " ++
            "\nbbIr=" ++ (Pretty.ppShow bb) ++
            "\nliveVars=" ++ (Pretty.ppShow liveVars) ++
            "\naliasingCtx=" ++ (Pretty.ppShow aliasingCtx)
        )
        False) = undefined
bbLivenessPass liveVars aliasingCtx fnIr bb = 
    let updatedCommVars = foldl (updateLiveVarsComm aliasingCtx) liveVars (bbIrCommands bb)
        updatedPhiVars = updateLiveVarsPhi updatedCommVars (bbIrPhiFn bb)
        argVars = 
            if (bbIndex bb) == 0
                then Set.fromList (functionIrArgs fnIr)
                else Set.empty
    in Set.union updatedPhiVars argVars

-- PHI FN LIVENESS ANALYSIS

updateLiveVarsPhi :: Set.Set VariableIr -> PhiFnIr -> Set.Set VariableIr
updateLiveVarsPhi liveVars phi = 
    -- delete all assigned vars and insert all used vars to live vars
    Set.union (getUsedVarsPhi phi) (Set.difference liveVars (getAssignedVarsPhi phi))

getAssignedVarsPhi :: PhiFnIr -> Set.Set VariableIr
getAssignedVarsPhi phi = 
    foldr
        (\(var, varPredMap) interUsedVars ->
            Set.insert var interUsedVars
        )
        Set.empty
        (Map.toList phi)

getUsedVarsPhi :: PhiFnIr -> Set.Set VariableIr
getUsedVarsPhi phi = 
    foldr
        (\(var, varPredMap) interUsedVars ->
            Set.union (getUsedVarsPredMap varPredMap) interUsedVars
        )
        Set.empty
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

updateLiveVarsComm :: AliasingCtx -> Set.Set VariableIr -> CommandIr -> Set.Set VariableIr
updateLiveVarsComm aliasingCtx liveVars comm = 
    let 
        -- delete the assigned var
        -- and add all used vars
        liveVarsRemovedAsn = 
            case (getAssignedVarsCommand aliasingCtx comm) of
                Just var -> Set.delete var liveVars
                Nothing -> liveVars
        liveVarsAddedUsed = Set.union liveVarsRemovedAsn (getUsedVarsCommand aliasingCtx comm)
    in liveVarsAddedUsed

getUsedVarsCommand :: AliasingCtx -> CommandIr -> Set.Set VariableIr
getUsedVarsCommand aliasingCtx comm = 
    case comm of
        INIT_IR var -> 
            Set.empty
        ASN_PURE_IR memop asnVar asnPure ->
            let asnVarSet = 
                    if memopIrIsDeref memop
                        then Set.singleton asnVar
                        else Set.empty
                offsetSet = 
                    case memopIrOffset memop of
                        Just offsetPuB ->
                            getUsedVarsPureBase aliasingCtx offsetPuB
                        Nothing ->
                            Set.empty
            in Set.union (getUsedVarsPure aliasingCtx asnPure) (Set.union asnVarSet offsetSet)
        ASN_IMPURE_IR asnVar asnImpure ->
            getUsedVarsImpure aliasingCtx asnImpure
        GOTO_BB_IR _ ->
            Set.empty
        SPLIT_BB_IR splitPure _ _ ->
            getUsedVarsPure aliasingCtx splitPure
        RET_PURE_IR retPure ->
            getUsedVarsPure aliasingCtx retPure
        RET_IR ->
            Set.empty
        ABORT_IR ->
            Set.empty

getAssignedVarsCommand :: AliasingCtx -> CommandIr -> Maybe VariableIr
getAssignedVarsCommand aliasingCtx comm = 
    case comm of
        INIT_IR var ->
            Nothing
        ASN_PURE_IR memop asnVar asnPure ->
            if (Set.member asnVar (aliasingCtxStackVars aliasingCtx)) 
                    || memopIrIsDeref memop
                    || (Maybe.isJust . memopIrOffset $ memop)
                then Nothing
                else Just asnVar
        ASN_IMPURE_IR asnVar asnImpure ->
            Just asnVar
        GOTO_BB_IR _ ->
            Nothing
        SPLIT_BB_IR _ _ _ ->
            Nothing
        RET_PURE_IR retPure ->
            Nothing
        RET_IR ->
            Nothing
        ABORT_IR ->
            Nothing

getUsedVarsPure :: AliasingCtx -> PureIr -> Set.Set VariableIr
getUsedVarsPure aliasingCtx pure =
    case pure of
        PURE_BASE_IR base -> getUsedVarsPureBase aliasingCtx base
        PURE_BINOP_IR (PureBinopIr _ _ base1 base2) -> Set.union (getUsedVarsPureBase aliasingCtx base1) (getUsedVarsPureBase aliasingCtx base2)
        PURE_UNOP_IR (PureUnopIr _ _ base) -> getUsedVarsPureBase aliasingCtx base
        PURE_MEMOP_IR var memop ->
            let varSet = 
                    if excludedStackVar aliasingCtx var
                        then Set.empty
                        else Set.singleton var
                offsetSet = 
                    case memopIrOffset memop of
                        Just offsetPuB ->
                            getUsedVarsPureBase aliasingCtx offsetPuB
                        Nothing ->
                            Set.empty
            in Set.union varSet offsetSet

getUsedVarsImpure :: AliasingCtx -> ImpureIr -> Set.Set VariableIr
getUsedVarsImpure aliasingCtx impure =
    case impure of
        IMPURE_BINOP_IR (ImpureBinopIr _ _ base1 base2) -> Set.union (getUsedVarsPureBase aliasingCtx base1) (getUsedVarsPureBase aliasingCtx base2)
        IMPURE_FNCALL_IR (ImpureFnCallIr _ base _) -> mconcat (map (getUsedVarsPureBase aliasingCtx) base)

getUsedVarsPureBase :: AliasingCtx -> PureBaseIr -> Set.Set VariableIr
getUsedVarsPureBase aliasingCtx base =
    case base of
        CONST_IR const -> Set.empty
        VAR_IR var -> 
            if excludedStackVar aliasingCtx var 
                then Set.empty
                else Set.singleton var

excludedStackVar :: AliasingCtx -> VariableIr -> Bool
excludedStackVar aliasingCtx var = 
    Set.member var (aliasingCtxStackVars aliasingCtx)

