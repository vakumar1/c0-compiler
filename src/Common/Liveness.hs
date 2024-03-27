module Common.Liveness (
    updateLiveVarsPhi,
    getUsedVarsPredMap,
    updateLiveVarsComm,
    getUsedVarsCommand,
    getAssignedVarsCommand,
)
where

import Model.Ir

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

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
