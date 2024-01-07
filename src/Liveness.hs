module Liveness (
    updateLiveVars,
    getUsedVarsCommand,
    getAssignedVarsCommand,
)
where

import Ir

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

updateLiveVars :: Set.Set VariableIr -> CommandIr -> Set.Set VariableIr
updateLiveVars liveVars comm = 
    let liveVarsAddedUsed = Set.union liveVars (getUsedVarsCommand comm)
        liveVarsRemovedAsn = 
            case (getAssignedVarsCommand comm) of
                Just name -> Set.delete name liveVarsAddedUsed
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
