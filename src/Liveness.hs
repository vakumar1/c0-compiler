module Liveness (
    constructIFG,
    IFG,
)

where

import Ir

import qualified Data.Set as Set
import qualified Data.Map as Map

constructIFG :: FunctionIr -> IFG
constructIFG fnIr = constructIFGHelper fnIr (bfsPredecessors fnIr) Set.empty Set.empty

constructIFGHelper :: FunctionIr -> [Int] -> Set.Set String -> IFG -> IFG
constructIFGHelper fnIr blocks liveVars initIFG = 
    let (_, finalIFG) = 
            foldl 
                (\(interLiveVars, interIFG) index -> 
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
                ifgAddedAsnEdges = foldr (\liveVar interIFG -> addEdgeToIFG (liveVar, (variableIrName asnVar)) interIFG) 
                                        initIFG 
                                        liveVars
            in (liveVarsRemovedAsn, ifgAddedAsnEdges)
        ASN_IMPURE_IR asnVar asnImpure ->
            let liveVarsAddedUsed = Set.union liveVars (getUsedVarsImpure asnImpure)
                liveVarsRemovedAsn = Set.delete (variableIrName asnVar) liveVarsAddedUsed
                ifgAddedAsnEdges = foldr (\liveVar interIFG -> addEdgeToIFG (liveVar, (variableIrName asnVar)) interIFG) 
                                        initIFG 
                                        liveVars
            in (liveVarsRemovedAsn, ifgAddedAsnEdges)
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

-- HELPERS

type IFG = Set.Set (String, String)

addEdgeToIFG :: (String, String) -> IFG -> IFG
addEdgeToIFG (var1, var2) ifg = Set.insert ((min var1 var2), (max var1 var2)) ifg
