module IrToSSA (
    irToMaximalSSA
) where

import Ir

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe

-- IR -> MAXIMAL SSA 

irToMaximalSSA :: FunctionIr -> FunctionIr
irToMaximalSSA fnIr = 
    let idList = bfsSuccessors fnIr
        blockList = Maybe.catMaybes (map (\id -> Map.lookup id (functionIrBlocks fnIr)) idList)
        (newBlocks, _) = foldr irToMaximalSSAFoldFn (Map.empty, Map.empty) blockList
    in FunctionIr newBlocks (functionIrPredecessorMap fnIr) (functionIrSuccessorMap fnIr) (functionIrTerminators fnIr)

irToMaximalSSAFoldFn :: BasicBlockIr -> (Map.Map Int BasicBlockIr, Map.Map String Int) -> (Map.Map Int BasicBlockIr, Map.Map String Int)
irToMaximalSSAFoldFn bb (interBlocks, interCtr) = 
    let (newBb, newCtr) = bbIrToMaximalSSA bb interCtr
        newBlocks = Map.insert (bbIndex newBb) newBb interBlocks
    in (newBlocks, newCtr)

bbIrToMaximalSSA :: BasicBlockIr -> Map.Map String Int -> (BasicBlockIr, Map.Map String Int)
bbIrToMaximalSSA bb varCtr = 
    let (newComms, newCtr) = foldr bbIrToMaximalSSAFoldFn ([], varCtr) (bbIrCommands bb)
    in (BasicBlockIr (bbIndex bb) newComms, newCtr)

bbIrToMaximalSSAFoldFn :: CommandIr -> ([CommandIr], Map.Map String Int) -> ([CommandIr], Map.Map String Int)
bbIrToMaximalSSAFoldFn comm (prevComms, varCtr) = 
    case comm of
        INIT_IR (VariableIr name _ _) -> 
            let newComm = comm
                newComms = newComm:prevComms
                newCtr = Map.insert name 0 varCtr
            in (newComms, newCtr)
        ASN_PURE_IR asnVar asnPure -> 
            let newPure = pureIrToMaximalSSA asnPure varCtr
                (newVar, newCtr) = prependCtr asnVar True varCtr
                newComm = ASN_PURE_IR newVar newPure
                newComms = newComm:prevComms
            in (newComms, newCtr)
        ASN_IMPURE_IR asnVar asnImpure ->
            let newImpure = impureIrToMaximalSSA asnImpure varCtr
                (newVar, newCtr) = prependCtr asnVar True varCtr
                newComm = ASN_IMPURE_IR newVar newImpure
                newComms = newComm:prevComms
            in (newComms, newCtr)
        GOTO_BB_IR _ -> 
            let newComm = comm
                newComms = newComm:prevComms
            in (newComms, varCtr)
        RET_PURE_IR retPure ->
            let newPure = pureIrToMaximalSSA retPure varCtr
                newComm = RET_PURE_IR newPure
                newComms = newComm:prevComms
            in (newComms, varCtr)

pureIrToMaximalSSA :: PureIr -> Map.Map String Int -> PureIr
pureIrToMaximalSSA pure varCtr = 
    case pure of
        PURE_BASE_IR base -> 
            PURE_BASE_IR (pureBaseIrToMaximalSSA base varCtr)
        PURE_BINOP_IR (PureBinopIr cat ty base1 base2) -> 
            PURE_BINOP_IR (PureBinopIr cat ty (pureBaseIrToMaximalSSA base1 varCtr) (pureBaseIrToMaximalSSA base2 varCtr))
        PURE_UNOP_IR (PureUnopIr cat ty base) -> 
            PURE_UNOP_IR (PureUnopIr cat ty (pureBaseIrToMaximalSSA base varCtr))

impureIrToMaximalSSA :: ImpureIr -> Map.Map String Int -> ImpureIr
impureIrToMaximalSSA impure varCtr = 
    case impure of
        IMPURE_BINOP_IR (ImpureBinopIr cat ty base1 base2) ->
            IMPURE_BINOP_IR (ImpureBinopIr cat ty (pureBaseIrToMaximalSSA base1 varCtr) (pureBaseIrToMaximalSSA base2 varCtr))

pureBaseIrToMaximalSSA :: PureBaseIr -> Map.Map String Int -> PureBaseIr
pureBaseIrToMaximalSSA base varCtr = 
    case base of
        CONST_IR _ -> base
        VAR_IR baseVar -> 
            let (newBaseVar, _) = prependCtr baseVar False varCtr
            in VAR_IR newBaseVar

prependCtr :: VariableIr -> Bool -> Map.Map String Int -> (VariableIr, Map.Map String Int)
prependCtr var incr varCtr = 
    let ctr = 
            case Map.lookup (variableIrName var) varCtr of
                Just ctr -> ctr
                Nothing -> 0
        newName = (show ctr) ++ ":" ++ (variableIrName var)
        newVar = VariableIr newName (variableIrType var) (variableIrTemp var)
        newMap = 
            if incr
                then Map.insert (variableIrName var) (ctr + 1) varCtr
                else varCtr
    in (newVar, newMap)
