module IrToSSA (
    irToMaximalSSA,
) where

import Errors
import Ir
import Liveness

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

-- MAXIMAL SSA PASS ON CFG -> 
-- - updates SSA Id for each variable (in phi-functions and commands) to conform to SSA form 
-- - injects live vars into successor phi-functions

irToMaximalSSA :: FunctionIr -> FunctionIr
irToMaximalSSA fnIr =
    let order = bfsSuccessors fnIr
        liveMap = livenessPass fnIr
        (newFnIr, _) = 
            foldl
                (\(interFnIr, interVersions) index ->
                    case Map.lookup index (functionIrBlocks interFnIr) of
                        Just bb -> 
                            let (updatedBb, newVersions) = bbIrToMaximalSSA bb interVersions
                                currUpdatedFnIr = addBbsToFunction interFnIr [updatedBb]
                                succUpdatedFnIr = bbInjectPhiFn currUpdatedFnIr liveMap newVersions (bbIndex updatedBb)
                            in (succUpdatedFnIr, newVersions)
                )
                (fnIr, Map.empty)
                order
    in newFnIr

-- LIVENESS PASS ON CFG -> returns map of variables that are live-in for each BB

livenessPass :: FunctionIr -> LiveMap
livenessPass fnIr = 
    let order = bfsPredecessors fnIr
        (liveMap, _) = 
            foldl 
                (\(interLiveMap, interLiveVars) index ->
                    case Map.lookup index (functionIrBlocks fnIr) of
                        Just bb -> 
                            let newLiveVars = bbLivenessPass interLiveVars bb
                                newLiveMap = Map.insert index newLiveVars interLiveMap
                            in (newLiveMap, newLiveVars)
                        Nothing -> (interLiveMap, interLiveVars)
                )
                (Map.empty, Set.empty)
                order
    in liveMap

bbLivenessPass :: Set.Set VariableIr -> BasicBlockIr -> Set.Set VariableIr
bbLivenessPass liveVars bb = 
    foldl updateLiveVarsComm liveVars (bbIrCommands bb)

-- MAXIMAL SSA PASS ON BB -> updates BB (phi functions and commands) to conform to SSA form

bbIrToMaximalSSA :: BasicBlockIr -> VariableIrVersion -> (BasicBlockIr, VariableIrVersion)
bbIrToMaximalSSA bb versions = 
    let (newPhiFnIrPairs, phiUpdateVersions) = 
            foldr
                (\(var, predMap) (interPhiPairs, interVersions) ->
                    let (newVar, newVersions) = updateVarVersion var interVersions
                    in ((newVar, predMap):interPhiPairs, newVersions)
                )
                ([], versions)
                (Map.toList (bbIrPhiFn bb))
        (newComms, commsUpdateVersions) = 
            foldr
                (\comm (interComms, interVersions) ->
                    let (newComm, newVersions) = commandIrToMaximalSSA comm interVersions
                    in (newComm:interComms, newVersions)
                )
                ([], phiUpdateVersions)
                (bbIrCommands bb)
    in (BasicBlockIr (bbIndex bb) (Map.fromList newPhiFnIrPairs) newComms, commsUpdateVersions)

commandIrToMaximalSSA :: CommandIr -> VariableIrVersion -> (CommandIr, VariableIrVersion)
commandIrToMaximalSSA comm versions = 
    case comm of
        INIT_IR var -> 
            (comm, versions)
        ASN_PURE_IR asnVar asnPure ->
            let newPure = pureIrToMaximalSSA asnPure versions
                (newVar, newVersions) = updateVarVersion asnVar versions 
                newComm = ASN_PURE_IR newVar newPure
             in (newComm, newVersions)
        ASN_IMPURE_IR asnVar asnImpure ->
            let newImpure = impureIrToMaximalSSA asnImpure versions
                (newVar, newVersions) = updateVarVersion asnVar versions
                newComm = ASN_IMPURE_IR newVar newImpure
             in (newComm, newVersions)
        GOTO_BB_IR _ ->
            (comm, versions)
        RET_PURE_IR retPure ->
            let newPure = pureIrToMaximalSSA retPure versions
                newComm = RET_PURE_IR newPure
             in (newComm, versions)

pureIrToMaximalSSA :: PureIr -> VariableIrVersion -> PureIr
pureIrToMaximalSSA pure versions =
    case pure of
        PURE_BASE_IR base ->
            PURE_BASE_IR (pureBaseIrToMaximalSSA base versions)
        PURE_BINOP_IR (PureBinopIr cat ty base1 base2) ->
            PURE_BINOP_IR (PureBinopIr cat ty (pureBaseIrToMaximalSSA base1 versions) (pureBaseIrToMaximalSSA base2 versions))
        PURE_UNOP_IR (PureUnopIr cat ty base) ->
            PURE_UNOP_IR (PureUnopIr cat ty (pureBaseIrToMaximalSSA base versions))

impureIrToMaximalSSA :: ImpureIr -> VariableIrVersion -> ImpureIr
impureIrToMaximalSSA impure versions =
    case impure of
        IMPURE_BINOP_IR (ImpureBinopIr cat ty base1 base2) ->
            IMPURE_BINOP_IR (ImpureBinopIr cat ty (pureBaseIrToMaximalSSA base1 versions) (pureBaseIrToMaximalSSA base2 versions))

pureBaseIrToMaximalSSA :: PureBaseIr -> VariableIrVersion -> PureBaseIr
pureBaseIrToMaximalSSA base versions =
    case base of
        CONST_IR _ -> base
        VAR_IR baseVar -> 
            case Map.lookup (variableIrName baseVar) versions of
                Just currVar -> VAR_IR currVar
                -- variable use in a pure IR must succeed the assignment of the variable
                Nothing -> error (compilerError ("Uncaught use of variable before assignment: " ++ (show baseVar)))

-- PHI FUNCTION INJECTION: inject variable passage from bb to all successors

bbInjectPhiFn :: FunctionIr -> LiveMap -> VariableIrVersion -> Int -> FunctionIr
bbInjectPhiFn fnIr liveMap versions predBbIndex = 
    let succBbIndices = 
            case Map.lookup predBbIndex (functionIrSuccessorMap fnIr) of
                Just succs -> succs
                Nothing -> Set.empty
    in foldr
            (\id interFnIr ->
                case Map.lookup id (functionIrBlocks interFnIr) of
                    Just bb ->  
                        let newBb = succBbInjectPhiFn bb liveMap versions predBbIndex
                        in addBbsToFunction interFnIr [newBb]
                    Nothing -> interFnIr
            )
            fnIr
            succBbIndices

-- for each live-in variable for the BB, inject the curr Version of the variable from the predecessor BB
succBbInjectPhiFn :: BasicBlockIr -> LiveMap -> VariableIrVersion -> Int -> BasicBlockIr
succBbInjectPhiFn bbIr liveMap versions predBbIndex = 
    let liveVars = 
            case Map.lookup (bbIndex bbIr) liveMap of
                Just s -> s
                Nothing -> Set.empty
        newPhiFn = 
            foldr
                (\baseVar interPhiFn ->
                    let initPredMap = 
                            case Map.lookup baseVar interPhiFn of
                                Just predMap -> predMap
                                Nothing -> Map.empty
                        newPredMap = 
                            case Map.lookup (variableIrName baseVar) versions of
                                Just currVar -> Map.insert predBbIndex currVar initPredMap
                                Nothing -> error (compilerError ("Incorrect liveness analysis resulted in variable " ++ (show baseVar) ++ 
                                                " that is live-in at successor and not live-out from predecessor"))
                    in Map.insert baseVar newPredMap interPhiFn
                )
                (bbIrPhiFn bbIr)
                liveVars
    in BasicBlockIr (bbIndex bbIr) newPhiFn (bbIrCommands bbIr)

-- SSA Data Types and Helpers

-- map from bb index to (base; i.e., SSAId = 0) variables that are live-in at start of bb
type LiveMap = Map.Map Int (Set.Set VariableIr)

-- map from variable name to most recent VariableIr version of variable
type VariableIrVersion = Map.Map String VariableIr

updateVarVersion :: VariableIr -> VariableIrVersion -> (VariableIr, VariableIrVersion)
updateVarVersion (VariableIr name _ ty tmp) versions = 
    let currCount = 
            case Map.lookup name versions of
                Just currVar -> (variableIrSSAId currVar)
                Nothing -> 0
        newVar = VariableIr name (currCount + 1) ty tmp
        newVersions = Map.insert name newVar versions
    in (newVar, newVersions)
