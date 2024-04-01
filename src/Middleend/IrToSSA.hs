module Middleend.IrToSSA (
    irToMaximalSSA,
) where

import Common.Errors
import Common.Graphs
import Model.Ir
import Common.Liveness

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

-- MAXIMAL SSA PASS ON CFG -> 
-- - updates SSA Id for each variable (in phi-functions and commands) to conform to SSA form 
-- - injects live vars into successor phi-functions
-- - returns 
-- - (i) the updated FnIr
-- - (ii) the SCC DAG + metadata constructed as a byproduct

irToMaximalSSA :: FunctionIr -> (Set.Set Int, DirectedGraph Int, Map.Map Int (SCC Int)) -> FunctionIr
irToMaximalSSA fnIr (leaves, dag, sccMap) =
    let bbLiveMap = livenessPass fnIr (leaves, dag, sccMap)
        versionedFnIr = versionPass fnIr bbLiveMap
    in versionedFnIr

-- MAXIMAL SSA PASS ON BB -> updates BB (phi functions and commands) to conform to SSA form by
-- (i) updating variables to most recent version
-- (ii) updating the most recent version for asn commands

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

-- version pass converts variable versions within BBs in loose BFS order
versionPass :: FunctionIr -> LiveMap -> FunctionIr
versionPass fnIr bbLiveMap = 
    let (newFnIr, _) = 
            foldl
                (\(interFnIr, interVersions) index ->
                    case Map.lookup index (functionIrBlocks interFnIr) of
                        Just bb -> 
                            let (updatedBb, newVersions) = bbIrToMaximalSSA bb interVersions
                                currUpdatedFnIr = addBbsToFunction [updatedBb] interFnIr
                                succUpdatedFnIr = bbInjectPhiFn currUpdatedFnIr bbLiveMap newVersions (bbIndex updatedBb)
                            in (succUpdatedFnIr, newVersions)
                        Nothing -> error (compilerError ("Attempted to access bb index supplied from BFS ordering that does not exist: bbIndex=" ++ (show index)))
                )
                (fnIr, Map.empty)
                (looseBbOrdering fnIr)
    in newFnIr

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
        SPLIT_BB_IR condPure splitLeft splitRight ->
            let newPure = pureIrToMaximalSSA condPure versions
                newComm = SPLIT_BB_IR newPure splitLeft splitRight
            in (newComm, versions)
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
            case Map.lookup predBbIndex (graphSuccessors (functionIrCFG fnIr)) of
                Just succs -> succs
                Nothing -> Set.empty
    in foldr
            (\id interFnIr ->
                case Map.lookup id (functionIrBlocks interFnIr) of
                    Just bb ->  
                        let newBb = succBbInjectPhiFn bb liveMap versions predBbIndex
                        in addBbsToFunction [newBb] interFnIr
                    Nothing -> error (compilerError ("BB Phi-Fn injection succ map lookup encountered basic block index not in fnIr" ++ (show id)))
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
