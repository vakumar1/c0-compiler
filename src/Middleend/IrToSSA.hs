module Middleend.IrToSSA (
    irToMaximalSSA,
) where

import Common.Errors
import Common.Graphs
import Model.Ir
import Common.Liveness
import Common.Constants

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import qualified Debug.Trace as Trace
import qualified Text.Show.Pretty as Pretty

-- MAXIMAL SSA PASS ON CFG -> 
-- - updates SSA Id for each variable (in phi-functions and commands) to conform to SSA form 
-- - injects live vars into successor phi-functions
-- - returns 
-- - (i) the updated FnIr
-- - (ii) the SCC DAG + metadata constructed as a byproduct

irToMaximalSSA :: FunctionIr -> TarjanResult Int -> FunctionIr
irToMaximalSSA fnIr tarjanResult
    | debugProcessingLogs && (Trace.trace 
        ("\n\nirToMaximalSSA -- " ++
            "\nfnIr=" ++ (Pretty.ppShow fnIr)
        )
        False) = undefined
irToMaximalSSA fnIr tarjanResult =
    let bbLiveMap = livenessPass fnIr tarjanResult
        initPhiFnIr = initPhiFn fnIr bbLiveMap
        versionedFnIr = versionPass initPhiFnIr bbLiveMap
    in versionedFnIr

-- MAXIMAL SSA PASS ON BB -> updates BB (phi functions and commands) to conform to SSA form by
-- (i) updating variables to most recent version
-- (ii) updating the most recent version for asn commands

-- map from variable name to most recent VariableIr version of variable
type VariableIrVersion = Map.Map String VariableIr

updateVarVersion :: VariableIr -> VariableIrVersion -> (VariableIr, VariableIrVersion)
updateVarVersion (VariableIr name _ ty tmp) versions = 
    let newCount = 
            case Map.lookup name versions of
                Just currVar -> (variableIrSSAId currVar) + 1
                Nothing -> 0
        newVar = VariableIr name newCount ty tmp
        newVersions = Map.insert name newVar versions
    in (newVar, newVersions)

-- initializes phi-fn of each BB with live in vars
initPhiFn :: FunctionIr -> LiveMap -> FunctionIr
initPhiFn fnIr liveMap = 
    let initPhiBBs = 
            map
                (\index ->
                    let bb = 
                            case Map.lookup index (functionIrBlocks fnIr) of
                                Just b -> b
                                Nothing -> error . compilerError $ "Attempted to access bb index supplied from BFS ordering that does not exist: bbIndex=" ++ (show index)
                        liveInVars = 
                            case Map.lookup index liveMap of
                                Just l -> l
                                Nothing -> Set.empty
                        newPhiFn = 
                            foldr
                                (\var interPhiFn->
                                    Map.insert var Map.empty interPhiFn
                                )
                                Map.empty
                                liveInVars
                        initPhiBb = 
                            BasicBlockIr
                                (functionIrIdentifier fnIr)
                                index
                                newPhiFn
                                (bbIrCommands bb)
                    in initPhiBb
                )
                [0..((length . functionIrBlocks $ fnIr) - 1)]
    in addBbsToFunction initPhiBBs fnIr

-- version pass converts variable versions within BBs in loose BFS order
versionPass :: FunctionIr -> LiveMap -> FunctionIr
versionPass fnIr bbLiveMap
    | debugProcessingLogs && (Trace.trace 
        ("\n\nversionPass -- " ++
            "\nbbLiveMap=" ++ (Pretty.ppShow bbLiveMap)
        )
        False) = undefined
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
                [0..((length . functionIrBlocks $ fnIr) - 1)]
    in newFnIr

bbIrToMaximalSSA :: BasicBlockIr -> VariableIrVersion -> (BasicBlockIr, VariableIrVersion)
bbIrToMaximalSSA bb versions
    | debugProcessingLogs && (Trace.trace 
        ("\n\nbbIrToMaximalSSA -- " ++
            "\nbbIr=" ++ (Pretty.ppShow bb) ++
            "\nversions=" ++ (Pretty.ppShow versions)
        )
        False) = undefined
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
    in (BasicBlockIr (bbIrFnName bb) (bbIndex bb) (Map.fromList newPhiFnIrPairs) newComms, commsUpdateVersions)

commandIrToMaximalSSA :: CommandIr -> VariableIrVersion -> (CommandIr, VariableIrVersion)
commandIrToMaximalSSA comm versions = 
    let m_assignedVar = getAssignedVarsCommand comm
        updatedAsnVarVersions = 
            case m_assignedVar of
                Nothing ->
                    versions
                Just var ->
                    let (newVar, newVersions) = updateVarVersion var versions
                    in newVersions
    in 
        case comm of
            INIT_IR var -> 
                (comm, versions)
            ASN_PURE_IR memop asnVar asnPure ->
                let newPure = pureIrToMaximalSSA asnPure versions
                    newMemop = memopIrToMaximalSSA memop versions
                    newAsnVar = varIrToMaximalSSA asnVar updatedAsnVarVersions
                    newComm = ASN_PURE_IR newMemop newAsnVar newPure
                in (newComm, updatedAsnVarVersions)
            ASN_IMPURE_IR asnVar asnImpure ->
                let newImpure = impureIrToMaximalSSA asnImpure versions
                    newAsnVar = varIrToMaximalSSA asnVar updatedAsnVarVersions
                    newComm = ASN_IMPURE_IR newAsnVar newImpure
                in (newComm, updatedAsnVarVersions)
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
            RET_IR ->
                (comm, versions)
            ABORT_IR ->
                (comm, versions)

pureIrToMaximalSSA :: PureIr -> VariableIrVersion -> PureIr
pureIrToMaximalSSA pure versions =
    case pure of
        PURE_BASE_IR base ->
            PURE_BASE_IR (pureBaseIrToMaximalSSA base versions)
        PURE_BINOP_IR (PureBinopIr cat ty base1 base2) ->
            PURE_BINOP_IR (PureBinopIr cat ty (pureBaseIrToMaximalSSA base1 versions) (pureBaseIrToMaximalSSA base2 versions))
        PURE_UNOP_IR (PureUnopIr cat ty base) ->
            PURE_UNOP_IR (PureUnopIr cat ty (pureBaseIrToMaximalSSA base versions))
        PURE_MEMOP_IR var memop ->
            PURE_MEMOP_IR var (memopIrToMaximalSSA memop versions)

memopIrToMaximalSSA :: MemopIr -> VariableIrVersion -> MemopIr
memopIrToMaximalSSA memop versions = 
    case memopIrOffset memop of
        Just offsetPuB ->
            MemopIr (memopIrIsDeref memop) (Just (pureBaseIrToMaximalSSA offsetPuB versions)) (memopIrRetType memop)
        Nothing ->
            memop

impureIrToMaximalSSA :: ImpureIr -> VariableIrVersion -> ImpureIr
impureIrToMaximalSSA impure versions =
    case impure of
        IMPURE_BINOP_IR (ImpureBinopIr cat ty base1 base2) ->
            IMPURE_BINOP_IR (ImpureBinopIr cat ty (pureBaseIrToMaximalSSA base1 versions) (pureBaseIrToMaximalSSA base2 versions))
        IMPURE_FNCALL_IR (ImpureFnCallIr name base ty) ->
            IMPURE_FNCALL_IR (ImpureFnCallIr name (map (\b -> pureBaseIrToMaximalSSA b versions) base) ty)

pureBaseIrToMaximalSSA :: PureBaseIr -> VariableIrVersion -> PureBaseIr
pureBaseIrToMaximalSSA base versions =
    case base of
        CONST_IR _ -> base
        VAR_IR baseVar -> VAR_IR (varIrToMaximalSSA baseVar versions)

varIrToMaximalSSA :: VariableIr -> VariableIrVersion -> VariableIr
varIrToMaximalSSA var versions = 
    case Map.lookup (variableIrName var) versions of
        Just currVar -> currVar
        Nothing -> var

-- PHI FUNCTION INJECTION: inject variable passage from bb to all successors

bbInjectPhiFn :: FunctionIr -> LiveMap -> VariableIrVersion -> Int -> FunctionIr
bbInjectPhiFn fnIr liveMap versions predBbIndex = 
    let succBbIndices = 
            case Map.lookup predBbIndex (graphSuccessors (functionIrCFG fnIr)) of
                Just succs -> succs
                Nothing -> Set.empty
        injectedSuccBBs = 
            map
                (\succIndex ->
                    case Map.lookup succIndex (functionIrBlocks fnIr) of
                        Just bb ->  succBbInjectPhiFn bb liveMap versions predBbIndex
                        Nothing -> error . compilerError $ "BB Phi-Fn injection succ map lookup encountered basic block index not in fnIr" ++ (show succIndex)
                )
                (Set.toList succBbIndices)
    in addBbsToFunction injectedSuccBBs fnIr

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
                    let predMapMatches = 
                            filter
                                (\(var, _) -> variableIrBaseEq var baseVar)
                                (Map.toList interPhiFn)
                    in if (length predMapMatches) /= 1
                            then error . compilerError $ "BB does not have exactly one phi-fn for a live in var: " ++
                                                            "bb=" ++ (show bbIr) ++ 
                                                            " baseVar=" ++ (show baseVar) ++
                                                            " phi-rn pred count=" ++ (show . length $ predMapMatches)
                            else
                                let (var, initPredMap) = head predMapMatches
                                    newPredMap = 
                                        case Map.lookup (variableIrName baseVar) versions of
                                            Just currVar -> Map.insert predBbIndex currVar initPredMap
                                            Nothing -> error (compilerError ("Incorrect liveness analysis resulted in variable " ++ (show baseVar) ++ 
                                                            " that is live-in at successor and not live-out from predecessor"))
                                in Map.insert var newPredMap interPhiFn
                )
                (bbIrPhiFn bbIr)
                liveVars
    in BasicBlockIr (bbIrFnName bbIr) (bbIndex bbIr) newPhiFn (bbIrCommands bbIr)
