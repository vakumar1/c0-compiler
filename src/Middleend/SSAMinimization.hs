module Middleend.SSAMinimization (
    irToMinimalSSA,
)
where

import Model.Ir
import Common.IrUtils
import Common.Constants

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Debug.Trace as Trace
import qualified Text.Show.Pretty as Pretty

-- MINIMAL SSA PASS -> attempts to remove phi-fns by adding mappings between redundant versioned variables

irToMinimalSSA :: FunctionIr -> FunctionIr
irToMinimalSSA fnIr = 
    let varReduction = variableReductionPass fnIr
        minimalSSAFnIr = variableReplacementPass varReduction fnIr
    in minimalSSAFnIr

type VariableIrReduction = Map.Map VariableIr VariableIr

getDeepReduction :: VariableIrReduction -> VariableIr -> VariableIr
getDeepReduction varReduction varIr = 
    case Map.lookup varIr varReduction of
        Just reducedVar -> getDeepReduction varReduction reducedVar
        Nothing -> varIr

-- variable reduction pass - construct map storing variable reduction on redundant phi-fns
variableReductionPass :: FunctionIr -> VariableIrReduction
variableReductionPass fnIr = variableReductionHelper Map.empty fnIr

variableReductionHelper :: VariableIrReduction -> FunctionIr -> VariableIrReduction
variableReductionHelper varReduction fnIr
    | debugSSAMinimization && (Trace.trace 
        ("\nvariableReductionHelper -- " ++
            "\nvarReduction=" ++ (Pretty.ppShow varReduction)
        )
        False) = undefined
variableReductionHelper varReduction fnIr = 
    let updatedVarReduction = 
            foldr
                (\index interVarReduction ->
                    bbIrVariableReductionPass interVarReduction (getBB fnIr index)
                )
                varReduction
                [0..((length . functionIrBlocks $ fnIr) - 1)]
    in 
        if (Map.size updatedVarReduction /= Map.size varReduction)
            then variableReductionHelper updatedVarReduction fnIr
            else updatedVarReduction
        
bbIrVariableReductionPass :: VariableIrReduction -> BasicBlockIr -> VariableIrReduction
bbIrVariableReductionPass varReduction bbIr
    | debugSSAMinimization && (Trace.trace 
        ("\nbbIrVariableReductionPass -- " ++
            "\nbbIndex=" ++ (show $ bbIndex bbIr) ++
            "\nbbIrPhiFn=" ++ (Pretty.ppShow $ bbIrPhiFn bbIr) ++
            "\nvarReduction=" ++ (Pretty.ppShow varReduction)
        )
        False) = undefined
bbIrVariableReductionPass varReduction bbIr = 
    foldr
        (\predMap interVarReduction ->
            phiPredMapVariableReductionPass interVarReduction predMap
        )
        varReduction
        (Map.toList $ bbIrPhiFn bbIr)

phiPredMapVariableReductionPass :: VariableIrReduction -> (VariableIr, Map.Map Int VariableIr) -> VariableIrReduction
phiPredMapVariableReductionPass varReduction (asnVar, predArgs) = 
    let varArgs = 
            foldr
                (\(_, varIr) interVarArgs ->
                    Set.insert (getDeepReduction varReduction varIr) interVarArgs
                )
                Set.empty
                (Map.toList predArgs)
    in 
        case (Set.toList varArgs) of
            uniqueArg : [] | uniqueArg /= asnVar ->
                Map.insert asnVar uniqueArg varReduction
            _ ->
                varReduction

-- variable replacement pass - use the constructed variable reduction to
-- - remove redundant phi-fns
-- - replace any corresponding variables with their reduced version

variableReplacementPass :: VariableIrReduction -> FunctionIr -> FunctionIr
variableReplacementPass varReduction fnIr = 
    let newBbs =
            map
                (\(_, bb) -> bbIrVariableReplacement varReduction bb)
                (Map.toList $ functionIrBlocks fnIr)
    in addBbsToFunction newBbs fnIr

bbIrVariableReplacement :: VariableIrReduction -> BasicBlockIr -> BasicBlockIr
bbIrVariableReplacement varReduction bb
    | debugSSAMinimization && (Trace.trace 
        ("\nbbIrVariableReplacement -- " ++
            "\nbbIr=" ++ (Pretty.ppShow bb) ++
            "\nvarReduction=" ++ (Pretty.ppShow varReduction)
        )
        False) = undefined
bbIrVariableReplacement varReduction bb = 
    let newPhiFn = phiFnIrRemovalVariableReplacement varReduction (bbIrPhiFn bb)
        newComms = map (commandIrVariableReplacement varReduction) (bbIrCommands bb)
    in
        BasicBlockIr
            (bbIrFnName bb)
            (bbIndex bb)
            newPhiFn
            newComms

phiFnIrRemovalVariableReplacement :: VariableIrReduction -> PhiFnIr -> PhiFnIr
phiFnIrRemovalVariableReplacement varReduction phiFnIr = 
    foldr
        (\(asnVar, predMap) interPhiFnIr ->
            let updatedAsnVar = varIrVariableReplacement varReduction asnVar
                (uniqueArgs, updatedPredMap) = 
                    foldr
                        (\(bbIndex, varIr) (interUniqueArgs, interUpdatedPredMap) ->
                            let updatedPredVar = varIrVariableReplacement varReduction varIr
                                newUniqueArgs = Set.insert updatedPredVar interUniqueArgs
                                newUpdatedPredMap = Map.insert bbIndex updatedPredVar interUpdatedPredMap
                            in (newUniqueArgs, newUpdatedPredMap)
                        )
                        (Set.empty, Map.empty)
                        (Map.toList predMap)
            in
                case (Set.toList uniqueArgs) of
                    uniqueArg : [] | uniqueArg == updatedAsnVar ->
                        interPhiFnIr
                    _ ->
                        Map.insert updatedAsnVar updatedPredMap interPhiFnIr
        )
        Map.empty
        (Map.toList phiFnIr)

commandIrVariableReplacement :: VariableIrReduction -> CommandIr -> CommandIr
commandIrVariableReplacement varReduction comm = 
    case comm of
        INIT_IR var -> 
            INIT_IR (varIrVariableReplacement varReduction var)
        ASN_PURE_IR memop asnVar asnPure ->
            ASN_PURE_IR (memopIrVariableReplacement varReduction memop) (varIrVariableReplacement varReduction asnVar) (pureIrVariableReplacement varReduction asnPure) 
        ASN_IMPURE_IR asnVar asnImpure ->
            ASN_IMPURE_IR (varIrVariableReplacement varReduction asnVar) (impureIrToVariableReplacement varReduction asnImpure) 
        GOTO_BB_IR _ ->
            comm
        SPLIT_BB_IR condPure splitLeft splitRight ->
            SPLIT_BB_IR (pureIrVariableReplacement varReduction condPure) splitLeft splitRight
        RET_PURE_IR retPure ->
            RET_PURE_IR (pureIrVariableReplacement varReduction retPure)
        RET_IR ->
            comm
        ABORT_IR ->
            comm

pureIrVariableReplacement :: VariableIrReduction -> PureIr -> PureIr
pureIrVariableReplacement varReduction pure =
    case pure of
        PURE_BASE_IR base ->
            PURE_BASE_IR (pureBaseIrVariableReplacement varReduction base)
        PURE_BINOP_IR (PureBinopIr cat ty base1 base2) ->
            PURE_BINOP_IR (PureBinopIr cat ty (pureBaseIrVariableReplacement varReduction base1) (pureBaseIrVariableReplacement varReduction base2))
        PURE_UNOP_IR (PureUnopIr cat ty base) ->
            PURE_UNOP_IR (PureUnopIr cat ty (pureBaseIrVariableReplacement varReduction base))
        PURE_MEMOP_IR var memop ->
            PURE_MEMOP_IR var (memopIrVariableReplacement varReduction memop)

memopIrVariableReplacement :: VariableIrReduction -> MemopIr -> MemopIr
memopIrVariableReplacement varReduction memop = 
    case memopIrOffset memop of
        Just offsetPuB ->
            MemopIr (memopIrIsDeref memop) (Just (pureBaseIrVariableReplacement varReduction offsetPuB)) (memopIrRetType memop)
        Nothing ->
            memop

impureIrToVariableReplacement :: VariableIrReduction -> ImpureIr -> ImpureIr
impureIrToVariableReplacement varReduction impure =
    case impure of
        IMPURE_BINOP_IR (ImpureBinopIr cat ty base1 base2) ->
            IMPURE_BINOP_IR (ImpureBinopIr cat ty (pureBaseIrVariableReplacement varReduction base1) (pureBaseIrVariableReplacement varReduction base2))
        IMPURE_FNCALL_IR (ImpureFnCallIr name base ty) ->
            IMPURE_FNCALL_IR (ImpureFnCallIr name (map (pureBaseIrVariableReplacement varReduction) base) ty)

pureBaseIrVariableReplacement :: VariableIrReduction -> PureBaseIr -> PureBaseIr
pureBaseIrVariableReplacement varReduction base =
    case base of
        CONST_IR _ -> base
        VAR_IR baseVar -> VAR_IR (varIrVariableReplacement varReduction baseVar)

varIrVariableReplacement :: VariableIrReduction -> VariableIr -> VariableIr
varIrVariableReplacement varReduction var = 
    case Map.lookup var varReduction of
        Just reducedVar -> reducedVar
        Nothing -> var
