module Middleend.ElabToIr (
    irFunction,
)
where

import Model.Elaborated
import Common.Errors
import Model.Ir
import Model.Tokens
import Model.Types

import qualified Data.Map as Map
import qualified Data.Set as Set

-- IR TRANSLATION
-- + DECLARATION VERIFICATION
-- + TYPE VERIFICATION

data IrProcessingState = IrProcessingState
    {
      irProcStateCurrBb :: BasicBlockIr             -- current BasicBlock to add statement commands to (Nothing on return)
    , irProcStateFunctionIr :: FunctionIr           -- current Function
    , irProcStateErrors :: [VerificationError]      -- current errors
    , irProcStateBbCtr :: Int                       -- BasicBlock id counter
    , irProcScopeState :: IrProcessingScopeState    -- current scope state
    }

data PredecessorCommands = PredecessorCommands
    { predecessorCommandsGotoBlocks :: [Int]            -- list of predecessor BasicBlock indexes to inject GOTO commands
    , predecessorCommandsSplitBlocks :: [(Int, Int)]    -- list of predecessor BasicBlock indexes to inject SPLIT commands
    }

data IrProcessingScopeState = IrProcessingScopeState
    { scopes :: [Scope]                             -- current ordered list of scopes
    , regCtr :: Int                                 -- current register id counter
    , mapCtr :: Int                                 -- current scope id counter
    }

data Scope = Scope
    { scopeMap :: Map.Map String (VariableElab, Bool)   -- map of variables (w/ boolean for whether variable is a temp)
    , scopeId :: Int                                    -- scope id
    }

irFunction :: FunctionElab -> (FunctionIr, [VerificationError])
irFunction fnElab =
    let (initBb, initState) = addBb (addScope (State [] 0 0 0))
        initFnIr = FunctionIr Map.empty Map.empty Map.empty Set.empty
        (_, _, finalFnIr, errs) = irSeq fnElab (functionElabBlock fnElab) initState initBb initFnIr []
     in (finalFnIr, errs)

irStatement :: StatementElab -> FunctionElab -> IrProcessingState -> (Bool, PredecessorCommands, IrProcessingState)
irStatement stmtElab fnElab state = 
    case stmt of
        RET_ELAB retElab -> irRet retElab fnElab state
        DECL_ELAB declElab -> irDecl declElab state
        ASN_ELAB asnElab -> irAsn asnElab state
        EXP_ELAB expElab -> irExpStmt expElab state
        SEQ_ELAB seqElab -> irSeq seqElab fnElab state
        IF_ELAB ifElab ->
        WHILE_ELAB whileElab ->

-- STATEMENT ELAB->IR
irSeq :: SeqElab -> FunctionElab -> IrProcessingState -> (Bool, PredecessorCommands, IrProcessingState)
irSeq seq fnElab state = 
    let 
        -- insert inner scope for block
        innerScopeState = addScope (irProcScopeState state)
        innerState = (irProcessingStateUpdateScopeState innerScopeState state)

        -- apply each statement in block and
        -- i. check if statement terminates
        -- ii. apply predecessor command injection
        (finalTerm, finalState) =
            foldl
                (\(interTerm, interState) stmt ->
                    if interTerm
                        then (interTerm, interState)
                        else
                            let (stmtTerm, stmtPredComms, stmtState) = irStatement stmt fnElab interState
                            in if stmtTerm
                                then (True, stmtState)
                                else (False, (applyPredecessorCommands stmtState stmtPredComms))
                )
                (False, innerState)
                seq

        -- remove inner scope
        outerScopeState = popScopeMap (irProcScopeState finalState)
        outerState = (irProcessingStateUpdateScopeState outerScopeState finalState)
    in (finalTerm, predecessorCommandsEmpty, outerState)

irDecl :: DeclElab -> IrProcessingState -> (Bool, PredecessorCommands, IrProcessingState)
irDecl (DeclElab varElab Nothing) state =
    let name = extractIdentifierName (variableElabIdentifier varElab)
        scopeState = (irProcScopeState state)
    -- lookup the variable name in the current scope
     in case Map.lookup name (scopeMap (head (scopes scopeState))) of
            -- error if the variable already exists
            Just (prevVarElab, _) ->
                let err = (DOUBLE_DECL (DoubleDeclarationError (variableElabIdentifier prevVarElab) (variableElabIdentifier varElab)))
                    declState = (irProcessingStateAppendErrs [err] state)
                in (False, predecessorCommandsEmpty, declState)
            -- insert the variable into the top scope if it does not exist
            Nothing ->
                let var = varElabToIr varElab (scopeId (head (scopes state)))
                    comm = INIT_IR var
                    declBbIr = appendCommsToBb (irProcStateFunctionIr state) [comm]
                    declScopeState = irProcessingScopeStateInsertToTopScope scopeState name varElab
                    declState = 
                        ((irProcessingStateUpdateBB declBbIr) .
                        (irProcessingStateUpdateScopeState declScopeState))
                        state
                in (False, predecessorCommandsEmpty, declState)
irDecl (DeclElab varElab (Just asn)) state =
    let (declComms, declState, declErrs) = irDecl (DeclElab varElab Nothing) state
        (asnComms, asnState, asnErrs) = irAsn asn declState

        (_, _, declState) = irDecl (DeclElab varElab Nothing) state
        (_, _, asnState) = irAsn asn declState
    in (False, predecessorCommandsEmpty, asnState)    

irAsn :: AsnElab -> IrProcessingState -> (Bool, PredecessorCommands, IrProcessingState)
irAsn (AsnElab tok e) state =
    let (expComms, m_expPT, expState, expErrs) = irExp e state
        scopeState = (irProcScopeState state)
        m_varElab = identifierLookup tok (scopes scopeState)
     in case m_varElab of
            -- fail if varElab is not declared
            Nothing ->
                let errs = (USE_BEFORE_DECL (UseBeforeDeclarationError tok)) : expErrs
                    asnState = (irProcessingStateAppendErrs errs state)
                in (False, predecessorCommandsEmpty, asnState)
            -- check that varElab is declared
            Just (varElab, _, varScopeId) ->
                let varName = extractIdentifierName (variableElabIdentifier varElab)
                    asnErrState = setAssignedInScope state varName varElab varScopeId
                 in case m_expPT of
                        -- fail if exp is malformed
                        Nothing ->
                            let asnState = (irProcessingStateAppendErrs expErrs asnErrState)
                            in (False, predecessorCommandsEmpty, asnState)
                        Just (expPu, expTy) ->
                            -- check that var and exp have the same type
                            let varTy = (typeElabType (variableElabType varElab))
                                asnComm = ASN_PURE_IR (varElabToIr varElab varScopeId) expPu
                             in if varTy != expTy
                                    -- fail on type mismatch
                                    then
                                        let errs = (ASN_TYPE_MISMATCH (AsnTypeMismatch tok varTy expTy)) : expErrs
                                            asnState = (irProcessingStateAppendErrs errs asnErrState)
                                        in (False, predecessorCommandsEmpty, asnState)
                                    -- mark the var as assigned
                                    else
                                        let asnScopeState = setAssignedInScope expState varName varElab varScopeId
                                            asnBbIr = appendCommsToBb (irProcStateFunctionIr state) (asnComm : expComms)
                                            asnState = 
                                                ((irProcessingStateUpdateBB asnBbIr) .
                                                (irProcessingStateUpdateScopeState asnScopeState) .
                                                (irProcessingStateAppendErrs expErrs))
                                                asnErrState
                                         in (False, predecessorCommandsEmpty, asnState)            

irExpStmt :: ExpElab -> IrProcessingState -> (Bool, PredecessorCommands, IrProcessingState)
irExpStmt e state =
    let (expComms, _, expScopeState, expErrs) = irExp e (irProcScopeState state)
        expBbIr = appendCommsToBb (irProcStateFunctionIr state) expComms
        expState = 
            ((irProcessingStateUpdateBB expBbIr) .
            (irProcessingStateUpdateScopeState expScopeState) .
            (irProcessingStateAppendErrs expErrs))
            state
    in (False, predecessorCommandsEmpty, expState)

irRet :: RetElab -> FunctionElab -> IrProcessingState -> (Bool, PredecessorCommands, IrProcessingState)
irRet (RetElab e) (FunctionElab _ (TypeElab retTy _) _) state =
    let (expComms, m_expPT, expScopeState, expErrs) = irExp e (irProcScopeState state)
        (retComms, retErrs) = 
            case m_expPT of
                Just (expPu, expTy) ->
                    if (retTy == expTy)
                        then
                            let retComm = RET_PURE_IR expPu
                            in (retComm : expComms, expErrs)
                        else ([], (RET_TYPE_MISMATCH (RetTypeMismatch retTy expTy)) : expErrs)
                Nothing ->
                    ([], expErrs)
        retBbIr = appendCommsToBb (irProcStateCurrBb state) retComms
        retState = 
            ((irProcessingStateUpdateBB retBbIr) .
            (irProcessingStateAppendErrs retErrs) .
            (irProcessingStateUpdateScopeState expScopeState))
            expScopeState
    in (True, predecessorCommandsEmpty, retState)


-- EXP ELAB->IR

irExp :: ExpElab -> IrProcessingState -> ([CommandIr], Maybe (PureIr, TypeCategory), IrProcessingState, [VerificationError])
irExp e state =
    case e of
        CONST_ELAB c -> irConst c state
        IDENTIFIER_ELAB i -> irIdentifier i state
        BINOP_ELAB b -> irBinop b state
        UNOP_ELAB u -> irUnop u state

irConst :: Const -> IrProcessingState -> ([CommandIr], Maybe (PureIr, TypeCategory), IrProcessingState, [VerificationError])
irConst const state = ([], Just (PURE_BASE_IR (CONST_IR const), constToType const), state, [])

irIdentifier :: Token -> IrProcessingState -> ([CommandIr], Maybe (PureIr, TypeCategory), IrProcessingState, [VerificationError])
irIdentifier tok state =
    case identifierLookup tok (scopes state) of
        Just (varElab, varAssigned, varScopeId) ->
            if varAssigned
                then
                    let varIr = varElabToIr varElab varScopeId
                        varTypeCat = (typeElabType (variableElabType varElab))
                     in ([], Just (PURE_BASE_IR (VAR_IR varIr), varTypeCat), state, [])
                else ([], Nothing, state, [(USE_BEFORE_ASN (UseBeforeAssignmentError tok))])
        Nothing ->
            ([], Nothing, state, [(USE_BEFORE_DECL (UseBeforeDeclarationError tok))])

irBinop :: BinopElab -> IrProcessingState -> ([CommandIr], Maybe (PureIr, TypeCategory), IrProcessingState, [VerificationError])
irBinop (BinopElab cat op e1 e2) state =
    let (expComms1, m_expPT1, expState1, expErrs1) = irExp e1 state
        (expComms2, m_expPT2, expState2, expErrs2) = irExp e2 expState1
     in case (m_expPT1, m_expPT2) of
            -- type check when we have 2 valid exps
            (Just (expPu1, expTy1), Just (expPu2, expTy2)) ->
                let m_infType = binopTypeInf cat expTy1 expTy2
                 in case m_infType of
                        -- return pure exp with inferred type
                        Just infType ->
                            let (binopComms, binopPu, binopState) = binopOpTranslate cat infType expPu1 expPu2 expState2
                             in (binopComms ++ expComms2 ++ expComms1, Just (binopPu, infType), binopState, expErrs2 ++ expErrs1)
                        -- fail when type for binop cannot be inferred
                        Nothing ->
                            ([], Nothing, state, (OP_TYPE_MISMATCH (OpTypeMismatch op [expTy1, expTy2])) : (expErrs2 ++ expErrs1))
            -- immediately fail if either exp is invalid
            _ ->
                ([], Nothing, state, expErrs2 ++ expErrs1)

irUnop :: UnopElab -> IrProcessingState -> ([CommandIr], Maybe (PureIr, TypeCategory), IrProcessingState, [VerificationError])
irUnop (UnopElab cat op e) state =
    let (expComms, m_expPT, expState, expErrs) = irExp e state
     in case m_expPT of
            -- type check when the exp is valid
            Just (expPu, expTy) ->
                let m_infType = unopTypeInf cat expTy
                 in case m_infType of
                        -- return pure exp with inferred type
                        Just infType ->
                            let (unopComms, unopPu, unopState) = unopOpTranslate cat infType expPu expState
                             in (unopComms ++ expComms, Just (unopPu, infType), unopState, expErrs)
                        -- fail when type for unop cannot be inferred
                        Nothing ->
                            ([], Nothing, state, (OP_TYPE_MISMATCH (OpTypeMismatch op [expTy])) : expErrs)
            -- immediately fail if exp is invalid
            _ ->
                ([], Nothing, state, expErrs)

-- MANAGE AND ACCESS IR PROCESSING STATE

irProcessingStateUpdateBB :: BasicBlockIr -> IrProcessingState -> IrProcessingState
irProcessingStateUpdateBB bb state = 
    IrProcessingState
        bb
        (irProcStateFunctionIr state)
        (irProcStateErrors state)
        (irProcStateBbCtr state)
        (irProcScopeState state)

irProcessingStateUpdateFn :: FunctionIr -> IrProcessingState -> IrProcessingState
irProcessingStateUpdateBB bb state = 
    IrProcessingState
        (irProcStateCurrBb state)
        fnIr
        (irProcStateErrors state)
        (irProcStateBbCtr state)
        (irProcScopeState state)

irProcessingStateAppendErrs :: [VerificationError] -> IrProcessingState -> IrProcessingState
irProcessingStateAppendErrs errs state = 
    IrProcessingState
        (irProcStateCurrBb state)
        (irProcStateFunctionIr state)
        (errs:(irProcStateErrors state))
        (irProcStateBbCtr state)
        (irProcScopeState state)

irProcessingStateUpdateScopeState :: IrProcessingScopeState -> IrProcessingState -> IrProcessingState
irProcessingStateUpdateBB scopeState state = 
    IrProcessingState
        (irProcStateCurrBb state)
        (irProcStateFunctionIr state)
        (irProcStateErrors state)
        (irProcStateBbCtr state)
        scopeState

irProcessingStateAddBB :: IrProcessingState -> (BasicBlockIr, IrProcessingState)
irProcessingStateAddBB state = 
    let newBb = BasicBlockIr (bbCtr state) Map.empty []
        newIrProcState = 
            IrProcessingState
                (irProcStateCurrBb state)
                (irProcStateFunctionIr state)
                (irProcStateErrors state)
                ((irProcStateBbCtr state) + 1)
                (irProcScopeState state)
    in (newBb, newIrProcState)

-- UPDATE AND APPLY PROCESSOR COMMANDS

predecessorCommandsEmpty :: PredecessorCommands
predecessorCommandsEmpty = 
    PredecessorCommands [] [] 

predecessorCommandsAddGoto :: PredecessorCommands -> Int -> PredecessorCommands
predecessorCommandsAddGoto predComms succIndex = 
    PredecessorCommands 
        (succIndex:(predecessorCommandsGotoBlocks (irProcStatePredComms state)))
        (predecessorCommandsSplitBlocks (irProcStatePredComms state))
        
predecessorCommandsAddSplit :: PredecessorCommands -> Int -> Int -> PredecessorCommands
irProcessingStateAddSplit predComms succIndex splitPos = 
    PredecessorCommands 
        (predecessorCommandsGotoBlocks (irProcStatePredComms state))
        ((succIndex, splitPos):(predecessorCommandsSplitBlocks (irProcStatePredComms state)))

applyPredecessorCommands :: IrProcessingState -> PredecessorCommands -> IrProcessingState
applyPredecessorCommands state predComms = 
    let fnIrAddedGotos = 
            foldr
                (\(predBlockIndex) interFnIr ->
                    case Map.lookup predBlockIndex (functionIrBlocks interFnIr) of
                        Just bb -> 
                            let newBBIr = applyPredecessorGoto bb successorBbIndex
                                newFnIr = addBbsToFunction interFnIr [newBBIr]
                            in newFnIr
                        Nothing -> error (compilerError ("Attempted to insert successor after nonexistent predecessor BasicBlock GOTO: BasicBlockIr=" ++ (show predBlockIndex)))
                )
                (irProcStateFunctionIr state)
                (predecessorCommandsGotoBlocks predComms)
        fnIrAddedSplits = 
            foldr
                (\(predBlockIndex, succSplitPos) interFnIr ->
                    case Map.lookup predBlockIndex (functionIrBlocks interFnIr) of
                        Just bb -> 
                            let newBBIr = applyPredecessorSplit bb successorBbIndex succSplitPos
                                newFnIr = addBbsToFunction interFnIr [newBBIr]
                            in newFnIr
                        Nothing -> error (compilerError ("Attempted to insert successor after nonexistent predecessor BasicBlock SPLIT: BasicBlockIr=" ++ (show predBlockIndex)))
                )
                fnIrAddedGotos
                (predecessorCommandsSplitBlocks predComms)
    in irProcessingStateUpdateFn state fnIrAddedSplits
    
applyPredecessorGoto :: BasicBlockIr -> Int -> BasicBlockIr
applyPredecessorGoto predBlock succIndex = 
    case (head (bbIrCommands predBlock)) of
        GOTO_BB_IR _ -> 
            let newGoto = GOTO_BB_IR succIndex
                newCommands = newGoto:(tail (bbIrCommands predBlock))
                newBBIr = BasicBlockIr (bbIndex predBlock) (bbIrPhiFn predBlock) newCommands
            in newBBIr
        _ -> error (compilerError ("Attempted to insert successor after predecessor BasicBlock with no GOTO command: BasicBlockIr=" ++ (show predBlock)))

applyPredecessorSplit :: BasicBlockIr -> Int -> Int -> BasicBlockIr
applyPredecessorSplit predBlock succIndex splitPos = 
    case (head (bbIrCommands predBlock)) of
        SPLIT_BB_IR base left right ->
            let newSplit = 
                    case splitPos of
                        0 -> SPLIT_BB_IR base succIndex right
                        1 -> SPLIT_BB_IR base left succIndex
                        _ -> error (compilerError ("Attempted to insert successor after predecessor BasicBlock with invalid split index: BasicBlockIr=" ++ (show predBlock) ++ " Index=" ++ splitPos))
                newCommands = newSplit:(tail (bbIrCommands predBlock))
                newBBIr = BasicBlockIr (bbIndex predBlock) (bbIrPhiFn predBlock) newCommands
            in newBBIr
        _ -> error (compilerError ("Attempted to insert successor after predecessor BasicBlock with no SPLIT command: BasicBlockIr=" ++ (show predBlock)))

-- MANAGE AND ACCESS SCOPE STATE

irProcessingScopeStateAddTemp :: IrProcessingScopeState -> (String, IrProcessingScopeState)
irProcessingScopeStateAddTemp state =
    let tempName = (show (regCtr state)) ++ ":"
        newIrProcScopeState = 
            IrProcessingScopeState
                (scopes state) 
                ((regCtr state) + 1) 
                (mapCtr state)
    in (tempName, newIrProcScopeState)

irProcessingScopeStateAddScope :: IrProcessingScopeState -> IrProcessingScopeState
irProcessingScopeStateAddScope state =
    let newScope = Scope Map.empty (mapCtr state)
    in IrProcessingScopeState
            (newScope : (scopes state)) 
            (regCtr state) 
            ((mapCtr state) + 1)

irProcessingScopeStateInsertToTopScope :: IrProcessingScopeState -> String -> VariableElab -> IrProcessingScopeState
irProcessingScopeStateInsertToTopScope state name varElab =
    let topScope = head (scopes state)
        varScope = 
            Scope 
                (Map.insert name (varElab, False) (scopeMap topScope)) 
                (scopeId topScope)
    in IrProcessingScopeState
            (varScope : (tail (scopes state))) 
            (regCtr state) 
            (mapCtr state)

irProcessingScopeStateSetAssignedInScope :: IrProcessingScopeState -> String -> VariableElab -> Int -> IrProcessingScopeState
irProcessingScopeStateSetAssignedInScope state varName varElab varScopeId =
    let newScopes =
            map
                ( \scope ->
                    if (scopeId scope) == varScopeId
                        then
                            let newScopeMap = Map.insert varName (varElab, True) (scopeMap scope)
                             in Scope newScopeMap (scopeId scope)
                        else scope
                )
                (scopes state)
     in IrProcessingScopeState 
            newScopes 
            (regCtr state) 
            (mapCtr state)

irProcessingScopeStatePopScopeMap :: IrProcessingScopeState -> IrProcessingScopeState
irProcessingScopeStatePopScopeMap state = 
    IrProcessingScopeState 
        (tail (scopes state)) 
        (regCtr state) 
        (mapCtr state)

identifierLookup :: Token -> [Scope] -> Maybe (VariableElab, Bool, Int)
identifierLookup tok scopes =
    case scopes of
        [] -> Nothing
        scope : _ ->
            case (Map.lookup (extractIdentifierName tok) (scopeMap scope)) of
                Just (var, assigned) -> Just (var, assigned, (scopeId scope))
                Nothing -> identifierLookup tok (tail scopes)

varElabToIr :: VariableElab -> Int -> VariableIr
varElabToIr varElab varScopeId =
    VariableIr
        ((show varScopeId) ++ ":" ++ (extractIdentifierName (variableElabIdentifier varElab)))
        0
        (typeElabType (variableElabType varElab))
        False

-- OP TRANSLATION

binopTypeInf :: BinopCatElab -> TypeCategory -> TypeCategory -> Maybe TypeCategory
binopTypeInf cat t1 t2 =
    case cat of
        ADD_EXP_ELAB ->
            case (t1, t2) of
                (INT_TYPE, INT_TYPE) -> Just INT_TYPE
                _ -> Nothing
        SUB_EXP_ELAB ->
            case (t1, t2) of
                (INT_TYPE, INT_TYPE) -> Just INT_TYPE
                _ -> Nothing
        MUL_EXP_ELAB ->
            case (t1, t2) of
                (INT_TYPE, INT_TYPE) -> Just INT_TYPE
                _ -> Nothing
        DIV_EXP_ELAB ->
            case (t1, t2) of
                (INT_TYPE, INT_TYPE) -> Just INT_TYPE
                _ -> Nothing
        MOD_EXP_ELAB ->
            case (t1, t2) of
                (INT_TYPE, INT_TYPE) -> Just INT_TYPE
                _ -> Nothing
        AND_EXP_ELAB ->
            case (t1, t2) of
                (INT_TYPE, INT_TYPE) -> Just INT_TYPE
                _ -> Nothing
        XOR_EXP_ELAB ->
            case (t1, t2) of
                (INT_TYPE, INT_TYPE) -> Just INT_TYPE
                _ -> Nothing
        OR_EXP_ELAB ->
            case (t1, t2) of
                (INT_TYPE, INT_TYPE) -> Just INT_TYPE
                _ -> Nothing
        SLA_EXP_ELAB ->
            case (t1, t2) of
                (INT_TYPE, INT_TYPE) -> Just INT_TYPE
                _ -> Nothing
        SRA_EXP_ELAB ->
            case (t1, t2) of
                (INT_TYPE, INT_TYPE) -> Just INT_TYPE
                _ -> Nothing
        LT_EXP_ELAB ->
            case (t1, t2) of
                (INT_TYPE, INT_TYPE) -> Just BOOL_TYPE
                _ -> Nothing
        GT_EXP_ELAB ->
            case (t1, t2) of
                (INT_TYPE, INT_TYPE) -> Just BOOL_TYPE
                _ -> Nothing
        LTE_EXP_ELAB ->
            case (t1, t2) of
                (INT_TYPE, INT_TYPE) -> Just BOOL_TYPE
                _ -> Nothing
        GTE_EXP_ELAB ->
            case (t1, t2) of
                (INT_TYPE, INT_TYPE) -> Just BOOL_TYPE
                _ -> Nothing
        EQ_EXP_ELAB ->
            case (t1, t2) of
                (INT_TYPE, INT_TYPE) -> Just BOOL_TYPE
                _ -> Nothing
        NEQ_EXP_ELAB ->
            case (t1, t2) of
                (INT_TYPE, INT_TYPE) -> Just BOOL_TYPE
                _ -> Nothing
        LOGAND_EXP_ELAB ->
            case (t1, t2) of
                (BOOL_TYPE, BOOL_TYPE) -> Just BOOL_TYPE
                _ -> Nothing
        LOGOR_EXP_ELAB ->
            case (t1, t2) of
                (BOOL_TYPE, BOOL_TYPE) -> Just BOOL_TYPE
                _ -> Nothing

binopOpTranslate :: BinopCatElab -> TypeCategory -> PureIr -> PureIr -> IrProcessingScopeState -> ([CommandIr], PureIr, IrProcessingScopeState)
binopOpTranslate cat ty p1 p2 state =
    let (expandComms1, expandPureBase1, expandState1) = expandPureIr p1 state
        (expandComms2, expandPureBase2, expandState2) = expandPureIr p2 expandState1
     in case cat of
            ADD_EXP_ELAB -> 
                (expandComms2 ++ expandComms1, PURE_BINOP_IR (PureBinopIr ADD_IR ty expandPureBase1 expandPureBase2), expandState2)
            SUB_EXP_ELAB -> 
                (expandComms2 ++ expandComms1, PURE_BINOP_IR (PureBinopIr SUB_IR ty expandPureBase1 expandPureBase2), expandState2)
            MUL_EXP_ELAB -> 
                (expandComms2 ++ expandComms1, PURE_BINOP_IR (PureBinopIr MUL_IR ty expandPureBase1 expandPureBase2), expandState2)
            DIV_EXP_ELAB ->
                let (tempName, newState) = addTemp state
                    temp = VariableIr tempName 0 ty True
                    binop = IMPURE_BINOP_IR (ImpureBinopIr DIV_IR ty expandPureBase1 expandPureBase2)
                    comm = ASN_IMPURE_IR temp binop
                 in (comm : (expandComms2 ++ expandComms1), PURE_BASE_IR (VAR_IR temp), newState)
            MOD_EXP_ELAB ->
                let (tempName, newState) = addTemp state
                    temp = VariableIr tempName 0 ty True
                    binop = IMPURE_BINOP_IR (ImpureBinopIr MOD_IR ty expandPureBase1 expandPureBase2)
                    comm = ASN_IMPURE_IR temp binop
                 in (comm : (expandComms2 ++ expandComms1), PURE_BASE_IR (VAR_IR temp), newState)
            AND_EXP_ELAB ->
                (expandComms2 ++ expandComms1, PURE_BINOP_IR (PureBinopIr AND_IR ty expandPureBase1 expandPureBase2), expandState2)
            XOR_EXP_ELAB ->
                (expandComms2 ++ expandComms1, PURE_BINOP_IR (PureBinopIr XOR_IR ty expandPureBase1 expandPureBase2), expandState2)
            OR_EXP_ELAB ->
                (expandComms2 ++ expandComms1, PURE_BINOP_IR (PureBinopIr OR_IR ty expandPureBase1 expandPureBase2), expandState2)
            SLA_EXP_ELAB ->
                (expandComms2 ++ expandComms1, PURE_BINOP_IR (PureBinopIr SLA_IR ty expandPureBase1 expandPureBase2), expandState2)
            SRA_EXP_ELAB ->
                (expandComms2 ++ expandComms1, PURE_BINOP_IR (PureBinopIr SRA_IR ty expandPureBase1 expandPureBase2), expandState2)
            LT_EXP_ELAB ->
                (expandComms2 ++ expandComms1, PURE_BINOP_IR (PureBinopIr LT_IR ty expandPureBase1 expandPureBase2), expandState2)
            GT_EXP_ELAB ->
                (expandComms2 ++ expandComms1, PURE_BINOP_IR (PureBinopIr GT_IR ty expandPureBase1 expandPureBase2), expandState2)
            LTE_EXP_ELAB ->
                (expandComms2 ++ expandComms1, PURE_BINOP_IR (PureBinopIr LTE_IR ty expandPureBase1 expandPureBase2), expandState2)
            GTE_EXP_ELAB ->
                (expandComms2 ++ expandComms1, PURE_BINOP_IR (PureBinopIr GTE_IR ty expandPureBase1 expandPureBase2), expandState2)
            EQ_EXP_ELAB ->
                (expandComms2 ++ expandComms1, PURE_BINOP_IR (PureBinopIr EQ_IR ty expandPureBase1 expandPureBase2), expandState2)
            NEQ_EXP_ELAB ->
                (expandComms2 ++ expandComms1, PURE_BINOP_IR (PureBinopIr NEQ_IR ty expandPureBase1 expandPureBase2), expandState2)
            -- TODO: add short-circuiting for logical and/or
            LOGAND_EXP_ELAB ->
                (expandComms2 ++ expandComms1, PURE_BINOP_IR (PureBinopIr LOGAND_IR ty expandPureBase1 expandPureBase2), expandState2)
            LOGOR_EXP_ELAB ->
                (expandComms2 ++ expandComms1, PURE_BINOP_IR (PureBinopIr LOGOR_IR ty expandPureBase1 expandPureBase2), expandState2)            

unopTypeInf :: UnopCatElab -> TypeCategory -> Maybe TypeCategory
unopTypeInf cat t1 =
    case cat of
        NEG_EXP_ELAB ->
            case t1 of
                INT_TYPE -> Just INT_TYPE
                _ -> Nothing
        NOT_EXP_ELAB ->
            case t1 of
                INT_TYPE -> Just INT_TYPE
                _ -> Nothing
        LOGNOT_EXP_ELAB ->
            case t1 of
                BOOL_TYPE -> Just BOOL_TYPE
                _ -> Nothing

unopOpTranslate :: UnopCatElab -> TypeCategory -> PureIr -> IrProcessingScopeState -> ([CommandIr], PureIr, IrProcessingScopeState)
unopOpTranslate cat ty p1 state =
    let (expandComms, expandPureBase, expandState) = expandPureIr p1 state
     in case cat of
            NEG_EXP_ELAB -> 
                (expandComms, PURE_UNOP_IR (PureUnopIr NEG_IR ty expandPureBase), expandState)
            NOT_EXP_ELAB ->
                (expandComms, PURE_UNOP_IR (PureUnopIr NOT_IR ty expandPureBase), expandState)
            LOGNOT_EXP_ELAB ->
                (expandComms, PURE_UNOP_IR (PureUnopIr LOGNOT_IR ty expandPureBase), expandState)

-- applies an extra level of processing to convert potentially recursive PureIr into
-- [CommandIr] + Const/VariableIr
expandPureIr :: PureIr -> IrProcessingScopeState -> ([CommandIr], PureBaseIr, IrProcessingScopeState)
expandPureIr pu state =
    case pu of
        PURE_BASE_IR base -> ([], base, state)
        PURE_BINOP_IR binop ->
            let (tempName, newState) = addTemp state
                binopTemp = VariableIr tempName 0 (pureBinopInfType binop) True
                binopComm = ASN_PURE_IR binopTemp pu
             in ([binopComm], VAR_IR binopTemp, newState)
        PURE_UNOP_IR unop ->
            let (tempName, newState) = addTemp state
                unopTemp = VariableIr tempName 0 (pureUnopInfType unop) True
                unopComm = ASN_PURE_IR unopTemp pu
             in ([unopComm], VAR_IR unopTemp, newState)
