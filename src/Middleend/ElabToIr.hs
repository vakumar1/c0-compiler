module Middleend.ElabToIr (
    irProg,
)
where

import Model.Elaborated
import Common.Errors
import Common.Graphs
import Model.Ir
import Model.Tokens
import Model.Types
import Common.Constants

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import qualified Debug.Trace as Trace
import qualified Text.Show.Pretty as Pretty

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
    deriving Show

data PredecessorCommands = PredecessorCommands
    { predecessorCommandsGotoBlocks :: [Int]            -- list of predecessor BasicBlock indexes to inject GOTO commands
    , predecessorCommandsSplitBlocks :: [(Int, Int)]    -- list of predecessor BasicBlock indexes to inject SPLIT commands
    }
    deriving Show

data IrProcessingScopeState = IrProcessingScopeState
    { scopes :: [Scope]                             -- current ordered list of scopes
    , globalFnScope :: GlobalFnScope                -- current fn signatures in global scope
    , regCtr :: Int                                 -- current register id counter
    , mapCtr :: Int                                 -- current scope id counter
    }
    deriving Show

data Scope = Scope
    { scopeMap :: Map.Map String (VariableElab, Bool)   -- map of variables (w/ boolean for whether variable is a temp)
    , scopeId :: Int                                    -- scope id
    }
    deriving Show

data GlobalFnScope = GlobalFnScope
    { globalFnScopeProgramIdentifier :: String
    , globalFnScopeSignatures :: Map.Map String FunctionSignatureElab
    , globalFnScopeDefined :: Set.Set String
    , globalFnScopeCounter :: Int
    }
    deriving Show

irProg :: String -> ProgramElab -> (ProgramIr, [VerificationError])
irProg progIdentifier progElab = 
    let initGlobalFnScope = 
            GlobalFnScope
                progIdentifier
                Map.empty
                Set.empty
                0
        (finalGlobalFnScope, fnIrs, errs) = 
            foldl
                (\(interFnScope, interFnIrs, interErrs) gdeclElab ->
                    case gdeclElab of
                        FNDECL_GDECL_ELAB fndeclElab ->
                            let (newGlobalFnScope, fndeclErrs) = irFunctionDecl interFnScope fndeclElab
                            in (newGlobalFnScope, interFnIrs, interErrs ++ fndeclErrs)
                        FNDEFN_GDECL_ELAB fndefnElab ->
                            let (newGlobalFnScope, fnIr, fndefnErrs) = irFunction interFnScope fndefnElab
                            in (newGlobalFnScope, interFnIrs ++ [fnIr], interErrs ++ fndefnErrs)
                )
                (initGlobalFnScope, [], [])
                progElab
    in (fnIrs, errs)

irFunctionDecl :: GlobalFnScope -> FunctionSignatureElab -> (GlobalFnScope, [VerificationError])
irFunctionDecl fnScope fnSignElab = 
    let fnName = functionSignatureElabName fnSignElab
        newFnScope = 
            GlobalFnScope
                (globalFnScopeProgramIdentifier fnScope)
                (Map.insert (extractIdentifierName fnName) fnSignElab (globalFnScopeSignatures fnScope))
                (globalFnScopeDefined fnScope)
                (globalFnScopeCounter fnScope)
        conflictErrs = 
            case findConflictFnDecl fnScope fnSignElab of
                Just currFnSignElab -> 
                    [
                        CONFLICTING_FN_DECL
                            (ConflictingFnDeclError
                                (functionSignatureElabName currFnSignElab)
                                ((map (typeElabType . variableElabType)) (functionSignatureElabArgs currFnSignElab))
                                (typeElabType . functionSignatureElabRetType $ currFnSignElab)
                                (functionSignatureElabName fnSignElab)
                                ((map (typeElabType . variableElabType)) (functionSignatureElabArgs fnSignElab))
                                (typeElabType . functionSignatureElabRetType $ fnSignElab))
                    ]
                Nothing -> []
    in (newFnScope, conflictErrs)

irFunction :: GlobalFnScope -> FunctionElab -> (GlobalFnScope, FunctionIr, [VerificationError])
irFunction fnScope fnElab =
    let 
        -- update global fn scope and translate fnElab
        fnName = extractIdentifierName . functionSignatureElabName . functionElabSignature $ fnElab
        fnIndex = globalFnScopeCounter fnScope
        newFnScope =
            GlobalFnScope
                (globalFnScopeProgramIdentifier fnScope)
                (Map.insert fnName (functionElabSignature fnElab) (globalFnScopeSignatures fnScope))
                (Set.insert fnName (globalFnScopeDefined fnScope))
                ((globalFnScopeCounter fnScope) + 1)
        initBbIr = BasicBlockIr fnName 0 Map.empty []
        (fnIrArgVars, initScopeState, initDeclErrs) = 
            foldl
                (\(interArgVars, interScopeState, interDeclErrs) varElab ->
                    let varName = extractIdentifierName . variableElabIdentifier $ varElab
                        varIr = varElabToIr varElab 0
                    in
                        case Map.lookup varName (scopeMap . head . scopes $ interScopeState) of
                            Just (prevVarElab, _) ->
                                (interArgVars ++ [varIr], interScopeState, interDeclErrs ++ [DOUBLE_DECL (DoubleDeclarationError (variableElabIdentifier prevVarElab) (variableElabIdentifier varElab))])
                            Nothing ->
                                (interArgVars ++ [varIr], irProcessingScopeStateInsertToTopScope interScopeState varName varElab True, interDeclErrs)
                )
                ([], irProcessingScopeStateAddScope (IrProcessingScopeState [] newFnScope 0 0), [])
                (functionSignatureElabArgs . functionElabSignature $ fnElab)
        initFnIr = FunctionIr fnName fnIrArgVars Map.empty emptyGraph
        initState = IrProcessingState initBbIr initFnIr [] 1 initScopeState
        (finalTerm, _, _, finalState) = irSeq (functionElabBlock fnElab) fnElab initState
        fnIr = irProcStateFunctionIr finalState
        
        -- compile errors
        translationErrs = irProcStateErrors finalState
        retErrs = 
            if not finalTerm
                then
                    [
                        INVALID_RET 
                            (InvalidReturnError 
                                (functionSignatureElabName . functionElabSignature $ fnElab))
                    ]
                else []
        conflictErrs = 
            case findConflictFnDecl fnScope (functionElabSignature fnElab) of
                Just currFnSignElab -> 
                    [
                        CONFLICTING_FN_DECL
                            (ConflictingFnDeclError
                                (functionSignatureElabName currFnSignElab)
                                ((map (typeElabType . variableElabType)) (functionSignatureElabArgs currFnSignElab))
                                (typeElabType . functionSignatureElabRetType $ currFnSignElab)
                                (functionSignatureElabName . functionElabSignature $ fnElab)
                                ((map (typeElabType . variableElabType)) (functionSignatureElabArgs . functionElabSignature $ fnElab))
                                (typeElabType . functionSignatureElabRetType . functionElabSignature $ fnElab))
                    ]
                Nothing -> []
        duplicateErrs = 
            case findDuplicateFnDefn fnScope (functionElabSignature fnElab) of
                Just currFnSignElab ->
                    [
                        DUPLICATE_FN_DEFN 
                            (DuplicateFnDefnError 
                                (functionSignatureElabName currFnSignElab) 
                                (functionSignatureElabName . functionElabSignature $ fnElab))
                    ]
                Nothing -> []
    in (newFnScope, fnIr, initDeclErrs ++ translationErrs ++ retErrs ++ conflictErrs ++ duplicateErrs)

-- each statement processor returns
--   Bool: whether the statement definitely terminates the function (within its parent statement context)
--   Bool: whether to create and start a new basic block for the following statement
--   PredecessorCommands: the list of injections to be inserted (if needed) after this statement
--   IrProcessingState: the new processing state
irStatement :: StatementElab -> FunctionElab -> (Bool, PredecessorCommands, IrProcessingState) -> (Bool, Bool, PredecessorCommands, IrProcessingState)
irStatement stmtElab fnElab (startBb, preds, state) 
    | debugLogs && (Trace.trace 
        ("\n\nirStatement -- " ++ 
            "\nstmtElab=" ++    (Pretty.ppShow stmtElab) ++ 
            "\nstartBB=" ++     (Pretty.ppShow startBb) ++ 
            "\nnextBBIndex=" ++ (Pretty.ppShow . irProcStateBbCtr $ state) ++
            "\npreds=" ++       (Pretty.ppShow preds) ++ 
            "\ncurrBB=" ++      (Pretty.ppShow . irProcStateCurrBb $ state) ++ 
            "\nfnIr=" ++        (Pretty.ppShow . irProcStateFunctionIr $ state)
        ) 
        False) = undefined
irStatement stmtElab fnElab (startBb, preds, state) = 
    let stmtState = 
            if not startBb
                then state
                else
                    -- commit current basic block + create new basic block + apply predecessor injection
                    let termState = irProcessingStateCommitBB state
                        (newBb, _state) = irProcessingStateAddBB termState
                        startState = irProcessingStateUpdateBB newBb _state
                        injectState = applyPredecessorCommands preds startState
                    in injectState
    in case stmtElab of
            RET_ELAB retElab -> irRet retElab fnElab stmtState
            DECL_ELAB declElab -> irDecl declElab stmtState
            ASN_ELAB asnElab -> irAsn asnElab stmtState
            EXP_ELAB expElab -> irExpStmt expElab stmtState
            SEQ_ELAB seqElab -> irSeq seqElab fnElab stmtState
            IF_ELAB ifElab -> irIf ifElab fnElab stmtState
            WHILE_ELAB whileElab -> irWhile whileElab fnElab stmtState

-- STATEMENT ELAB->IR
irSeq :: SeqElab -> FunctionElab -> IrProcessingState -> (Bool, Bool, PredecessorCommands, IrProcessingState)
irSeq seq fnElab state = 
    let 
        -- insert inner scope for block
        innerScopeState = irProcessingScopeStateAddScope (irProcScopeState state)
        innerState = (irProcessingStateUpdateScopeState innerScopeState state)

        -- apply each statement in block
        (finalTerm, finalStartBb, finalPreds, finalState) =
            if (null seq)
                then
                    (False, False, predecessorCommandsSingleton innerState, innerState)
                else
                    foldl
                        (\(interTerm, interStartBb, interPreds, interState) stmt ->
                            if interTerm
                                -- short-circuit once fn has already been terminated
                                then (interTerm, interStartBb, interPreds, interState)
                                else irStatement stmt fnElab (interStartBb, interPreds, interState)
                        )
                        (False, False, predecessorCommandEmpty, innerState)
                        seq

        -- remove inner scope
        outerScopeState = irProcessingScopeStatePopScopeMap (irProcScopeState finalState)
        outerState = (irProcessingStateUpdateScopeState outerScopeState finalState)
    in (finalTerm, finalStartBb, finalPreds, outerState)

irIf :: IfElab -> FunctionElab -> IrProcessingState -> (Bool, Bool, PredecessorCommands, IrProcessingState)
irIf (IfElab condExp ifStmt Nothing) fnElab state = 
    let 
        -- evaluate if cond exp and add split comm (with both indices unfilled) to curr basic block
        (m_condPu, condState) = irCond condExp state
        splitComm = 
            case m_condPu of
                Just condPu -> SPLIT_BB_IR condPu 0 0
                Nothing -> SPLIT_BB_IR dummyPureIr 0 0
        termState = irProcessingStateAddComms [splitComm] condState
        termBBIndex = bbIndex . irProcStateCurrBb $ termState

        -- evaluate inner stmt + commit curr BB and return preds w/ split command for term block
        termInnerPreds = predecessorCommandsAddSplit predecessorCommandEmpty termBBIndex 0
        (innerTerm, innerStartBb, innerPreds, innerState) = irStatement ifStmt fnElab (True, termInnerPreds, termState)
        innerCommitState = irProcessingStateCommitBB innerState
        outerPreds = predecessorCommandsAddSplit innerPreds termBBIndex 1

        resetFinalTermBBIr = 
            case Map.lookup termBBIndex (functionIrBlocks . irProcStateFunctionIr $ innerCommitState) of
                Just bb -> bb
                Nothing -> error . compilerError $ "Attempted to retrieve updated cond BB but was never committed by if stmt: bbIndex=" ++ (show termBBIndex)
        resetFinalTermState = irProcessingStateUpdateBB resetFinalTermBBIr innerCommitState
    in (False, True, outerPreds, resetFinalTermState)

irIf (IfElab condExp ifStmt (Just elseStmt)) fnElab state =
    let 
        -- evaluate if cond exp and add split comm (with both indices unfilled) to curr basic block
        (m_condPu, condState) = irCond condExp state
        splitComm = 
            case m_condPu of
                Just condPu -> SPLIT_BB_IR condPu 0 0
                Nothing -> SPLIT_BB_IR dummyPureIr 0 0
        termState = irProcessingStateAddComms [splitComm] condState
        termBBIndex = bbIndex . irProcStateCurrBb $ termState

        -- evaluate if stmt + commit curr BB
        termIfPreds = predecessorCommandsAddSplit predecessorCommandEmpty termBBIndex 0
        (ifTerm, ifStartBb, ifPreds, ifState) = irStatement ifStmt fnElab (True, termIfPreds, termState)
        ifCommitState = irProcessingStateCommitBB ifState

        -- evaluate else stmt (starting from cond BB) + commit curr BB
        resetElseTermBBIr = 
            case Map.lookup termBBIndex (functionIrBlocks . irProcStateFunctionIr $ ifCommitState) of
                Just bb -> bb
                Nothing -> error . compilerError $ "Attempted to retrieve updated cond BB but was never committed by if stmt: bbIndex=" ++ (show termBBIndex)
        resetElseTermState = irProcessingStateUpdateBB resetElseTermBBIr ifCommitState
        termElsePreds = predecessorCommandsAddSplit predecessorCommandEmpty termBBIndex 1
        (elseTerm, elseStartBb, elsePreds, elseState) = irStatement elseStmt fnElab (True, termElsePreds, resetElseTermState)
        elseCommitState = irProcessingStateCommitBB elseState

        resetFinalTermBBIr = 
            case Map.lookup termBBIndex (functionIrBlocks . irProcStateFunctionIr $ elseCommitState) of
                Just bb -> bb
                Nothing -> error . compilerError $ "Attempted to retrieve updated cond BB but was never committed by if stmt: bbIndex=" ++ (show termBBIndex)
        resetFinalTermState = irProcessingStateUpdateBB resetFinalTermBBIr elseCommitState
    in (ifTerm && elseTerm, True, predecessorCommandsMerge ifPreds elsePreds, resetFinalTermState)

irWhile :: WhileElab -> FunctionElab -> IrProcessingState -> (Bool, Bool, PredecessorCommands, IrProcessingState)
irWhile (WhileElab condExp whileStmt) fnElab state = 
    let 
        -- PART I. cond block preparation

        -- terminate the current basic block and start the cond blocks
        -- i. create the cond start basic block
        -- ii. add edge to function CFG + terminate the current basic block w/ a direct GOTO to cond basic block
        -- iii. commit current basic block + set current basic block to cond start block
        (condStartBbIr, initState) = irProcessingStateAddBB state
        condStartBBIndex = bbIndex condStartBbIr
        condStartState = 
            (irProcessingStateUpdateBB condStartBbIr) .
            irProcessingStateCommitBB .
            (irProcessingStateAddComms [GOTO_BB_IR condStartBBIndex]) .
            (irProcessingStateUpdateFn (addEdgeToCFG (bbIndex . irProcStateCurrBb $ initState) condStartBBIndex (irProcStateFunctionIr initState))) $
            initState

        -- process the cond exp
        -- i. process the cond exp + append a SPLIT comm with indices left unfilled
        -- ii. commit the cond term basic block
        (m_condPu, condProcessedState) = irCond condExp condStartState
        splitComm = 
            case m_condPu of
                Just condPu -> SPLIT_BB_IR condPu 0 0
                Nothing -> SPLIT_BB_IR dummyPureIr 0 0
        condTermState = 
            irProcessingStateCommitBB .
            (irProcessingStateAddComms [splitComm]) $
            condProcessedState
        condTermBBIndex = bbIndex . irProcStateCurrBb $ condTermState

        -- PART II. stmt block evaluation + manual injection
        
        -- evaluate stmt basic block (with cond->stmt SPLIT injection)
        condToStmtPreds = predecessorCommandsAddSplit predecessorCommandEmpty condTermBBIndex 0
        (_, _, stmtToCondPreds, stmtState) = irStatement whileStmt fnElab (True, condToStmtPreds, condTermState)

        -- apply stmt->cond injection:
        -- i. commit the current basic block (whatever it is)
        -- ii. set the current basic block to be the updated cond start block
        -- iii. manually apply stmt->cond injection
        -- iv. commit the cond start block
        updatedCondStartBBIr = 
            case Map.lookup condStartBBIndex (functionIrBlocks . irProcStateFunctionIr $ stmtState) of
                Just bb -> bb
                Nothing -> error . compilerError $ "Attempted to retrieve updated cond start BB but was never committed by while stmt: bbIndex=" ++ (show condStartBBIndex)
        injectedCondStartState = 
            irProcessingStateCommitBB .
            (applyPredecessorCommands stmtToCondPreds) .
            (irProcessingStateUpdateBB updatedCondStartBBIr) .
            irProcessingStateCommitBB $
            stmtState

        -- PART III. set the current basic block to be the updated cond term block + pass params to upcoming basic block
        updatedCondTermBBIr = 
            case Map.lookup condTermBBIndex (functionIrBlocks . irProcStateFunctionIr $ injectedCondStartState) of
                Just bb -> bb
                Nothing -> error . compilerError $ "Attempted to retrieve updated cond term BB but was never committed by while stmt: bbIndex=" ++ (show condTermBBIndex)
        condReTermState = irProcessingStateUpdateBB updatedCondTermBBIr injectedCondStartState
        condToNextPreds = predecessorCommandsAddSplit predecessorCommandEmpty condTermBBIndex 1
    in (False, True, condToNextPreds, condReTermState)

irCond :: ExpElab -> IrProcessingState -> (Maybe PureIr, IrProcessingState)
irCond e state = 
    let 
        -- evaluate if exp and create temp
        (m_expPT, expState) = irExp e state
    in case m_expPT of
        -- fail if exp malformed
        Nothing -> 
            (Nothing, expState)
        Just (expPu, expTy) ->
            if (BOOL_TYPE /= expTy)
                -- fail on non-bool exp type mismatch
                then 
                    (Just expPu, irProcessingStateAppendErrs [IF_COND_TYPE_MISMATCH (IfCondTypeMismatch expTy)] expState)
                -- success - add asn command
                else
                    (Just expPu, expState)

irDecl :: DeclElab -> IrProcessingState -> (Bool, Bool, PredecessorCommands, IrProcessingState)
irDecl (DeclElab varElab Nothing) state =
    let name = extractIdentifierName (variableElabIdentifier varElab)
        scopeState = (irProcScopeState state)
    -- lookup the variable name in the current scope
     in case Map.lookup name (scopeMap (head (scopes scopeState))) of
            -- error if the variable already exists
            Just (prevVarElab, _) ->
                let err = (DOUBLE_DECL (DoubleDeclarationError (variableElabIdentifier prevVarElab) (variableElabIdentifier varElab)))
                    declState = (irProcessingStateAppendErrs [err] state)
                in (False, False, predecessorCommandsSingleton declState, declState)
            -- insert the variable into the top scope if it does not exist
            Nothing ->
                let var = varElabToIr varElab (scopeId (head (scopes scopeState)))
                    comm = INIT_IR var
                    declBbIr = appendCommsToBb (irProcStateCurrBb state) [comm]
                    declScopeState = irProcessingScopeStateInsertToTopScope scopeState name varElab False
                    declState = 
                        ((irProcessingStateUpdateBB declBbIr) .
                        (irProcessingStateUpdateScopeState declScopeState))
                        state
                in (False, False, predecessorCommandsSingleton declState, declState)

irDecl (DeclElab varElab (Just asn)) state =
    let (_, _, _, declState) = irDecl (DeclElab varElab Nothing) state
        (_, _, _, asnState) = irAsn asn declState
    in (False, False, predecessorCommandsSingleton declState, asnState)    

irAsn :: AsnElab -> IrProcessingState -> (Bool, Bool, PredecessorCommands, IrProcessingState)
irAsn (AsnElab tok e) state =
    let (m_expPT, expState) = irExp e state
        m_varElab = identifierLookup tok (scopes . irProcScopeState $ expState)
     in case m_varElab of
            -- fail if varElab is not declared
            Nothing ->
                let asnState = irProcessingStateAppendErrs [USE_BEFORE_DECL (UseBeforeDeclarationError tok)] expState
                in (False, False, predecessorCommandsSingleton asnState, asnState)
            -- check that varElab is declared
            Just (varElab, _, varScopeId) ->
                let varName = extractIdentifierName (variableElabIdentifier varElab)
                    setScopeState = irProcessingScopeStateSetAssignedInScope (irProcScopeState expState) varName varElab varScopeId
                 in case m_expPT of
                        -- fail if exp is malformed
                        Nothing ->
                            let asnState = irProcessingStateUpdateScopeState setScopeState expState
                            in (False, False, predecessorCommandsSingleton asnState, asnState)
                        Just (expPu, expTy) ->
                            -- check that var and exp have the same type
                            let varTy = (typeElabType (variableElabType varElab))
                                asnComm = ASN_PURE_IR (varElabToIr varElab varScopeId) expPu
                             in if varTy /= expTy
                                    -- fail on type mismatch
                                    then
                                        let asnState = 
                                                (irProcessingStateAppendErrs [ASN_TYPE_MISMATCH (AsnTypeMismatch tok varTy expTy)]) .
                                                (irProcessingStateUpdateScopeState setScopeState) $
                                                expState
                                        in (False, False, predecessorCommandsSingleton asnState, asnState)
                                    -- success - add asn statement to basic block
                                    else
                                        let asnState = 
                                                (irProcessingStateAddComms [asnComm]) .
                                                (irProcessingStateUpdateScopeState setScopeState) $
                                                expState
                                         in (False, False, predecessorCommandsSingleton asnState, asnState)

irExpStmt :: ExpElab -> IrProcessingState -> (Bool, Bool, PredecessorCommands, IrProcessingState)
irExpStmt e state =
    let (_, expState) = irExp e state
    in (False, False, predecessorCommandsSingleton expState, expState)

irRet :: RetElab -> FunctionElab -> IrProcessingState -> (Bool, Bool, PredecessorCommands, IrProcessingState)
irRet (RetElab (Just e)) (FunctionElab (FunctionSignatureElab _ _ (TypeElab retTy _)) _) state =
    let (m_expPT, expState) = irExp e state
    in case m_expPT of
            -- fail if exp is malformed
            Nothing ->
                (True, False, predecessorCommandEmpty, expState)
            Just (expPu, expTy) ->
                if (retTy /= expTy)
                    -- fail on ret/exp type mismatch
                    then 
                        let retState = irProcessingStateAppendErrs [RET_TYPE_MISMATCH (RetTypeMismatch retTy expTy)] expState
                        in (True, False, predecessorCommandEmpty, retState)
                    -- success - add ret statement to basic block and commit
                    else
                        let retState = 
                                irProcessingStateCommitBB .
                                irProcessingStateAddComms [RET_PURE_IR expPu] $
                                expState
                        in (True, False, predecessorCommandEmpty, retState)
irRet (RetElab Nothing) (FunctionElab (FunctionSignatureElab _ _ (TypeElab retTy _)) _) state =
    if retTy /= VOID_TYPE
        -- fail on ret/exp type mismatch
        then
            let retState = irProcessingStateAppendErrs [RET_TYPE_MISMATCH (RetTypeMismatch retTy VOID_TYPE)] state
            in (True, False, predecessorCommandEmpty, retState)
        -- success - add ret statement to basic block and commit
        else
            let retState = 
                    irProcessingStateCommitBB .
                    irProcessingStateAddComms [RET_IR] $
                    state
            in (True, False, predecessorCommandEmpty, retState)

-- EXP ELAB->IR

irExp :: ExpElab -> IrProcessingState -> (Maybe (PureIr, TypeCategory), IrProcessingState)
irExp e state =
    case e of
        CONST_ELAB c -> irConst c state
        IDENTIFIER_ELAB i -> irIdentifier i state
        BINOP_ELAB b -> irBinop b state
        LOG_BINOP_ELAB lb -> irLogBinop lb state
        TERN_ELAB t -> irTernop t state
        UNOP_ELAB u -> irUnop u state
        FN_CALL_ELAB f -> irFunctionCall f state

irConst :: Const -> IrProcessingState -> (Maybe (PureIr, TypeCategory), IrProcessingState)
irConst const state = (Just (PURE_BASE_IR (CONST_IR const), constToType const), state)

irIdentifier :: Token -> IrProcessingState -> (Maybe (PureIr, TypeCategory), IrProcessingState)
irIdentifier tok state =
    let (m_expPT, errs) = 
            case identifierLookup tok (scopes . irProcScopeState $ state) of
                Just (varElab, varAssigned, varScopeId) ->
                    if varAssigned
                        then
                            let varIr = varElabToIr varElab varScopeId
                                varTypeCat = (typeElabType (variableElabType varElab))
                            in (Just (PURE_BASE_IR (VAR_IR varIr), varTypeCat), [])
                        else (Nothing, [USE_BEFORE_ASN (UseBeforeAssignmentError tok)])
                Nothing ->
                    (Nothing, [USE_BEFORE_DECL (UseBeforeDeclarationError tok)])
    in (m_expPT, irProcessingStateAppendErrs errs state)

irTernop :: TernopElab -> IrProcessingState -> (Maybe (PureIr, TypeCategory), IrProcessingState)
irTernop (TernopElab op eCond e1 e2) state = 
    let
        -- allocate cond temp (tCond) and asn temp (tAsn) + process eCond + add asn comm tCond = eCond
        (tCondName, _scopeState) = irProcessingScopeStateAddTemp . irProcScopeState $ state
        (tAsnName, newScopeState) = irProcessingScopeStateAddTemp _scopeState
        tCond = VariableIr tCondName 0 BOOL_TYPE True
        tCondPu = PURE_BASE_IR (VAR_IR tCond)
        initState = irProcessingStateUpdateScopeState newScopeState state
        (m_condExp, condState) = irCond eCond initState
        asnCommCond = 
            case m_condExp of
                Just expPu -> ASN_PURE_IR tCond expPu
                Nothing -> ASN_PURE_IR tCond dummyPureIr
        
        -- add SPLIT comm on tCond (and prepare predecessor injection) - then commit current BB
        condBBIndex = bbIndex . irProcStateCurrBb $ condState
        splitComm = SPLIT_BB_IR tCondPu 0 0
        predSplitComm1 = predecessorCommandsAddSplit predecessorCommandEmpty condBBIndex 0
        predSplitComm2 = predecessorCommandsAddSplit predecessorCommandEmpty condBBIndex 1
        splitState = 
            irProcessingStateCommitBB .
            (irProcessingStateAddComms [splitComm, asnCommCond]) $
            condState

        -- start a new BB + inject BB->SPLIT comm + process exp1 + prepare GOTO injection - then commit
        startState1 = 
            (applyPredecessorCommands predSplitComm1) .
            irProcessingStateAddAndUpdateBB $
            splitState
        (m_expPT1, expState1) = irExp e1 startState1
        asnComm1 = 
            case m_expPT1 of
                Just (expPu1, expTy1) -> ASN_PURE_IR (VariableIr tAsnName 0 expTy1 True) expPu1
                Nothing -> ASN_PURE_IR (dummyVariableIr tAsnName) dummyPureIr
        predGotoComm1 = predecessorCommandsSingleton expState1
        termState1 = 
            irProcessingStateCommitBB .
            (irProcessingStateAddComms [asnComm1]) $
            expState1

        -- start a new BB + inject BB->SPLIT comm + process exp1 + prepare GOTO injection - then commit
        startState2 = 
            (applyPredecessorCommands predSplitComm2) .
            irProcessingStateAddAndUpdateBB $
            termState1
        (m_expPT2, expState2) = irExp e2 startState2
        asnComm2 = 
            case m_expPT2 of
                Just (expPu2, expTy2) -> ASN_PURE_IR (VariableIr tAsnName 0 expTy2 True) expPu2
                Nothing -> ASN_PURE_IR (dummyVariableIr tAsnName) dummyPureIr
        predGotoComm2 = predecessorCommandsSingleton expState2
        termState2 = 
            irProcessingStateCommitBB .
            (irProcessingStateAddComms [asnComm2]) $
            expState2

        -- type check expression values + start a new BB + inject BB->GOTO comms
        (m_retPT, errs) = 
            case (m_expPT1, m_expPT2) of
                (Just (_, expTy1), Just (_, expTy2)) ->
                    let errs = 
                            if expTy1 == expTy2 
                                then []
                                else [OP_TYPE_MISMATCH (OpTypeMismatch op [expTy1, expTy2])]
                    in (Just (PURE_BASE_IR (VAR_IR (VariableIr tAsnName 0 expTy1 True)), expTy1), errs)
                _ -> (Nothing, [])
        startState = 
            (irProcessingStateAppendErrs errs) .
            (applyPredecessorCommands (predecessorCommandsMerge predGotoComm1 predGotoComm2)) .
            irProcessingStateAddAndUpdateBB $
            termState2

    in (m_retPT, startState)

irLogBinop :: LogBinopElab -> IrProcessingState -> (Maybe (PureIr, TypeCategory), IrProcessingState)
irLogBinop (LogBinopElab cat op e1 e2) state = 
    let 
        -- allocate temp + process e1 + add asn comm t = e1
        (tempName, newScopeState) = irProcessingScopeStateAddTemp . irProcScopeState $ state
        temp = VariableIr tempName 0 BOOL_TYPE True
        tempPu = PURE_BASE_IR (VAR_IR temp)
        initState = irProcessingStateUpdateScopeState newScopeState state
        (m_expPu1, expState1) = irCond e1 initState
        asnComm1 = 
            case m_expPu1 of
                Just expPu1 -> ASN_PURE_IR temp expPu1
                Nothing -> ASN_PURE_IR temp dummyPureIr

        -- add SPLIT comm on t (and prepare predecessor injection) - then commit current BB + start a new BB + inject BB to SPLIT comm
        splitComm = SPLIT_BB_IR tempPu 0 0
        (splitPos1, splitPos2) = 
            case cat of
                LOGAND_EXP_ELAB -> (0, 1)
                LOGOR_EXP_ELAB -> (1, 0)
        predSplitComm1 = predecessorCommandsAddSplit predecessorCommandEmpty (bbIndex . irProcStateCurrBb $ expState1) splitPos1
        predSplitComm2 = predecessorCommandsAddSplit predecessorCommandEmpty (bbIndex . irProcStateCurrBb $ expState1) splitPos2
        splitState = 
            (applyPredecessorCommands predSplitComm1) .
            irProcessingStateAddAndUpdateBB .
            irProcessingStateCommitBB .
            (irProcessingStateAddComms [splitComm, asnComm1]) $ 
            expState1

        -- process e2 + add asn comm t = e2
        (m_expPu2, expState2) = irCond e2 splitState
        asnComm2 = 
            case m_expPu2 of
                Just expPu2 -> ASN_PURE_IR temp expPu2
                Nothing -> ASN_PURE_IR temp dummyPureIr

        -- prepare predecessor injection for GOTO + commit current BB + start a new BB + inject SPLIT/GOTO comms to BB
        predComm2 = predecessorCommandsMerge predSplitComm2 (predecessorCommandsSingleton expState2)
        startState = 
            (applyPredecessorCommands predComm2) .
            irProcessingStateAddAndUpdateBB .
            irProcessingStateCommitBB .
            (irProcessingStateAddComms [asnComm2]) $
            expState2

        -- add errors and return state
    in (Just (tempPu, BOOL_TYPE), startState)

irBinop :: BinopElab -> IrProcessingState -> (Maybe (PureIr, TypeCategory), IrProcessingState)
irBinop (BinopElab cat op e1 e2) state =
    let (m_expPT1, expState1) = irExp e1 state
        (m_expPT2, expState2) = irExp e2 expState1
     in case (m_expPT1, m_expPT2) of
            -- type check when we have 2 valid exps
            (Just (expPu1, expTy1), Just (expPu2, expTy2)) ->
                let m_infType = binopTypeInf cat expTy1 expTy2
                 in case m_infType of
                        -- return pure exp with inferred type
                        Just infType ->
                            let (binopComms, binopPu, binopScopeState) = binopOpTranslate cat infType expPu1 expPu2 (irProcScopeState expState2)
                                binopState = 
                                    (irProcessingStateAddComms binopComms) .
                                    (irProcessingStateUpdateScopeState binopScopeState) $
                                    expState2
                            in (Just (binopPu, infType), binopState)                            
                        -- fail when type for binop cannot be inferred
                        Nothing ->
                            (Nothing, irProcessingStateAppendErrs [OP_TYPE_MISMATCH (OpTypeMismatch op [expTy1, expTy2])] expState2)
            -- immediately fail if either exp is invalid
            _ ->
                (Nothing, expState2)

irUnop :: UnopElab -> IrProcessingState -> (Maybe (PureIr, TypeCategory), IrProcessingState)
irUnop (UnopElab cat op e) state =
    let (m_expPT, expState) = irExp e state
     in case m_expPT of
            -- type check when the exp is valid
            Just (expPu, expTy) ->
                let m_infType = unopTypeInf cat expTy
                 in case m_infType of
                        -- return pure exp with inferred type
                        Just infType ->
                            let (unopComms, unopPu, unopScopeState) = unopOpTranslate cat infType expPu (irProcScopeState expState)
                                unopState = 
                                    (irProcessingStateAddComms unopComms) .
                                    (irProcessingStateUpdateScopeState unopScopeState) $
                                    expState
                             in (Just (unopPu, infType), unopState)
                        -- fail when type for unop cannot be inferred
                        Nothing ->
                            (Nothing, irProcessingStateAppendErrs [OP_TYPE_MISMATCH (OpTypeMismatch op [expTy])] state)
            -- immediately fail if exp is invalid
            _ ->
                (Nothing, state)

irFunctionCall :: FunctionCallElab -> IrProcessingState -> (Maybe (PureIr, TypeCategory), IrProcessingState)
irFunctionCall fnCallElab state = 
    let (m_argsPT, argExtractState) = 
            foldl
                (\(interArgs, interState) argExpElab ->
                    let (m_expPT, newState) = irExp argExpElab interState
                    in (interArgs ++ [m_expPT], newState)
                )
                ([], state)
                (functionCallElabArgs fnCallElab)
        expExtractFailed = any Maybe.isNothing m_argsPT
    in if not expExtractFailed
            -- verify all argument processing succeeded
            then
                case Map.lookup (extractIdentifierName . functionCallElabName $ fnCallElab) (globalFnScopeSignatures . globalFnScope . irProcScopeState $ state) of
                    -- verify the function is defined
                    Just fnSignElab ->
                        let 
                            -- extract pure/types from caller args
                            (callerArgsPuBase, callerArgsTy) = 
                                unzip
                                (map
                                    (\m_argPT ->
                                        case m_argPT of
                                            Just pt -> pt
                                            Nothing -> error. compilerError $ "Attempted to extract caller arg type in fn translation from Nothing when should have aborted"
                                    )
                                    m_argsPT)
                            
                            -- verify type matches with function signature
                            fnElabTy = map (typeElabType . variableElabType) (functionSignatureElabArgs fnSignElab)
                            typeMismatchErrs = 
                                if irFunctionArgsMismatchedErrs fnElabTy callerArgsTy
                                    then [ARG_MISMATCH (ArgMismatchError (functionCallElabName fnCallElab) fnElabTy callerArgsTy)]
                                    else []
                            
                            -- extract pure base from pure args and generate fn call comm(s)
                            (argsPuBase, argsProcessComms, argsProcessScopeState)  = 
                                foldl
                                    (\(interArgs, interComms, interScopeState) argPu ->
                                        let (comms, argPuBase, newScopeState) = expandPureIr argPu interScopeState
                                        in (interArgs ++ [argPuBase], comms ++ interComms, newScopeState)
                                    )
                                    ([], [], irProcScopeState argExtractState)
                                    callerArgsPuBase
                            fnRetTy = typeElabType . functionSignatureElabRetType $ fnSignElab
                            (fnCallTempName, fnCallScopeState) = irProcessingScopeStateAddTemp argsProcessScopeState
                            fnCallTemp = VariableIr fnCallTempName 0 fnRetTy True
                            fnCallPu = PURE_BASE_IR (VAR_IR fnCallTemp)
                            fnCallComm = 
                                ASN_IMPURE_IR 
                                    fnCallTemp
                                    (IMPURE_FNCALL_IR
                                        (ImpureFnCallIr 
                                            (extractIdentifierName . functionCallElabName $ fnCallElab)
                                            argsPuBase
                                            fnRetTy))
                            fnCallState = 
                                (irProcessingStateAppendErrs typeMismatchErrs) .
                                (irProcessingStateUpdateScopeState argsProcessScopeState) .
                                (irProcessingStateAddComms (fnCallComm:argsProcessComms)) $ 
                                argExtractState
                        in (Just (fnCallPu, fnRetTy), fnCallState)
                    Nothing -> 
                        (Nothing, irProcessingStateAppendErrs [USE_BEFORE_DECL (UseBeforeDeclarationError (functionCallElabName fnCallElab))] state)
            else
                (Nothing, state)

irFunctionArgsMismatchedErrs :: [TypeCategory] -> [TypeCategory] -> Bool
irFunctionArgsMismatchedErrs fnElabTy fnCallTy = 
    if (length fnElabTy) /= (length fnCallTy)
        then True
        else
            if 
                any
                    (\(argElabTy, argCallTy) -> argCallTy == VOID_TYPE || argElabTy /= argCallTy)
                    (zip fnElabTy fnCallTy)
                then True
                else False

-- MANAGE AND ACCESS IR PROCESSING STATE - BASE UTILITIES

irProcessingStateUpdateBB :: BasicBlockIr -> IrProcessingState -> IrProcessingState
irProcessingStateUpdateBB bb state = 
    IrProcessingState
        bb
        (irProcStateFunctionIr state)
        (irProcStateErrors state)
        (irProcStateBbCtr state)
        (irProcScopeState state)

irProcessingStateUpdateFn :: FunctionIr -> IrProcessingState -> IrProcessingState
irProcessingStateUpdateFn fnIr state = 
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
        (errs ++ (irProcStateErrors state))
        (irProcStateBbCtr state)
        (irProcScopeState state)

irProcessingStateUpdateScopeState :: IrProcessingScopeState -> IrProcessingState -> IrProcessingState
irProcessingStateUpdateScopeState scopeState state = 
    IrProcessingState
        (irProcStateCurrBb state)
        (irProcStateFunctionIr state)
        (irProcStateErrors state)
        (irProcStateBbCtr state)
        scopeState

irProcessingStateAddBB :: IrProcessingState -> (BasicBlockIr, IrProcessingState)
irProcessingStateAddBB state = 
    let newBb = BasicBlockIr (functionIrIdentifier . irProcStateFunctionIr $ state) (irProcStateBbCtr state) Map.empty []
        newIrProcState = 
            IrProcessingState
                (irProcStateCurrBb state)
                (irProcStateFunctionIr state)
                (irProcStateErrors state)
                ((irProcStateBbCtr state) + 1)
                (irProcScopeState state)
    in (newBb, newIrProcState)

-- MANAGE AND ACCESS IR PROCESSING STATE - HIGH-LEVEL UTILITIES

irProcessingStateCommitBB :: IrProcessingState -> IrProcessingState
irProcessingStateCommitBB state = 
    irProcessingStateUpdateFn (addBbsToFunction [(irProcStateCurrBb state)] (irProcStateFunctionIr state)) state

irProcessingStateAddComms :: [CommandIr] -> IrProcessingState -> IrProcessingState
irProcessingStateAddComms comms state = 
    let updatedBBIr = appendCommsToBb (irProcStateCurrBb state) comms
    in irProcessingStateUpdateBB updatedBBIr state

irProcessingStateAddAndUpdateBB :: IrProcessingState -> IrProcessingState
irProcessingStateAddAndUpdateBB state = 
    let (newBb, _state) = irProcessingStateAddBB state
        updatedState = irProcessingStateUpdateBB newBb _state
    in updatedState

-- UPDATE AND APPLY PROCESSOR COMMANDS

predecessorCommandEmpty :: PredecessorCommands
predecessorCommandEmpty = PredecessorCommands [] []

predecessorCommandsSingleton :: IrProcessingState -> PredecessorCommands
predecessorCommandsSingleton state = 
    let currBbIndex = (bbIndex (irProcStateCurrBb (state)))
    in PredecessorCommands [currBbIndex] []

predecessorCommandsMerge :: PredecessorCommands -> PredecessorCommands -> PredecessorCommands
predecessorCommandsMerge predComms1 predComms2 = 
    PredecessorCommands 
        ((predecessorCommandsGotoBlocks predComms1) ++ (predecessorCommandsGotoBlocks predComms2))
        ((predecessorCommandsSplitBlocks predComms1) ++ (predecessorCommandsSplitBlocks predComms2))

predecessorCommandsAddGoto :: PredecessorCommands -> Int -> PredecessorCommands
predecessorCommandsAddGoto predComms predIndex = 
    PredecessorCommands 
        (predIndex:(predecessorCommandsGotoBlocks predComms))
        (predecessorCommandsSplitBlocks predComms)
        
predecessorCommandsAddSplit :: PredecessorCommands -> Int -> Int -> PredecessorCommands
predecessorCommandsAddSplit predComms predIndex splitPos = 
    PredecessorCommands 
        (predecessorCommandsGotoBlocks predComms)
        ((predIndex, splitPos):(predecessorCommandsSplitBlocks predComms))

applyPredecessorCommands :: PredecessorCommands -> IrProcessingState -> IrProcessingState
applyPredecessorCommands predComms state
    | debugLogs && (Trace.trace 
        ("\n\napplyPredecessorCommands -- " ++ 
            "\npredComms=" ++    (Pretty.ppShow predComms) ++
            "\nstate=" ++    (Pretty.ppShow state)
        ) 
        False) = undefined
applyPredecessorCommands predComms state = 
    let successorBbIndex = (bbIndex (irProcStateCurrBb state))
        fnIrAddedGotos = 
            foldr
                (\(predBlockIndex) interFnIr ->
                    case Map.lookup predBlockIndex (functionIrBlocks interFnIr) of
                        Just bb -> 
                            let newBBIr = applyPredecessorGoto bb successorBbIndex
                                newFnIr = 
                                    ((addEdgeToCFG predBlockIndex successorBbIndex) . 
                                    (addBbsToFunction [newBBIr]))
                                    interFnIr
                            in newFnIr
                        Nothing -> error . compilerError $ "Attempted to insert successor after nonexistent predecessor BasicBlock GOTO: predBB=" ++ (show predBlockIndex) ++ " succBB=" ++ (show successorBbIndex)
                )
                (irProcStateFunctionIr state)
                (predecessorCommandsGotoBlocks predComms)
        fnIrAddedSplits = 
            foldr
                (\(predBlockIndex, succSplitPos) interFnIr ->
                    case Map.lookup predBlockIndex (functionIrBlocks interFnIr) of
                        Just bb -> 
                            let newBBIr = applyPredecessorSplit bb successorBbIndex succSplitPos
                                newFnIr = 
                                    ((addEdgeToCFG predBlockIndex successorBbIndex) . 
                                    (addBbsToFunction [newBBIr]))
                                    interFnIr
                            in newFnIr
                        Nothing -> error . compilerError $ "Attempted to insert successor after nonexistent predecessor BasicBlock SPLIT: predBB=" ++ (show predBlockIndex) ++ " succBB=" ++ (show successorBbIndex)
                )
                fnIrAddedGotos
                (predecessorCommandsSplitBlocks predComms)
    in irProcessingStateUpdateFn fnIrAddedSplits state
    
-- GOTO commands are directly appended to the block (i.e. the block does not have an existing GOTO command)
applyPredecessorGoto :: BasicBlockIr -> Int -> BasicBlockIr
applyPredecessorGoto predBlock succIndex = 
    appendCommsToBb predBlock [GOTO_BB_IR succIndex]

-- SPLIT commands are modified in the block (i.e., the block has an existing SPLIT command)
applyPredecessorSplit :: BasicBlockIr -> Int -> Int -> BasicBlockIr
applyPredecessorSplit predBlock succIndex splitPos = 
    case (head (bbIrCommands predBlock)) of
        SPLIT_BB_IR base left right ->
            let newSplit = 
                    case splitPos of
                        0 -> SPLIT_BB_IR base succIndex right
                        1 -> SPLIT_BB_IR base left succIndex
                        _ -> error (compilerError ("Attempted to insert successor after predecessor BasicBlock with invalid split index: BasicBlockIr=" ++ (show predBlock) ++ " Index=" ++ (show splitPos)))
                newCommands = newSplit:(tail (bbIrCommands predBlock))
                newBBIr = BasicBlockIr (bbIrFnName predBlock) (bbIndex predBlock) (bbIrPhiFn predBlock) newCommands
            in newBBIr
        _ -> error (compilerError ("Attempted to insert successor after predecessor BasicBlock with no SPLIT command: BasicBlockIr=" ++ (show predBlock)))

-- MANAGE AND ACCESS SCOPE STATE

irProcessingScopeStateAddTemp :: IrProcessingScopeState -> (String, IrProcessingScopeState)
irProcessingScopeStateAddTemp state =
    let tempName = (show (regCtr state)) ++ ":"
        newIrProcScopeState = 
            IrProcessingScopeState
                (scopes state) 
                (globalFnScope state)
                ((regCtr state) + 1) 
                (mapCtr state)
    in (tempName, newIrProcScopeState)

irProcessingScopeStateAddScope :: IrProcessingScopeState -> IrProcessingScopeState
irProcessingScopeStateAddScope state =
    let newScope = Scope Map.empty (mapCtr state)
    in IrProcessingScopeState
            (newScope : (scopes state)) 
            (globalFnScope state)
            (regCtr state) 
            ((mapCtr state) + 1)

irProcessingScopeStateInsertToTopScope :: IrProcessingScopeState -> String -> VariableElab -> Bool -> IrProcessingScopeState
irProcessingScopeStateInsertToTopScope state name varElab initAssigned =
    let topScope = head (scopes state)
        varScope = 
            Scope 
                (Map.insert name (varElab, initAssigned) (scopeMap topScope)) 
                (scopeId topScope)
    in IrProcessingScopeState
            (varScope : (tail (scopes state))) 
            (globalFnScope state)
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
            (globalFnScope state)
            (regCtr state) 
            (mapCtr state)

irProcessingScopeStatePopScopeMap :: IrProcessingScopeState -> IrProcessingScopeState
irProcessingScopeStatePopScopeMap state = 
    IrProcessingScopeState 
        (tail (scopes state)) 
        (globalFnScope state)
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

findConflictFnDecl :: GlobalFnScope -> FunctionSignatureElab -> Maybe FunctionSignatureElab
findConflictFnDecl fnScope fnSignElab = 
    case Map.lookup (extractIdentifierName . functionSignatureElabName $ fnSignElab) (globalFnScopeSignatures fnScope) of
        Just currFnSignElab ->
            if currFnSignElab /= fnSignElab
                then Just currFnSignElab
                else Nothing
        Nothing -> Nothing

findDuplicateFnDefn :: GlobalFnScope -> FunctionSignatureElab -> Maybe FunctionSignatureElab
findDuplicateFnDefn fnScope fnSignElab = 
    let fnName = extractIdentifierName . functionSignatureElabName $ fnSignElab
    in if not (Set.member fnName (globalFnScopeDefined fnScope))
            then Nothing
            else 
                case Map.lookup fnName (globalFnScopeSignatures fnScope) of
                    Just currFnSignElab ->
                        if fnSignElab == currFnSignElab
                            then Just currFnSignElab
                            else Nothing

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
            if t1 == t2
                then Just BOOL_TYPE
                else Nothing
        NEQ_EXP_ELAB ->
            case (t1, t2) of
                (INT_TYPE, INT_TYPE) -> Just BOOL_TYPE
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
                let (tempName, newState) = irProcessingScopeStateAddTemp state
                    temp = VariableIr tempName 0 ty True
                    binop = IMPURE_BINOP_IR (ImpureBinopIr DIV_IR ty expandPureBase1 expandPureBase2)
                    comm = ASN_IMPURE_IR temp binop
                 in (comm : (expandComms2 ++ expandComms1), PURE_BASE_IR (VAR_IR temp), newState)
            MOD_EXP_ELAB ->
                let (tempName, newState) = irProcessingScopeStateAddTemp state
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
                (expandComms2 ++ expandComms1, PURE_BINOP_IR (PureBinopIr SAL_IR ty expandPureBase1 expandPureBase2), expandState2)
            SRA_EXP_ELAB ->
                (expandComms2 ++ expandComms1, PURE_BINOP_IR (PureBinopIr SAR_IR ty expandPureBase1 expandPureBase2), expandState2)
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
            let (tempName, newState) = irProcessingScopeStateAddTemp state
                binopTemp = VariableIr tempName 0 (pureBinopInfType binop) True
                binopComm = ASN_PURE_IR binopTemp pu
             in ([binopComm], VAR_IR binopTemp, newState)
        PURE_UNOP_IR unop ->
            let (tempName, newState) = irProcessingScopeStateAddTemp state
                unopTemp = VariableIr tempName 0 (pureUnopInfType unop) True
                unopComm = ASN_PURE_IR unopTemp pu
             in ([unopComm], VAR_IR unopTemp, newState)
