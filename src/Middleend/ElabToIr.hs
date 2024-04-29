module Middleend.ElabToIr (
    irFunction,
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

data PredecessorCommands = PredecessorCommands
    { predecessorCommandsGotoBlocks :: [Int]            -- list of predecessor BasicBlock indexes to inject GOTO commands
    , predecessorCommandsSplitBlocks :: [(Int, Int)]    -- list of predecessor BasicBlock indexes to inject SPLIT commands
    }
    deriving Show

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
    let initBbIr = BasicBlockIr 0 Map.empty []
        initFnIr = FunctionIr Map.empty emptyGraph
        initScopeState = IrProcessingScopeState [] 0 0
        initState = IrProcessingState initBbIr initFnIr [] 1 initScopeState
        (finalTerm, _, _, finalState) = irSeq (functionElabBlock fnElab) fnElab initState
        errs = 
            if finalTerm
                then (irProcStateErrors finalState)
                else (INVALID_RET (InvalidReturnError (functionElabName fnElab))):(irProcStateErrors finalState)
        fnIr = (irProcStateFunctionIr finalState)
    in (fnIr, errs)

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
                    let termFnIr = addBbsToFunction [(irProcStateCurrBb state)] (irProcStateFunctionIr state)
                        termState = irProcessingStateUpdateFn termFnIr state
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
        (m_condPu, initCondComms, condScopeState, condErrs) = irCond condExp (irProcScopeState state)
        condComms = 
            case m_condPu of
                Just condPu -> (SPLIT_BB_IR condPu 0 0):initCondComms
                Nothing -> (SPLIT_BB_IR dummyPureIr 0 0):initCondComms
        termBbIr = appendCommsToBb (irProcStateCurrBb state) condComms
        termState = 
            (irProcessingStateUpdateBB termBbIr) .
            (irProcessingStateAppendErrs condErrs) .
            (irProcessingStateUpdateScopeState condScopeState) $
            state
        termBbIndex = bbIndex termBbIr

        -- evaluate inner stmt and return preds w/ split command for term block
        termInnerPreds = predecessorCommandsAddSplit predecessorCommandEmpty termBbIndex 0
        (innerTerm, innerStartBb, innerPreds, innerState) = irStatement ifStmt fnElab (True, termInnerPreds, termState)
        outerPreds = predecessorCommandsAddSplit innerPreds termBbIndex 1

        resetFinalTermBBIr = 
            case Map.lookup (bbIndex termBbIr) (functionIrBlocks . irProcStateFunctionIr $ innerState) of
                Just bb -> bb
                Nothing -> error . compilerError $ "Attempted to retrieve updated cond BB but was never committed by if stmt: bbIndex=" ++ (show . bbIndex $ termBbIr)
        resetFinalTermState = irProcessingStateUpdateBB resetFinalTermBBIr innerState
    in (False, True, outerPreds, resetFinalTermState)

irIf (IfElab condExp ifStmt (Just elseStmt)) fnElab state =
    let 
        -- evaluate if cond exp and add split comm (with both indices unfilled) to curr basic block
        (m_condPu, initCondComms, condScopeState, condErrs) = irCond condExp (irProcScopeState state)
        condComms = 
            case m_condPu of
                Just condPu -> (SPLIT_BB_IR condPu 0 0):initCondComms
                Nothing -> (SPLIT_BB_IR dummyPureIr 0 0):initCondComms
        termBbIr = appendCommsToBb (irProcStateCurrBb state) condComms
        termState = 
            ((irProcessingStateUpdateBB termBbIr) .
            (irProcessingStateAppendErrs condErrs) .
            (irProcessingStateUpdateScopeState condScopeState))
            state

        -- evaluate if stmt
        termIfPreds = predecessorCommandsAddSplit predecessorCommandEmpty (bbIndex termBbIr) 0
        (ifTerm, ifStartBb, ifPreds, ifState) = irStatement ifStmt fnElab (True, termIfPreds, termState)

        -- evaluate else stmt
        resetElseTermBBIr = 
            case Map.lookup (bbIndex termBbIr) (functionIrBlocks . irProcStateFunctionIr $ ifState) of
                Just bb -> bb
                Nothing -> error . compilerError $ "Attempted to retrieve updated cond BB but was never committed by if stmt: bbIndex=" ++ (show . bbIndex $ termBbIr)
        resetElseTermState = irProcessingStateUpdateBB resetElseTermBBIr ifState
        termElsePreds = predecessorCommandsAddSplit predecessorCommandEmpty (bbIndex termBbIr) 1
        (elseTerm, elseStartBb, elsePreds, elseState) = irStatement elseStmt fnElab (True, termElsePreds, resetElseTermState)

        resetFinalTermBBIr = 
            case Map.lookup (bbIndex termBbIr) (functionIrBlocks . irProcStateFunctionIr $ elseState) of
                Just bb -> bb
                Nothing -> error . compilerError $ "Attempted to retrieve updated cond BB but was never committed by if stmt: bbIndex=" ++ (show . bbIndex $ termBbIr)
        resetFinalTermState = irProcessingStateUpdateBB resetFinalTermBBIr elseState
    in (ifTerm && elseTerm, True, predecessorCommandsMerge ifPreds elsePreds, resetFinalTermState)

irWhile :: WhileElab -> FunctionElab -> IrProcessingState -> (Bool, Bool, PredecessorCommands, IrProcessingState)
irWhile (WhileElab condExp whileStmt) fnElab state = 
    let 
        -- PART I. cond block preparation

        -- manually prepare the cond basic block
        -- i. create the basic block
        -- ii. add all cond commands + terminate with a split (with both indices unfilled)
        (condBbIr, initState) = irProcessingStateAddBB state
        (m_condPu, initCondComms, condScopeState, condErrs) = irCond condExp (irProcScopeState state)
        condComms = 
            case m_condPu of
                Just condPu -> (SPLIT_BB_IR condPu 0 0):initCondComms
                Nothing -> (SPLIT_BB_IR dummyPureIr 0 0):initCondComms
        condTermBbIr = appendCommsToBb condBbIr condComms
        condBbIndex = bbIndex condBbIr

        -- terminate the current basic block w/ a direct GOTO to cond basic block
        -- also manually add edge in function CFG
        termBbIr = appendCommsToBb (irProcStateCurrBb initState) [(GOTO_BB_IR condBbIndex)]
        manualInjectFnIr = (addEdgeToCFG (bbIndex termBbIr) condBbIndex (irProcStateFunctionIr initState))

        -- commit the original term and cond basic blocks to fn
        condTermFnIr = addBbsToFunction [termBbIr, condTermBbIr] manualInjectFnIr
        condTermState = 
            (irProcessingStateUpdateBB condTermBbIr) .
            (irProcessingStateAppendErrs condErrs) .
            (irProcessingStateUpdateScopeState condScopeState) .
            (irProcessingStateUpdateFn condTermFnIr) $
            initState

        -- PART II. stmt block evaluation + manual injection
        
        -- evaluate stmt basic block (with cond->stmt SPLIT injection)
        condToStmtPreds = predecessorCommandsAddSplit predecessorCommandEmpty condBbIndex 0
        (_, _, stmtToCondPreds, stmtState) = irStatement whileStmt fnElab (True, condToStmtPreds, condTermState)

        -- apply stmt->cond injection:
        -- i. commit the current basic block (whatever it is)
        -- ii. set the current basic block to be the updated cond block
        -- iii. manually apply stmt->cond injection
        stmtTermFnIr = addBbsToFunction [(irProcStateCurrBb stmtState)] (irProcStateFunctionIr stmtState)
        updatedCondTermBBIr = 
            case Map.lookup condBbIndex (functionIrBlocks stmtTermFnIr) of
                Just bb -> bb
                Nothing -> error (compilerError ("Attempted to retrieve updated cond BB but was never committed by while stmt: bbIndex=" ++ (show condBbIndex)))
        stmtTermState = 
            (irProcessingStateUpdateBB updatedCondTermBBIr) .
            (irProcessingStateUpdateFn stmtTermFnIr) $
            stmtState
        injectedState = applyPredecessorCommands stmtToCondPreds stmtTermState

        -- PART III. pass params to upcoming basic block
        condToNextPreds = predecessorCommandsAddSplit predecessorCommandEmpty condBbIndex 1
    in (False, True, condToNextPreds, injectedState)

irCond :: ExpElab -> IrProcessingScopeState -> (Maybe PureIr, [CommandIr], IrProcessingScopeState, [VerificationError])
irCond e scopeState = 
    let 
        -- evaluate if exp and create temp
        (expComms, m_expPT, expScopeState, expErrs) = irExp e scopeState
    in case m_expPT of
        -- fail if exp malformed
        Nothing -> 
            (Nothing, [], expScopeState, expErrs)
        Just (expPu, expTy) ->
            if (BOOL_TYPE /= expTy)
                -- fail on non-bool exp type mismatch
                then 
                    (Just expPu, [], expScopeState, (IF_COND_TYPE_MISMATCH (IfCondTypeMismatch expTy)) : expErrs)
                -- success - add asn command
                else
                    (Just expPu, expComms, expScopeState, expErrs)

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
                    declScopeState = irProcessingScopeStateInsertToTopScope scopeState name varElab
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
    let scopeState = (irProcScopeState state)
        (expComms, m_expPT, expScopeState, expErrs) = irExp e scopeState
        m_varElab = identifierLookup tok (scopes expScopeState)
     in case m_varElab of
            -- fail if varElab is not declared
            Nothing ->
                let errs = (USE_BEFORE_DECL (UseBeforeDeclarationError tok)) : expErrs
                    asnState = 
                        ((irProcessingStateAppendErrs errs) .
                        (irProcessingStateUpdateScopeState expScopeState))
                        state
                in (False, False, predecessorCommandsSingleton asnState, asnState)
            -- check that varElab is declared
            Just (varElab, _, varScopeId) ->
                let varName = extractIdentifierName (variableElabIdentifier varElab)
                    setScopeState = irProcessingScopeStateSetAssignedInScope expScopeState varName varElab varScopeId
                 in case m_expPT of
                        -- fail if exp is malformed
                        Nothing ->
                            let asnState = 
                                    ((irProcessingStateAppendErrs expErrs) .
                                    (irProcessingStateUpdateScopeState setScopeState))
                                    state
                            in (False, False, predecessorCommandsSingleton asnState, asnState)
                        Just (expPu, expTy) ->
                            -- check that var and exp have the same type
                            let varTy = (typeElabType (variableElabType varElab))
                                asnComm = ASN_PURE_IR (varElabToIr varElab varScopeId) expPu
                             in if varTy /= expTy
                                    -- fail on type mismatch
                                    then
                                        let errs = (ASN_TYPE_MISMATCH (AsnTypeMismatch tok varTy expTy)) : expErrs
                                            asnState = 
                                                ((irProcessingStateAppendErrs errs) .
                                                (irProcessingStateUpdateScopeState setScopeState))
                                                state
                                        in (False, False, predecessorCommandsSingleton asnState, asnState)
                                    -- success - add asn statement to basic block
                                    else
                                        let asnBbIr = appendCommsToBb (irProcStateCurrBb state) (asnComm : expComms)
                                            asnState = 
                                                ((irProcessingStateUpdateBB asnBbIr) .
                                                (irProcessingStateAppendErrs expErrs) .
                                                (irProcessingStateUpdateScopeState setScopeState))
                                                state
                                         in (False, False, predecessorCommandsSingleton asnState, asnState)

irExpStmt :: ExpElab -> IrProcessingState -> (Bool, Bool, PredecessorCommands, IrProcessingState)
irExpStmt e state =
    let (expComms, _, expScopeState, expErrs) = irExp e (irProcScopeState state)
        expBbIr = appendCommsToBb (irProcStateCurrBb state) expComms
        expState = 
            ((irProcessingStateUpdateBB expBbIr) .
            (irProcessingStateUpdateScopeState expScopeState) .
            (irProcessingStateAppendErrs expErrs))
            state
    in (False, False, predecessorCommandsSingleton expState, expState)

irRet :: RetElab -> FunctionElab -> IrProcessingState -> (Bool, Bool, PredecessorCommands, IrProcessingState)
irRet (RetElab e) (FunctionElab _ (TypeElab retTy _) _) state =
    let scopeState = (irProcScopeState state)
        (expComms, m_expPT, expScopeState, expErrs) = irExp e scopeState
    in case m_expPT of
            -- fail if exp is malformed
            Nothing ->
                let retState = 
                        ((irProcessingStateAppendErrs expErrs) .
                        (irProcessingStateUpdateScopeState expScopeState))
                        state
                in (True, False, predecessorCommandEmpty, retState)
            Just (expPu, expTy) ->
                if (retTy /= expTy)
                    -- fail on ret/exp type mismatch
                    then 
                        let errs = (RET_TYPE_MISMATCH (RetTypeMismatch retTy expTy)) : expErrs
                            retState = 
                                ((irProcessingStateAppendErrs errs) .
                                (irProcessingStateUpdateScopeState expScopeState))
                                state
                        in (True, False, predecessorCommandEmpty, retState)
                    -- success - add ret statement to basic block and terminate
                    else
                        let comms = (RET_PURE_IR expPu) : expComms
                            retBbIr = (appendCommsToBb (irProcStateCurrBb state) comms)
                            retFnIr = addBbsToFunction [retBbIr] (irProcStateFunctionIr state)
                            retState = 
                                ((irProcessingStateUpdateFn retFnIr) .
                                (irProcessingStateAppendErrs expErrs) .
                                (irProcessingStateUpdateScopeState expScopeState))
                                state
                        in (True, False, predecessorCommandEmpty, retState)

-- EXP ELAB->IR

irExp :: ExpElab -> IrProcessingScopeState -> ([CommandIr], Maybe (PureIr, TypeCategory), IrProcessingScopeState, [VerificationError])
irExp e state =
    case e of
        CONST_ELAB c -> irConst c state
        IDENTIFIER_ELAB i -> irIdentifier i state
        BINOP_ELAB b -> irBinop b state
        UNOP_ELAB u -> irUnop u state

irConst :: Const -> IrProcessingScopeState -> ([CommandIr], Maybe (PureIr, TypeCategory), IrProcessingScopeState, [VerificationError])
irConst const state = ([], Just (PURE_BASE_IR (CONST_IR const), constToType const), state, [])

irIdentifier :: Token -> IrProcessingScopeState -> ([CommandIr], Maybe (PureIr, TypeCategory), IrProcessingScopeState, [VerificationError])
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

irBinop :: BinopElab -> IrProcessingScopeState -> ([CommandIr], Maybe (PureIr, TypeCategory), IrProcessingScopeState, [VerificationError])
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

irUnop :: UnopElab -> IrProcessingScopeState -> ([CommandIr], Maybe (PureIr, TypeCategory), IrProcessingScopeState, [VerificationError])
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
    let newBb = BasicBlockIr (irProcStateBbCtr state) Map.empty []
        newIrProcState = 
            IrProcessingState
                (irProcStateCurrBb state)
                (irProcStateFunctionIr state)
                (irProcStateErrors state)
                ((irProcStateBbCtr state) + 1)
                (irProcScopeState state)
    in (newBb, newIrProcState)

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
                                newFnIr = 
                                    ((addEdgeToCFG predBlockIndex successorBbIndex) . 
                                    (addBbsToFunction [newBBIr]))
                                    interFnIr
                            in newFnIr
                        Nothing -> error (compilerError ("Attempted to insert successor after nonexistent predecessor BasicBlock SPLIT: BasicBlockIr=" ++ (show predBlockIndex)))
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
            let (tempName, newState) = irProcessingScopeStateAddTemp state
                binopTemp = VariableIr tempName 0 (pureBinopInfType binop) True
                binopComm = ASN_PURE_IR binopTemp pu
             in ([binopComm], VAR_IR binopTemp, newState)
        PURE_UNOP_IR unop ->
            let (tempName, newState) = irProcessingScopeStateAddTemp state
                unopTemp = VariableIr tempName 0 (pureUnopInfType unop) True
                unopComm = ASN_PURE_IR unopTemp pu
             in ([unopComm], VAR_IR unopTemp, newState)
