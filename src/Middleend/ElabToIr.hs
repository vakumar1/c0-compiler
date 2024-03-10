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

irFunction :: FunctionElab -> (FunctionIr, [VerificationError])
irFunction fnElab =
    let (initBb, initState) = addBb (addScope (State [] 0 0 0))
        initFnIr = FunctionIr Map.empty Map.empty Map.empty Set.empty
        (_, _, finalFnIr, errs) = irSeq fnElab (functionElabBlock fnElab) initState initBb initFnIr []
     in (finalFnIr, errs)

-- Seq Elab -> IR is responsible for:
-- i. constructing all new BasicBlocks (except the initial BB)
-- ii. appending commands to the current BB
-- iii. adding all BBs it terminates to function-wide BB tracker map
-- iv. adding edges between BBs in function-wide CFG (and adding corresponding instructions)
-- v. returning the indices of the ENTRY BB and EXIT BB of the Seq
irSeq ::
    FunctionElab ->
    SeqElab ->
    State ->
    BasicBlockIr ->
    FunctionIr ->
    [VerificationError] ->
    ((Int, Int), State, FunctionIr, [VerificationError])
irSeq fnElab seq state currBb fnIr errs =
    let currIndex = bbIndex currBb
     in case seq of
            -- i. terminate current basic block and add to fnIr
            [] ->
                let termFnIr = addBbsToFunction fnIr [currBb]
                 in ((currIndex, currIndex), state, termFnIr, errs)
            RET_ELAB retElab : _ ->
                let (retComms, retState, retErrs) = irRet retElab fnElab state
                    retBb = appendCommsToBb currBb retComms
                    retFnIr = addBbsToFunction fnIr [retBb]
                 in ((currIndex, currIndex), retState, retFnIr, retErrs ++ errs)
            -- ii. extend current basic block
            DECL_ELAB declElab : _ ->
                let (declComms, declState, declErrs) = irDecl declElab state
                    declBb = appendCommsToBb currBb declComms
                 in irSeq fnElab (tail seq) declState declBb fnIr (declErrs ++ errs)
            ASN_ELAB asnElab : _ ->
                let (asnComms, asnState, asnErrs) = irAsn asnElab state
                    asnBb = appendCommsToBb currBb asnComms
                 in irSeq fnElab (tail seq) asnState asnBb fnIr (asnErrs ++ errs)
            EXP_ELAB expElab : _ ->
                let (expComms, expState, expErrs) = irExpStmt expElab state
                    expBb = appendCommsToBb currBb expComms
                in irSeq fnElab (tail seq) expState expBb fnIr (expErrs ++ errs)
            -- iii. start new basic block
            SEQ_ELAB innerSeq : _ ->
                -- create inner block; create new scope; and evaluate for inner seq (then clean up top scope)
                let (innerBb, createInnerState) = addBb state
                    addScopeState = addScope createInnerState
                    ((innerStartIndex, innerEndIndex), innerState, innerFnIr, innerErrs) = irSeq fnElab innerSeq addScopeState innerBb fnIr []
                    removeScopeState = popScopeMap innerState

                    -- curr => inner blocks: add goto command and CFG edges
                    currInnerComm = GOTO_BB_IR innerStartIndex
                    currInnerBb = appendCommsToBb currBb [currInnerComm]
                    currInnerFnIr = addEdgeToCFG innerFnIr currIndex innerStartIndex
                    innerFinishedBb =
                        -- note: the innerEndIndex BB must be in the latest fnIr since it terminates the BB
                        -- and is responsible for adding it to the fnIr it returns
                        case (Map.lookup innerEndIndex (functionIrBlocks currInnerFnIr)) of
                            Just bb -> bb
                 in -- o/w create tail block
                    if (bbTerminates innerFinishedBb)
                        then -- ignore tail block if inner BB terminates

                            let
                                -- add updated curr block to fnIr
                                finalFnIr = addBbsToFunction currInnerFnIr [currInnerBb]
                                finalErrs = innerErrs ++ errs
                             in
                                ((currIndex, innerEndIndex), removeScopeState, finalFnIr, finalErrs)
                        else -- o/w create tail block

                            let
                                -- create next block; and evaluate for tail seq
                                nextSeq = tail seq
                                (nextBb, createNextState) = addBb removeScopeState
                                ((nextStartIndex, nextEndIndex), nextState, nextFnIr, nextErrs) = irSeq fnElab nextSeq createNextState nextBb currInnerFnIr []

                                -- inner => next blocks: add goto command and CFG edges
                                innerNextComm = GOTO_BB_IR nextStartIndex
                                innerNextBb = appendCommsToBb innerFinishedBb [innerNextComm]
                                innerNextFnIr = addEdgeToCFG nextFnIr innerEndIndex nextStartIndex

                                -- add updated curr/inner blocks to fnIr
                                finalFnIr = addBbsToFunction innerNextFnIr [currInnerBb, innerNextBb]
                                finalErrs = nextErrs ++ innerErrs ++ errs
                             in
                                ((currIndex, nextEndIndex), nextState, finalFnIr, finalErrs)

-- STATEMENT ELAB->IR

irDecl :: DeclElab -> State -> ([CommandIr], State, [VerificationError])
irDecl (DeclElab varElab Nothing) state =
    let name = extractIdentifierName (variableElabIdentifier varElab)
        varState = insertToTopScope state name varElab
     in case Map.lookup name (scopeMap (head (scopes state))) of
            Just (prevVarElab, _) ->
                let err = (DOUBLE_DECL (DoubleDeclarationError (variableElabIdentifier prevVarElab) (variableElabIdentifier varElab)))
                 in ([], state, [err])
            Nothing ->
                let varState = insertToTopScope state name varElab
                    var = varElabToIr varElab (scopeId (head (scopes state)))
                    comm = INIT_IR var
                 in ([comm], varState, [])
irDecl (DeclElab varElab (Just asn)) state =
    let (declComms, declState, declErrs) = irDecl (DeclElab varElab Nothing) state
        (asnComms, asnState, asnErrs) = irAsn asn declState
     in ((asnComms ++ declComms), asnState, (asnErrs ++ declErrs))

irAsn :: AsnElab -> State -> ([CommandIr], State, [VerificationError])
irAsn (AsnElab tok e) state =
    let (expComms, m_expPT, expState, expErrs) = irExp e state
        m_varElab = identifierLookup tok (scopes state)
     in case m_varElab of
            -- check that varElab is declared
            Just (varElab, _, varScopeId) ->
                let varName = extractIdentifierName (variableElabIdentifier varElab)
                    asnErrState = setAssignedInScope state varName varElab varScopeId
                 in case m_expPT of
                        Just (expPu, expTy) ->
                            -- check that var and exp have the same type
                            let varTy = (typeElabType (variableElabType varElab))
                                asnComm = ASN_PURE_IR (varElabToIr varElab varScopeId) expPu
                             in if varTy == expTy
                                    then
                                        let asnState = setAssignedInScope expState varName varElab varScopeId
                                         in ((asnComm : expComms), asnState, expErrs)
                                    else ([], asnErrState, (ASN_TYPE_MISMATCH (AsnTypeMismatch tok varTy expTy)) : expErrs)
                        Nothing ->
                            ([], asnErrState, expErrs)
            -- fail if varElab is not declared
            Nothing ->
                ([], state, (USE_BEFORE_DECL (UseBeforeDeclarationError tok)) : expErrs)

irExpStmt :: ExpElab -> State -> ([CommandIr], State, [VerificationError])
irExpStmt e state = 
    let (expComms, _, expState, expErrs) = irExp e state
    in (expComms, expState, expErrs)

irRet :: RetElab -> FunctionElab -> State -> ([CommandIr], State, [VerificationError])
irRet (RetElab e) (FunctionElab _ (TypeElab retTy _) _) state =
    let (expComms, m_expPT, expState, expErrs) = irExp e state
     in case m_expPT of
            Just (expPu, expTy) ->
                if (retTy == expTy)
                    then
                        let retComm = RET_PURE_IR expPu
                         in (retComm : expComms, expState, expErrs)
                    else ([], state, (RET_TYPE_MISMATCH (RetTypeMismatch retTy expTy)) : expErrs)
            Nothing ->
                ([], state, expErrs)

-- EXP ELAB->IR

irExp :: ExpElab -> State -> ([CommandIr], Maybe (PureIr, TypeCategory), State, [VerificationError])
irExp e state =
    case e of
        CONST_ELAB c -> irConst c state
        IDENTIFIER_ELAB i -> irIdentifier i state
        BINOP_ELAB b -> irBinop b state
        UNOP_ELAB u -> irUnop u state

irConst :: Const -> State -> ([CommandIr], Maybe (PureIr, TypeCategory), State, [VerificationError])
irConst const state = ([], Just (PURE_BASE_IR (CONST_IR const), constToType const), state, [])

irIdentifier :: Token -> State -> ([CommandIr], Maybe (PureIr, TypeCategory), State, [VerificationError])
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

irBinop :: BinopElab -> State -> ([CommandIr], Maybe (PureIr, TypeCategory), State, [VerificationError])
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

irUnop :: UnopElab -> State -> ([CommandIr], Maybe (PureIr, TypeCategory), State, [VerificationError])
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

-- HELPERS: MANAGE AND ACCESS ELABORATED STATE

data State = State
    { scopes :: [Scope]
    , regCtr :: Int
    , mapCtr :: Int
    , bbCtr :: Int
    }

data Scope = Scope
    { scopeMap :: Map.Map String (VariableElab, Bool)
    , scopeId :: Int
    }

addTemp :: State -> (String, State)
addTemp state =
    let tempName = (show (regCtr state)) ++ ":"
     in (tempName, State (scopes state) ((regCtr state) + 1) (mapCtr state) (bbCtr state))

addScope :: State -> State
addScope state =
    let newScope = Scope Map.empty (mapCtr state)
     in State (newScope : (scopes state)) (regCtr state) ((mapCtr state) + 1) (bbCtr state)

addBb :: State -> (BasicBlockIr, State)
addBb state =
    let newBb = BasicBlockIr (bbCtr state) Map.empty []
     in (newBb, State (scopes state) (regCtr state) (mapCtr state) ((bbCtr state) + 1))

insertToTopScope :: State -> String -> VariableElab -> State
insertToTopScope state name varElab =
    let topScope = head (scopes state)
        varScope = Scope (Map.insert name (varElab, False) (scopeMap topScope)) (scopeId topScope)
     in State (varScope : (tail (scopes state))) (regCtr state) (mapCtr state) (bbCtr state)

setAssignedInScope :: State -> String -> VariableElab -> Int -> State
setAssignedInScope state varName varElab varScopeId =
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
     in State newScopes (regCtr state) (mapCtr state) (bbCtr state)

popScopeMap :: State -> State
popScopeMap state = State (tail (scopes state)) (regCtr state) (mapCtr state) (bbCtr state)

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

-- HELPERS: OP TRANSLATION

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

binopOpTranslate :: BinopCatElab -> TypeCategory -> PureIr -> PureIr -> State -> ([CommandIr], PureIr, State)
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

unopOpTranslate :: UnopCatElab -> TypeCategory -> PureIr -> State -> ([CommandIr], PureIr, State)
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
expandPureIr :: PureIr -> State -> ([CommandIr], PureBaseIr, State)
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
