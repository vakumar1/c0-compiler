module ElabToIr (
    irFunction,
)
where

import Elaborated
import Errors
import Ir
import Tokens
import Types

import qualified Data.Map as Map

-- IR TRANSLATION
-- + DECLARATION VERIFICATION
-- + TYPE VERIFICATION

irFunction :: FunctionElab -> ([BasicBlockIr], [VerificationError])
irFunction fn =
    let initBb = BasicBlockIr [] []
        emptyState = State [] 0 0
        initState = addScope emptyState
     in irSeq fn (functionElabBlock fn) initBb initState []

irSeq :: FunctionElab -> SeqElab -> BasicBlockIr -> State -> [VerificationError] -> ([BasicBlockIr], [VerificationError])
irSeq fn ss bb state errs =
    case ss of
        -- i. terminate current basic block
        [] -> ([bb], errs)
        RET_ELAB r : _ ->
            let (retComms, _, retErrs) = irRet r fn state
                bbRet = BasicBlockIr (bbIrArgs bb) (retComms ++ (bbIrCommands bb))
             in ([bbRet], retErrs ++ errs)
        -- ii. extend current basic block
        DECL_ELAB d : _ ->
            let (declComms, declState, declErrs) = irDecl d state
                bbDecl = BasicBlockIr (bbIrArgs bb) (declComms ++ (bbIrCommands bb))
             in irSeq fn (tail ss) bbDecl declState (declErrs ++ errs)
        ASN_ELAB a : _ ->
            let (asnComms, asnState, asnErrs) = irAsn a state
                bbAsn = BasicBlockIr (bbIrArgs bb) (asnComms ++ (bbIrCommands bb))
             in irSeq fn (tail ss) bbAsn asnState (asnErrs ++ errs)
        -- iii. start new basic block
        SEQ_ELAB s : _ ->
            let currBbArgs = getIrArgs (scopes state)

                -- evaluate for inner block
                innerBb = BasicBlockIr currBbArgs []
                innerState = addScope state
                (innerSeqBb, innerSeqErrs) = irSeq fn s innerBb innerState []

                -- evaluate for remainder of block
                nextBb = BasicBlockIr currBbArgs []
                (nextSeqBb, nextSeqErrs) = irSeq fn (tail ss) nextBb state []
             in -- concat results from existing bb, inner bb(s), and remaining bb(s)
                (nextSeqBb ++ innerSeqBb ++ [bb], nextSeqErrs ++ innerSeqErrs ++ errs)

irDecl :: DeclElab -> State -> ([CommandIr], State, [VerificationError])
irDecl (DeclElab varElab Nothing) state =
    let name = extractIdentifierName (variableElabIdentifier varElab)
        varState = insertToTopScope state name varElab
     in case Map.lookup name (scopeMap (head (scopes state))) of
            Just prevVarElab ->
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
            Just (varElab, varScopeId) ->
                case m_expPT of
                    Just (expPu, expTy) ->
                        -- check that var and exp have the same type
                        let varTy = (typeElabType (variableElabType varElab))
                            asnComm = ASN_PURE_IR (varElabToIr varElab varScopeId) expPu
                         in if varTy == expTy
                                then ((asnComm : expComms), expState, expErrs)
                                else ([], state, (ASN_TYPE_MISMATCH (AsnTypeMismatch tok varTy expTy)) : expErrs)
                    Nothing ->
                        ([], state, expErrs)
            -- fail if varElab is not declared
            Nothing ->
                ([], state, (USE_BEFORE_DECL (UseBeforeDeclarationError tok)) : expErrs)

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

irExp :: ExpElab -> State -> ([CommandIr], Maybe (PureIr, TypeCategory), State, [VerificationError])
irExp e state =
    case e of
        CONST_ELAB c -> irConst c state
        IDENTIFIER_ELAB i -> irIdentifier i state
        BINOP_ELAB b -> irBinop b state
        UNOP_ELAB u -> irUnop u state

irConst :: Const -> State -> ([CommandIr], Maybe (PureIr, TypeCategory), State, [VerificationError])
irConst const state = ([], Just (CONST_IR const, constToType const), state, [])

irIdentifier :: Token -> State -> ([CommandIr], Maybe (PureIr, TypeCategory), State, [VerificationError])
irIdentifier tok state =
    case identifierLookup tok (scopes state) of
        Just (varElab, varScopeId) ->
            let varIr = varElabToIr varElab varScopeId
                varTypeCat = (typeElabType (variableElabType varElab))
             in ([], Just (VAR_IR varIr, varTypeCat), state, [])
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
                            let (unopComms, unopPu) = unopOpTranslate cat expPu
                             in (unopComms ++ expComms, Just (unopPu, infType), expState, expErrs)
                        -- fail when type for unop cannot be inferred
                        Nothing ->
                            ([], Nothing, state, (OP_TYPE_MISMATCH (OpTypeMismatch op [expTy])) : expErrs)
            -- immediately fail if exp is invalid
            _ ->
                ([], Nothing, state, expErrs)

-- MANAGE AND ACCESS ELABORATED STATE

data State = State
    { scopes :: [Scope]
    , regCtr :: Int
    , mapCtr :: Int
    }

data Scope = Scope
    { scopeMap :: Map.Map String VariableElab
    , scopeId :: Int
    }

addTemp :: State -> (String, State)
addTemp state =
    let tempName = (show (regCtr state)) ++ ":temp"
     in (tempName, State (scopes state) ((regCtr state) + 1) (mapCtr state))

addScope :: State -> State
addScope state =
    let newScope = Scope Map.empty (mapCtr state)
     in State (newScope : (scopes state)) (regCtr state) ((mapCtr state) + 1)

insertToTopScope :: State -> String -> VariableElab -> State
insertToTopScope state name varElab =
    let topScope = head (scopes state)
        varScope = Scope (Map.insert name varElab (scopeMap topScope)) (scopeId topScope)
     in State (varScope : (tail (scopes state))) (regCtr state) (mapCtr state)

popScopeMap :: State -> State
popScopeMap state = State (tail (scopes state)) (regCtr state) (mapCtr state)

identifierLookup :: Token -> [Scope] -> Maybe (VariableElab, Int)
identifierLookup tok scopes =
    case scopes of
        [] -> Nothing
        scope : _ ->
            case (Map.lookup (extractIdentifierName tok) (scopeMap scope)) of
                Just var -> Just (var, (scopeId scope))
                Nothing -> identifierLookup tok (tail scopes)

varElabToIr :: VariableElab -> Int -> VariableIr
varElabToIr varElab varScopeId =
    VariableIr
        ((show varScopeId) ++ ":" ++ (extractIdentifierName (variableElabIdentifier varElab)))
        (typeElabType (variableElabType varElab))
        False

--  BB ARGUMENT EXTRACTION

-- merges all scopes (from right to left to prioritize higher scopes)
-- maps all varElabs (with corresponding scope id) to varIr
getIrArgs :: [Scope] -> [VariableIr]
getIrArgs scopes =
    let singleMap = foldr getIrArgsFoldFn Map.empty scopes
     in map getIrArgsTranslateFn (Map.toList singleMap)

-- iterates through all elements in SCOPE and inserts (with scope id) into INIT_MAP
getIrArgsFoldFn :: Scope -> Map.Map String (Int, VariableElab) -> Map.Map String (Int, VariableElab)
getIrArgsFoldFn scope initMap =
    let currScopeId = (scopeId scope)
     in foldr (\(s, v) interMap -> Map.insert s (currScopeId, v) interMap) initMap (Map.toList (scopeMap scope))

getIrArgsTranslateFn :: (String, (Int, VariableElab)) -> VariableIr
getIrArgsTranslateFn (_, (varScopeId, varElab)) = varElabToIr varElab varScopeId

-- OP HELPERS

binopTypeInf :: BinopCatElab -> TypeCategory -> TypeCategory -> Maybe TypeCategory
binopTypeInf cat t1 t2 =
    case (t1, t2) of
        (INT_TYPE, INT_TYPE) -> Just INT_TYPE
        _ -> Nothing

binopOpTranslate :: BinopCatElab -> TypeCategory -> PureIr -> PureIr -> State -> ([CommandIr], PureIr, State)
binopOpTranslate cat ty p1 p2 state =
    case cat of
        ADD_EXP_ELAB -> ([], PURE_BINOP_IR (ADD_IR p1 p2), state)
        SUB_EXP_ELAB -> ([], PURE_BINOP_IR (SUB_IR p1 p2), state)
        MUL_EXP_ELAB -> ([], PURE_BINOP_IR (MUL_IR p1 p2), state)
        DIV_EXP_ELAB ->
            let (tempName, newState) = addTemp state
                temp = VariableIr tempName ty True
                binop = DIV_IR p1 p2
                comm = ASN_IMPURE_BINOP_IR temp binop
             in ([comm], VAR_IR temp, newState)
        MOD_EXP_ELAB ->
            let (tempName, newState) = addTemp state
                temp = VariableIr tempName ty True
                binop = MOD_IR p1 p2
                comm = ASN_IMPURE_BINOP_IR temp binop
             in ([comm], VAR_IR temp, newState)

unopTypeInf :: UnopCatElab -> TypeCategory -> Maybe TypeCategory
unopTypeInf cat t1 =
    case t1 of
        INT_TYPE -> Just INT_TYPE
        _ -> Nothing

unopOpTranslate :: UnopCatElab -> PureIr -> ([CommandIr], PureIr)
unopOpTranslate cat p1 =
    case cat of
        NEG_EXP_ELAB -> ([], PURE_UNOP_IR (NEG_IR p1))
