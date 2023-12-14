module Ir (
    irFunction,
)
where

import Elaborated
import Errors
import IrTree
import Tokens
import Types

import qualified Data.Map as Map

-- IR TRANSLATION
-- + DECLARATION VERIFICATION
-- + TYPE VERIFICATION

irFunction :: FunctionElab -> ([BasicBlockIr], [VerificationError])
irFunction fn =
    let initBb = BasicBlockIr [] []
        initScope = Scope [Map.empty] 0
     in irSeq fn (functionElabBlock fn) (BasicBlockIr [] []) initScope []

irSeq :: FunctionElab -> SeqElab -> BasicBlockIr -> Scope -> [VerificationError] -> ([BasicBlockIr], [VerificationError])
irSeq fn ss bb scope errs =
    case ss of
        -- i. terminate current basic block
        [] -> ([bb], errs)
        RET_ELAB r : _ ->
            let (retComms, _, retErrs) = irRet r fn scope
                bbRet = BasicBlockIr (bbIrArgs bb) (retComms ++ (bbIrCommands bb))
             in ([bbRet], retErrs ++ errs)
        
        -- ii. extend current basic block
        DECL_ELAB d : _ ->
            let (declComms, declScope, declErrs) = irDecl d scope
                bbDecl = BasicBlockIr (bbIrArgs bb) (declComms ++ (bbIrCommands bb))
             in irSeq fn (tail ss) bbDecl declScope (declErrs ++ errs)
        ASN_ELAB a : _ ->
            let (asnComms, asnScope, asnErrs) = irAsn a scope
                bbAsn = BasicBlockIr (bbIrArgs bb) (asnComms ++ (bbIrCommands bb))
             in irSeq fn (tail ss) bbAsn asnScope (asnErrs ++ errs)

        -- iii. start new basic block
        SEQ_ELAB s : _ ->
            let currBbArgs = getIrArgs (scopeMaps scope)

                -- evaluate for inner block
                innerBb = BasicBlockIr currBbArgs []
                innerScope = Scope (Map.empty:(scopeMaps scope)) (scopeRegCtr scope)
                (innerSeqBb, innerSeqErrs) = irSeq fn s innerBb innerScope []

                -- evaluate for remainder of block
                nextBb = BasicBlockIr currBbArgs []
                (nextSeqBb, nextSeqErrs) = irSeq fn (tail ss) nextBb scope []

            -- concat results from existing bb, inner bb(s), and remaining bb(s)
            in (nextSeqBb ++ innerSeqBb ++ [bb], nextSeqErrs ++ innerSeqErrs ++ errs)

irDecl :: DeclElab -> Scope -> ([CommandIr], Scope, [VerificationError])
irDecl (DeclElab varElab Nothing) scope =
    let name = extractIdentifierName (variableElabIdentifier varElab)
        ty = typeElabType (variableElabType varElab)
        varScopeMap = Map.insert name varElab (head (scopeMaps scope))
        varScope = Scope (varScopeMap : (tail (scopeMaps scope))) (scopeRegCtr scope)
     in case Map.lookup name (head (scopeMaps scope)) of
            Just prevVarElab ->
                let err = (DOUBLE_DECL (DoubleDeclarationError (variableElabIdentifier prevVarElab) (variableElabIdentifier varElab)))
                 in ([], varScope, [err])
            Nothing ->
                let varScopeMap = Map.insert name varElab (head (scopeMaps scope))
                    varScope = Scope (varScopeMap : (tail (scopeMaps scope))) (scopeRegCtr scope)
                    var = VariableIr name ty False
                    comm = INIT_IR var
                 in ([comm], varScope, [])
irDecl (DeclElab varElab (Just asn)) scope =
    let (comm1, sco1, err1) = irDecl (DeclElab varElab Nothing) scope
        (comm2, sco2, err2) = irAsn asn sco1
     in (comm2 ++ comm1, sco2, err2 ++ err1)

irAsn :: AsnElab -> Scope -> ([CommandIr], Scope, [VerificationError])
irAsn (AsnElab tok e) scope =
    let (comm1, m_pt1, sco1, err1) = irExp e scope
        m_var = identifierLookup tok (scopeMaps scope)
     in case m_var of
            -- check that varElab is declared
            Just varElab ->
                case m_pt1 of
                    Just (pu1, ty1) ->
                        -- check that var and exp have the same type
                        let varTy = (typeElabType (variableElabType varElab))
                         in if varTy == ty1
                                then (comm1, sco1, err1)
                                else ([], scope, (ASN_TYPE_MISMATCH (AsnTypeMismatch tok varTy ty1)) : err1)
                    Nothing ->
                        ([], scope, err1)
            -- fail if varElab is not declared
            Nothing ->
                ([], scope, (USE_BEFORE_DECL (UseBeforeDeclarationError tok)) : err1)

irRet :: RetElab -> FunctionElab -> Scope -> ([CommandIr], Scope, [VerificationError])
irRet (RetElab e) (FunctionElab _ (TypeElab retTy _) _) scope =
    let (comm1, m_pt1, sco1, err1) = irExp e scope
     in case m_pt1 of
            Just (pu1, ty1) ->
                if (retTy == ty1)
                    then
                        let comm2 = RET_PURE_IR pu1
                         in (comm2 : comm1, sco1, err1)
                    else ([], scope, (RET_TYPE_MISMATCH (RetTypeMismatch retTy ty1)) : err1)
            Nothing ->
                ([], scope, err1)

irExp :: ExpElab -> Scope -> ([CommandIr], Maybe (PureIr, TypeCategory), Scope, [VerificationError])
irExp e scope =
    case e of
        CONST_ELAB c -> irConst c scope
        IDENTIFIER_ELAB i -> irIdentifier i scope
        BINOP_ELAB b -> irBinop b scope
        UNOP_ELAB u -> irUnop u scope

irConst :: Const -> Scope -> ([CommandIr], Maybe (PureIr, TypeCategory), Scope, [VerificationError])
irConst const scope = ([], Just (CONST_IR const, constToType const), scope, [])

irIdentifier :: Token -> Scope -> ([CommandIr], Maybe (PureIr, TypeCategory), Scope, [VerificationError])
irIdentifier tok scope =
    case identifierLookup tok (scopeMaps scope) of
        Just varElab ->
            let varName = (extractIdentifierName (variableElabIdentifier varElab))
                varTypeCat = (typeElabType (variableElabType varElab))
                varIr = VariableIr varName varTypeCat False
             in ([], Just (VAR_IR varIr, varTypeCat), scope, [])
        Nothing ->
            ([], Nothing, scope, [(USE_BEFORE_DECL (UseBeforeDeclarationError tok))])

irBinop :: BinopElab -> Scope -> ([CommandIr], Maybe (PureIr, TypeCategory), Scope, [VerificationError])
irBinop (BinopElab cat op e1 e2) scope =
    let (comm1, m_pt1, sco1, err1) = irExp e1 scope
        (comm2, m_pt2, sco2, err2) = irExp e2 sco1
     in case (m_pt1, m_pt2) of
            -- type check when we have 2 valid exps
            (Just pt1, Just pt2) ->
                let (pu1, ty1) = pt1
                    (pu2, ty2) = pt2
                    m_infType = binopTypeInf cat ty1 ty2
                 in case m_infType of
                        -- return pure exp with inferred type
                        Just ty3 ->
                            let (comm3, pu3, sco3) = binopOpTranslate cat ty3 pu1 pu2 sco2
                             in (comm3 ++ comm2 ++ comm1, Just (pu3, ty3), sco3, err2 ++ err1)
                        -- fail when type for binop cannot be inferred
                        Nothing ->
                            let err = OP_TYPE_MISMATCH (OpTypeMismatch op [ty1, ty2])
                             in ([], Nothing, scope, err : (err2 ++ err1))
            -- immediately fail if either exp is invalid
            _ ->
                ([], Nothing, scope, err2 ++ err1)

irUnop :: UnopElab -> Scope -> ([CommandIr], Maybe (PureIr, TypeCategory), Scope, [VerificationError])
irUnop (UnopElab cat op e) scope =
    let (comm1, m_pt1, sco1, err1) = irExp e scope
     in case m_pt1 of
            -- type check when the exp is valid
            Just pt1 ->
                let (pu1, ty1) = pt1
                    m_infType = unopTypeInf cat ty1
                 in case m_infType of
                        -- return pure exp with inferred type
                        Just ty2 ->
                            let (comm2, pu2) = unopOpTranslate cat pu1
                             in (comm2 ++ comm1, Just (pu2, ty2), sco1, err1)
                        -- fail when type for unop cannot be inferred
                        Nothing ->
                            let err = OP_TYPE_MISMATCH (OpTypeMismatch op [ty1])
                             in ([], Nothing, scope, err : err1)
            -- immediately fail if exp is invalid
            _ ->
                ([], Nothing, scope, err1)

-- HELPERS

data Scope = Scope
    { scopeMaps :: [Map.Map String VariableElab]
    , scopeRegCtr :: Int
    }

getIrArgs :: [Map.Map String VariableElab] -> [VariableIr]
getIrArgs maps = 
    let singleMap = foldr (\nextMap interMap -> (getIrArgsFoldFn interMap (Map.toList nextMap))) Map.empty maps
    in map getIrArgsTranslateFn (Map.toList singleMap)

getIrArgsFoldFn :: Map.Map String VariableElab -> [(String, VariableElab)] -> Map.Map String VariableElab
getIrArgsFoldFn initMap nextMap = 
    foldr (\(s, v) interMap -> Map.insert s v interMap) initMap nextMap

getIrArgsTranslateFn :: (String, VariableElab) -> VariableIr
getIrArgsTranslateFn (varName, varElab) = 
    VariableIr varName (typeElabType (variableElabType varElab)) False

identifierLookup :: Token -> [Map.Map String VariableElab] -> Maybe VariableElab
identifierLookup tok maps =
    case maps of
        [] -> Nothing
        _ ->
            case (Map.lookup (extractIdentifierName tok) (head maps)) of
                Just var -> Just var
                Nothing -> identifierLookup tok (tail maps)

binopTypeInf :: BinopCatElab -> TypeCategory -> TypeCategory -> Maybe TypeCategory
binopTypeInf cat t1 t2 =
    case (t1, t2) of
        (INT_TYPE, INT_TYPE) -> Just INT_TYPE
        _ -> Nothing

binopOpTranslate :: BinopCatElab -> TypeCategory -> PureIr -> PureIr -> Scope -> ([CommandIr], PureIr, Scope)
binopOpTranslate cat ty p1 p2 scope =
    case cat of
        ADD_EXP_ELAB -> ([], PURE_BINOP_IR (ADD_IR p1 p2), scope)
        SUB_EXP_ELAB -> ([], PURE_BINOP_IR (SUB_IR p1 p2), scope)
        MUL_EXP_ELAB -> ([], PURE_BINOP_IR (MUL_IR p1 p2), scope)
        DIV_EXP_ELAB ->
            let temp = VariableIr (show (scopeRegCtr scope)) ty True
                binop = DIV_IR p1 p2
                comm = ASN_IMPURE_BINOP_IR temp binop
             in ([comm], VAR_IR temp, Scope (scopeMaps scope) ((scopeRegCtr scope) + 1))
        MOD_EXP_ELAB ->
            let temp = VariableIr (show (scopeRegCtr scope)) ty True
                binop = MOD_IR p1 p2
                comm = ASN_IMPURE_BINOP_IR temp binop
             in ([comm], VAR_IR temp, Scope (scopeMaps scope) ((scopeRegCtr scope) + 1))

unopTypeInf :: UnopCatElab -> TypeCategory -> Maybe TypeCategory
unopTypeInf cat t1 =
    case t1 of
        INT_TYPE -> Just INT_TYPE
        _ -> Nothing

unopOpTranslate :: UnopCatElab -> PureIr -> ([CommandIr], PureIr)
unopOpTranslate cat p1 =
    case cat of
        NEG_EXP_ELAB -> ([], PURE_UNOP_IR (NEG_IR p1))
