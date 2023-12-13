module Ir (

)

where

import Elaborated
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
    irSeq fn (functionElabBlock fn) (BasicBlockIr [] []) initScope

irSeq :: FunctionElab -> SeqElab -> BasicBlockIr -> Scope -> [VerificationError] -> ([BasicBlockIr], [VerificationError])
irSeq fn ss bb errs scope = 
    case ss of
        -- i. terminate current basic block
        [] -> ([bb], [])
        RET_ELAB r : _ -> 
            let (retComms, retErrs) = irRet fn r scope
                bbRet = BasicBlockIr (bbIrArgs bb) (retComms ++ (bbIrCommands bb))
            in ([bbRet], retErrs + errs)

        -- ii. extend current basic block
        DECL_ELAB d : _ -> 
            let (declComms, declScope, declErrs) = irDecl d scope
                bbDecl = BasicBlockIr (bbIrArgs bb) (declComms ++ (bbIrCommands bb))
            in irSeq (tail ss) bbDecl (declErrs ++ errs) declScope
        ASN_ELAB a : _ -> 
            let (asnComms, asnErrs) = irAsn a scope
                bbAsn = BasicBlockIr (bbIrArgs bb) (asnComms ++ (bbIrCommands bb))
            in irSeq (tail ss) bbAsn (asnErrs ++ errs) scope
        
            
irStmt :: StatementElab -> Scope -> (Bool, [BasicBlockIr], [CommandIr], Scope, [VerificationError])
irStmt s scope = 
    case s of
        DECL_ELAB d -> 
            let (comm, m_ty, ret, sco, err) = irDecl d

            irDecl d scope
        ASN_ELAB a -> irAsn a scope
        RET_ELAB r -> irRet r scope

        SEQ_ELAB ss -> 
            let seqScopeMaps = (Map.Empty):(scopeMaps scope)
                (bbs, ret, _, errs) = irSeq ss seqScopeMaps
            in 

irDecl :: DeclElab -> Scope -> ([CommandIr], Scope, [VerificationError])
irDecl (DeclElab varElab Nothing) scope = 
    let name = extractIdentifierName (variableElabIdentifier varElab)
        ty = typeElabType (variableElabType varElab)
        varScopeMap = Map.insert name varElab (head (scopeMaps scope))
        varScope = Scope (varScopeMap:(tail (scopeMaps scope))) (scopeRegCtr scope)
    case Map.lookup (head (scopeMaps scope)) of
        Just prevVarElab ->
            ([], Nothing, False, varScope,
                [(DOUBLE_DECL DoubleDeclarationError (variableElabIdentifier prevVarElab) (variableElabIdentifier varElab))])
        Nothing -> 
            let varScopeMap = Map.insert name varElab (head (scopeMaps scope))
                varScope = Scope (varScopeMap:(tail (scopeMaps scope))) (scopeRegCtr scope)
                var = VariableIr name ty
                comm = INIT_IR var
            in ([comm], ty, False, varScope, [])
irDecl (DeclElab varElab (Just asn)) scope = 
    let (comm1, m_ty1, ret1, sco1, err1) = irDecl (DeclElab varElab Nothing)
        (comm2, m_ty2, ret2, sco2, err2) = irAsn asn sco1
    in case (m_ty1, m_ty2) of
        (Just ty1, Just ty2) ->
            (comm2 ++ comm1, Just ty1, False, sco2, err2 ++ err1)
        _ ->
            ([], Nothing, False, scope, err2 ++ err1)

irAsn :: AsnElab -> Scope -> ([CommandIr], [VerificationError])
irAsn (AsnElab tok e) scope = 
    let (comm1, m_pt1, sco1, err1) = irExp e scope
        m_var = identifierLookup tok scope
    in case m_var of
        -- check that varElab is declared
        Just varElab ->
            case m_pt1 of
                Just (pu1, ty1) ->
                    -- check that var and exp have the same type
                    let varTy = (typeElabType (variableElabType varElab))
                    in if varTy == ty1
                        then (comm1, ty1, False, sco1, err1)
                        else ([], Nothing, False, scope, (ASN_TYPE_MISMATCH (AsnTypeMismatch tok varTy ty1)))
                Nothing ->
                    ([], Nothing, False, scope, err1)
        -- fail if varElab is not declared
        Nothing ->
            ([], Nothing, False, scope, (USE_BEFORE_DECL (UseBeforeDeclarationError tok)):err1)

irRet :: RetElab -> FunctionElab -> Scope -> ([CommandIr], [VerificationError])
irRet (RetElab e) (FunctionElab _ retTy _) scope = 
    let (comm1, m_pt1, sco1, err1) = irExp e scope
    in case m_pt1 of
        Just (pu1, ty1) ->
            if (retTy == ty1)
                then
                    let comm2 = RET_PURE_IR pu1
                    in (comm2:comm1, ty1, True, sco1, err1)
                else
                    ([], Nothing, False, scope, [(RET_TYPE_MISMATCH (RetTypeMismatch retTy ty1))])
        Nothing ->
            ([], Nothing, False, scope, err1)

irExp :: ExpElab -> Scope -> ([CommandIr], Maybe (PureIr, TypeCategory), Scope, [VerificationError])
irExp e scope = 
    case e of
        CONST_ELAB c -> irConst c scope
        IDENTIFIER_ELAB i -> irIdentifier i scope
        BINOP_ELAB b -> irBinop b scope
        UNOP_ELAB u -> irUnop u scope

irConst :: Const -> Scope -> ([CommandIr], Maybe (PureIr, TypeCategory), Scope, [VerificationError])
irConst const scope = ([], Just (CONST_IR const), Just (constToType const), [])

irIdentifier :: Token -> Scope -> ([CommandIr], Maybe (PureIr, TypeCategory), Scope, [VerificationError])
irIdentifier tok scope = 
    case identifierLookup tok scope of
        Just varElab -> 
            let varName = (extractIdentifierName (variableElabIdentifier varElab))
                varTypeCat = (typeElabType (variableElabType varElab))
                varIr = VariableIr varName varTypeCat False
            in ([], Just (VAR_IR varIr, varTypeCat), [])
        Nothing ->
            ([], Nothing, [(USE_BEFORE_DECL (UseBeforeDeclarationError tok))])

irBinop :: BinopElab -> Scope -> ([CommandIr], Maybe (PureIr, TypeCategory), Scope, [VerificationError])
irBinop (BinopElab cat op e1 e2) scope = 
    let (comm1, m_pt1, err1) = irExp e1 scope
        (comm2, m_pt2, err2) = irExp e2 scope
    in case (m_pt1, m_pt2) of
        -- type check when we have 2 valid exps
        (Just pt1, Just pt2) -> 
            let (pu1, ty1) = pt1
                (pu2, ty2) = pt2
                m_infType = binopTypeInf cat ty1 ty2
            in case m_infType of
                -- return pure exp with inferred type
                Just ty3 ->
                    let (comm3, pu3) = binopOpTranslate cat pu1 pu2
                    in (comm3 ++ comm2 ++ comm1, Just (pu3, ty3), err2 ++ err1)
                -- fail when type for binop cannot be inferred
                Nothing ->
                    let err = TypeMismatchError op (BINOP_CAT_ELAB cat) [t1, t2]
                    in ([], Nothing, err:(err2 ++ err1))
        -- immediately fail if either exp is invalid
        _ -> 
            ([], Nothing, err2 ++ err1)

irUnop :: UnopElab -> Scope -> ([CommandIr], Maybe (PureIr, TypeCategory), Scope, [VerificationError])
irUnop (UnopElab cat op e) scope = 
    let (comm1, m_pt1, err1) = irExp e scope
    in case m_pt1 of
        -- type check when the exp is valid
        Just pt1 ->
            let (pu1, ty1) = pt1
                m_infType = unopTypeInf cat ty1
            in case m_infType of
                -- return pure exp with inferred type
                Just ty2 ->
                    let (comm2, pu2) = unopOpTranslate cat pu1
                    in (comm2 ++ comm1, Just (pu2, ty2), err1)
                -- fail when type for unop cannot be inferred
                Nothing ->
                    let err = TypeMismatchError op (UNOP_CAT_ELAB cat) [ty1]
                    in ([], Nothing, err:err1)
        -- immediately fail if exp is invalid
        _ ->
            ([], Nothing, err1)

-- HELPERS

identifierLookup :: Token -> [Map.Map String VariableElab] -> Maybe VariableElab
identifierLookup tok maps = 
    case scope of
        [] -> Nothing
        scope : _ -> 
            case (Map.lookup (extractIdentifierName tok) (head maps)) of
                Just var -> var
                Nothing -> identifierLookup tok (tail maps)

binopTypeInf :: BinopElabCat -> TypeCategory -> TypeCategory -> Maybe TypeCategory
binopTypeInf cat t1 t2 = 
    case (t1, t2) of
        (INT_TYPE, INT_TYPE) -> Just INT_TYPE
        _ -> Nothing

binopOpTranslate :: BinopElabCat -> TypeCategory -> PureIr -> PureIr -> Int -> ([CommandIr] PureIr, Int)
binopOpTranslate cat ty p1 p2 regCtr = 
    case cat of
        ADD_EXP_ELAB -> ([], PURE_BINOP_IR (PureBinopIr (ADD_IR p1 p2)), regCtr)
        SUB_EXP_ELAB -> ([], PURE_BINOP_IR (PureBinopIr (SUB_IR p1 p2)), regCtr)
        MUL_EXP_ELAB -> ([], PURE_BINOP_IR (PureBinopIr (MUL_IR p1 p2)), regCtr)
        DIV_EXP_ELAB -> 
            let temp = VariableIr (show regCtr) ty True
                binop = DIV_IR p1 p2
                comm = ASN_IMPURE_BINOP_IR temp binop
            in ([comm], VAR_IR temp, regCtr + 1)
        MOD_EXP_ELAB -> 
            let temp = VariableIr (show regCtr) ty True
                binop = MOD_IR p1 p2
                comm = ASN_IMPURE_BINOP_IR temp binop
            in ([comm], VAR_IR temp, regCtr + 1)

unopTypeInf :: UnopElabCat -> TypeCategory -> Maybe TypeCategory
unopTypeInf cat t1 = 
    case t1 of
        INT_TYPE -> Just INT_TYPE
        _ -> Nothing

unopOpTranslate :: UnopElabCat -> PureIr -> ([CommandIr], Maybe PureIr)
unopOpTranslate cat p1 = 
    case cat of
        NEG_EXP_ELAB -> Just ([], PURE_UNOP_IR (PureUnopIr (NEG_IR p1)))


-- EXPRESSIONS IR

translateIrExp :: ExpElab -> ([CommandIr], PureIr)
translateIrExp e =
    case e of
        CONST_ELAB const -> translateIrConst const
        IDENTIFIER_ELAB identTok -> translateIrIdentifier identTok
        BINOP_ELAB binop -> translateIrBinop binop
        UNOP_ELAB unop -> translateIrUnop unop

translateIrConst :: Const -> ([CommandIr], PureIr)
translateIrConst const = ([], CONST_IR const)

translateIrIdentifier :: Token -> ([CommandIr], PureIr)
translateIrIdentifier identTok = ([], )
    let id = extractIdentifierName identTok
     in case (Map.lookup name (regMap rData)) of
            Just i -> ([], VAR_IR i, rData)
            Nothing ->
                let var = (regCtr rData)
                    newRegCtr = (regCtr rData) + 1
                    newRegMap = (Map.insert name (regCtr rData))
                 in ([], VAR_IR var, RegData newRegCtr newRegMap)

translateIrBinop :: BinopElab -> ([CommandIr], PureIr)
translateIrBinop binop rData =
    case binop of
        ADD_EXP_ELAB e1 e2 ->
            let (c1, p1, p2, r1) = generateAndConcatSubCommands e1 e2 rData
                p = PURE_BINOP_IR (PureBinopIr ADD_IR p1 p2)
             in (c1, p, r1)
        SUB_EXP_ELAB e1 e2 ->
            let (c1, p1, p2, r1) = generateAndConcatSubCommands e1 e2 rData
                p = PURE_BINOP_IR (PureBinopIr SUB_IR p1 p2)
             in (c1, p, r1)
        MUL_EXP_ELAB e1 e2 ->
            let (c1, p1, p2, r1) = generateAndConcatSubCommands e1 e2 rData
                p = PURE_BINOP_IR (PureBinopIr MUL_IR p1 p2)
             in (c1, p, r1)
        DIV_EXP_ELAB e1 e2 ->
            let (c1, p1, p2, r1) = generateAndConcatSubCommands e1 e2 rData
                de = DIV_IR p1 p2
                v = (regCtr rData)
                c2 = (ASN_IMPURE_BINOP (v de)) : c1
             in (c2, VAR_IR v, RegData ((regCtr rData) + 1) (regMap rData))
        MOD_EXP_ELAB e1 e2 ->
            let (c1, p1, p2, r1) = generateAndConcatSubCommands e1 e2 rData
                me = MOD_IR p1 p2
                v = (regCtr rData)
                c2 = (ASN_IMPURE_BINOP (v me)) : c1
             in (c2, VAR_IR v, RegData ((regCtr rData) + 1) (regMap rData))

translateIrUnop :: UnopElab -> ([CommandIr], PureIr)
translateIrUnop unop rData =
    case unop of
        NEG_EXP_ELAB e ->
            let (c1, p1, r1) = translateIrExp e rData
                p = PURE_UNOP_IR (PureUnopIr NEG_IR p1)
             in (c1, p, r1)

-- HELPERS

generateAndConcatSubCommands :: ExpElab -> ExpElab -> -> ([CommandIr], PureIr)
generateAndConcatSubCommands e1 e2 rData =
    let (c1, p1, r1) = translateIrExp e1 rData
        (c2, p2, r2) = translateIrExp e2 r1
     in (c2 ++ c1, p1, p2, r2)
