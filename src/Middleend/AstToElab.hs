module Middleend.AstToElab (
    elaborateProg,
) where

import Model.Ast
import Model.Elaborated
import Common.Errors
import Model.Tokens
import Model.Types

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import qualified Numeric

{-

ELABORATION

-}

elaborateProg :: Program -> ProgramElab
elaborateProg prog = Maybe.mapMaybe elaborateGDecl prog

elaborateGDecl :: GlobalDecl -> Maybe GlobalDeclElab
elaborateGDecl gdecl = 
    case gdecl of
        TYPEDEF_GDECL td -> Nothing
        FNDECL_GDECL fndecl -> Just (FNDECL_GDECL_ELAB (elaborateFnSignature fndecl))
        FNDEFN_GDECL fndefn -> Just (FNDEFN_GDECL_ELAB (elaborateFn fndefn))

elaborateFn :: Function -> FunctionElab
elaborateFn fn =
    let fnSignElab = elaborateFnSignature . functionSignature $ fn
        fnRetTy = typeElabType . functionSignatureElabRetType $ fnSignElab
        stmtsElab = elaborateStmts . functionBlock $ fn
        terminatedStmtsElab = 
            if fnRetTy == VOID_TYPE
                then stmtsElab ++ [RET_ELAB (RetElab Nothing)]
                else stmtsElab
    in  
        FunctionElab
            fnSignElab
            terminatedStmtsElab

elaborateFnSignature :: FunctionSignature -> FunctionSignatureElab
elaborateFnSignature fnSign = 
    FunctionSignatureElab
        (functionSignatureName fnSign)
        (map elaborateParam (functionSignatureArgs fnSign))
        (elaborateType . functionSignatureRetType $ fnSign)

elaborateParam :: Param -> VariableElab
elaborateParam param = 
    VariableElab
        (paramIdentifier param)
        (elaborateType . paramType $ param)

elaborateStmt :: Statement -> StatementElab
elaborateStmt st = 
    case st of
        SIMP_STMT s -> elaborateSimp s
        CONTROL_STMT c -> elaborateControl c
        BLOCK_STMT b -> elaborateBlock b

elaborateSimp :: Simp -> StatementElab
elaborateSimp s = 
    case s of
        ASN_SIMP a -> ASN_ELAB (elaborateAsn a)
        DECL_SIMP d -> DECL_ELAB (elaborateDecl d)
        POST_SIMP p -> ASN_ELAB (elaboratePost p)
        EXP_SIMP e -> EXP_ELAB (elaborateExp e)
        ASSERT_SIMP a -> IF_ELAB (elaborateAssert a)

elaborateControl :: Control -> StatementElab
elaborateControl c = 
    case c of
        RET_CTRL m_e -> 
            case m_e of
                Just e -> RET_ELAB (RetElab (Just (elaborateExp e)))
                Nothing -> RET_ELAB (RetElab Nothing)
        IF_CTRL i -> IF_ELAB (elaborateIf i)
        WHILE_CTRL w -> WHILE_ELAB (elaborateWhile w)
        FOR_CTRL f -> SEQ_ELAB (elaborateFor f)

elaborateBlock :: Block -> StatementElab
elaborateBlock b = SEQ_ELAB (elaborateStmts b)

-- individual statement elaboration

elaborateStmts :: Statements -> SeqElab
elaborateStmts ss = map elaborateStmt ss

elaborateAsn :: Asn -> AsnElab
elaborateAsn (Asn as lval e) =
    case (tokenCat as) of
        EQUAL -> 
            AsnElab 
                (elaborateLval lval)
                (elaborateExp e)
        _ -> 
            let opTok = decomposeAsnOp as
                binop = 
                    case (translateBinop . tokenCat $ opTok) of
                        Left bop -> 
                            (BINOP_ELAB
                                (BinopElab
                                    bop
                                    opTok
                                    REF_LVAL_EXP_ELAB
                                    (elaborateExp e)))
                        Right lbop ->
                            (LOG_BINOP_ELAB
                                (LogBinopElab
                                    lbop
                                    opTok
                                    REF_LVAL_EXP_ELAB
                                    (elaborateExp e)))
            in 
                AsnElab 
                    (elaborateLval lval)
                    binop

elaborateDecl :: Decl -> DeclElab
elaborateDecl decl =
    case decl of
        Decl id ty Nothing Nothing ->
            DeclElab (VariableElab id (elaborateType ty)) Nothing
        Decl id ty (Just as) (Just e) ->
            DeclElab (VariableElab id (elaborateType ty)) (Just (elaborateAsn (Asn as (BASE_GEN_IDENT id) e)))

elaboratePost :: Post -> AsnElab
elaboratePost (Post po lval) = 
    let opTok = decomposeAsnOp po
        binop = 
            case (translateBinop . tokenCat $ opTok) of
                Left bop -> 
                    (BINOP_ELAB
                        (BinopElab
                            bop
                            opTok
                            REF_LVAL_EXP_ELAB
                            (CONST_ELAB (INT_CONST 1))))
                Right lbop ->
                    (LOG_BINOP_ELAB
                        (LogBinopElab
                            lbop
                            opTok
                            REF_LVAL_EXP_ELAB
                            (CONST_ELAB (INT_CONST 1))))
    in 
        AsnElab 
            (elaborateLval lval)
            binop

elaborateAssert :: Assert -> IfElab
elaborateAssert (Assert assertTok e) = 
    let assertCondExp = 
            UNOP_ELAB
                (UnopElab
                    LOGNOT_EXP_ELAB
                    (Token
                        EXCL
                        (tokenData assertTok))
                    (elaborateExp e))
    in
        IfElab
            assertCondExp
            (ABORT_ELAB (AbortElab assertTok))
            Nothing

elaborateIf :: If -> IfElab
elaborateIf ifAst = 
    IfElab
        (elaborateExp (ifExp ifAst))
        (elaborateStmt (ifStmt ifAst))
        (fmap elaborateStmt (ifElseoptStmt ifAst))

elaborateWhile :: While -> WhileElab
elaborateWhile while = 
    WhileElab 
        (elaborateExp (whileExp while))
        (elaborateStmt (whileStmt while))

elaborateFor :: For -> SeqElab
elaborateFor for = 
    let initStmtElabs = 
            case (forInitSimp for) of
                Nothing -> []
                Just s -> [elaborateSimp s]
        termExpElab = elaborateExp (forTermExp for)
        innerStmtElab = 
            let forStmtElabs = [elaborateStmt (forStmt for)]
                forInterSimpElabs = 
                    case (forInterSimp for) of
                        Nothing -> []
                        Just s -> [elaborateSimp s]
            in SEQ_ELAB (forStmtElabs ++ forInterSimpElabs)
        whileStmtElab = WhileElab termExpElab innerStmtElab
    in initStmtElabs ++ [WHILE_ELAB whileStmtElab]

elaborateLval :: GenIdent -> LvalElab
elaborateLval lval = 
    let (id, ops) = generateLvalMemops lval
    in LvalElab id ops

elaborateExp :: Exp -> ExpElab
elaborateExp e =
    case e of
        HEXNUM_EXP h -> CONST_ELAB (elaborateConst h)
        DECNUM_EXP d -> CONST_ELAB (elaborateConst d)
        BOOL_EXP b -> CONST_ELAB (elaborateConst b)
        NULL_EXP n -> CONST_ELAB (elaborateConst n)
        TERN_EXP t -> TERN_ELAB (elaborateTernop t)
        BINOP_EXP b -> 
            case (elaborateBinop b) of
                Left be -> BINOP_ELAB be
                Right lbe -> LOG_BINOP_ELAB lbe
        UNOP_EXP u -> UNOP_ELAB (elaborateUnop u)
        FN_CALL_EXP f -> FN_CALL_ELAB (elaborateFunctionCall f)
        GEN_IDENT_EXP g ->
            let (baseTok, memops) = generateExpMemops g
            in 
                case memops of
                    [] -> IDENTIFIER_ELAB baseTok
                    _ -> MEMOP_ELAB (MemopElab baseTok memops)

elaborateTernop :: Ternop -> TernopElab
elaborateTernop (Ternop op eCond e1 e2) = 
    TernopElab op (elaborateExp eCond) (elaborateExp e1) (elaborateExp e2)

elaborateBinop :: Binop -> Either BinopElab LogBinopElab
elaborateBinop (Binop op e1 e2) =
    case (translateBinop (tokenCat op)) of
        Left bop -> Left (BinopElab bop op (elaborateExp e1) (elaborateExp e2))
        Right lbop -> Right (LogBinopElab lbop op (elaborateExp e1) (elaborateExp e2))

elaborateUnop :: Unop -> UnopElab
elaborateUnop (Unop op e) =
    UnopElab (translateUnop (tokenCat op)) op (elaborateExp e)

elaborateType :: Type -> TypeElab
elaborateType ty = 
    case ty of
        BASE_TYPE_AST tok ->
            case (tokenCat tok) of
                TYPE typeCat -> TypeElab typeCat tok
                _ -> error . compilerError $ "Attempted to elaborate non-type token as type: " ++ (show tok)
        POINTER_TYPE_AST t ->
            let subElaboratedType = elaborateType t
            in 
                TypeElab
                    (POINTER_TYPE (typeElabType subElaboratedType))
                    (typeElabToken subElaboratedType)
        ARRAY_TYPE_AST t tok ->
            case elaborateConst tok of
                INT_CONST i ->
                    if i > 0
                        then
                            let subElaboratedType = elaborateType t
                            in
                                TypeElab
                                    (ARRAY_TYPE (typeElabType subElaboratedType) i)
                                    (typeElabToken subElaboratedType)
                        else
                            error . compilerError $ "Array size must be positive integer: size=" ++ (show tok)
                _ ->
                    error . compilerError $ "Array size must be positive integer: size=" ++ (show tok)

    
elaborateConst :: Token -> Const
elaborateConst tok =
    case (tokenCat tok) of
        HEXNUM h ->
            case (Numeric.readHex h) of
                [(i, _)] -> INT_CONST i
        DECNUM d ->
            case (Numeric.readDec d) of
                [(i, _)] -> INT_CONST i
        TRUE -> BOOL_CONST True
        FALSE -> BOOL_CONST False
        NULL -> NULL_CONST

elaborateFunctionCall :: FunctionCall -> FunctionCallElab
elaborateFunctionCall fnCall = 
    FunctionCallElab (functionCallName fnCall) (map elaborateExp (functionCallArgs fnCall))

translateBinop :: TokenCategory -> Either BinopCatElab LogBinopCatElab
translateBinop cat = 
    case cat of
        PLUS -> Left ADD_EXP_ELAB
        DASH -> Left SUB_EXP_ELAB
        STAR -> Left MUL_EXP_ELAB
        SLASH -> Left DIV_EXP_ELAB
        PERC -> Left MOD_EXP_ELAB
        AMP -> Left AND_EXP_ELAB
        CARET -> Left XOR_EXP_ELAB
        PIPE -> Left OR_EXP_ELAB
        LEFT_LEFT -> Left SLA_EXP_ELAB
        RIGHT_RIGHT -> Left SRA_EXP_ELAB
        LEFT -> Left LT_EXP_ELAB
        RIGHT -> Left GT_EXP_ELAB
        LEFT_EQ -> Left LTE_EXP_ELAB
        RIGHT_EQ -> Left GTE_EXP_ELAB
        EQ_EQ -> Left EQ_EXP_ELAB
        EXCL_EQ -> Left NEQ_EXP_ELAB
        AMP_AMP -> Right LOGAND_EXP_ELAB
        PIPE_PIPE -> Right LOGOR_EXP_ELAB
        _ -> error . compilerError $ "Attempt to translate non-binop exp token."

translateUnop :: TokenCategory -> UnopCatElab
translateUnop cat = 
    case cat of
        DASH -> NEG_EXP_ELAB
        TILDE -> NOT_EXP_ELAB
        EXCL -> LOGNOT_EXP_ELAB
        STAR -> DEREF_EXP_ELAB
        AMP -> REF_EXP_ELAB
        _ -> error (compilerError "Attempt to translate non-unop exp token.")

decomposeAsnOp :: Token -> Token
decomposeAsnOp (Token tokenCat tokenData) = 
    case tokenCat of
        PLUS_EQ -> Token PLUS tokenData
        DASH_EQ -> Token DASH tokenData
        STAR_EQ -> Token STAR tokenData
        SLASH_EQ -> Token SLASH tokenData
        PERC_EQ -> Token PERC tokenData
        AMP_EQ -> Token AMP tokenData
        CARET_EQ -> Token CARET tokenData
        PIPE_EQ -> Token PIPE tokenData
        PLUS_PLUS -> Token PLUS tokenData
        DASH_DASH -> Token DASH tokenData
        LEFT_LEFT_EQ -> Token LEFT_LEFT tokenData
        RIGHT_RIGHT_EQ -> Token RIGHT_RIGHT tokenData
        _ -> error (compilerError "Attempt to decompose non-combination asn token.")

wrapConstExp :: String -> Token -> Token
wrapConstExp decInt op = Token (DECNUM decInt) (tokenData op)

generateLvalMemops :: GenIdent -> (Token, [MemopElabCat])
generateLvalMemops lval = 
    case lval of
        BASE_GEN_IDENT i -> (i, [])
        DEREF_GEN_IDENT innerLval -> 
            let (i, subOps) = generateLvalMemops innerLval
            in (i, subOps ++ [DEREF_MEMOP_ELAB])
        ARR_INDEX_GEN_IDENT innerLval indexExp ->
            let indexExpElab = elaborateExp indexExp
                (i, subOps) = generateLvalMemops innerLval
            in (i, subOps ++ [ARR_INDEX_MEMOP_ELAB indexExpElab])

generateExpMemops :: GenIdent -> (Token, [MemopElabCat])
generateExpMemops genIdent = 
    case genIdent of
        BASE_GEN_IDENT identTok ->
            (identTok, [])
        DEREF_GEN_IDENT innerGenIdent -> 
            let (e, subOps) = generateExpMemops innerGenIdent
            in (e, subOps ++ [DEREF_MEMOP_ELAB])
        ARR_INDEX_GEN_IDENT innerGenIdent indexExp ->
            let indexExpElab = elaborateExp indexExp
                (e, subOps) = generateExpMemops innerGenIdent
            in (e, subOps ++ [ARR_INDEX_MEMOP_ELAB indexExpElab])
