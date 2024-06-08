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
    FunctionElab
        (elaborateFnSignature . functionSignature $ fn)
        (elaborateStmts . functionBlock $ fn)

elaborateFnSignature :: FunctionSignature -> FunctionSignatureElab
elaborateFnSignature fnSign = 
    FunctionSignatureElab
        (functionSignatureName fnSign)
        (map elaborateParam (functionSignatureArgs fnSign))
        (elaborateType . functionSignatureRetType $ fnSign)

elaborateParam :: Param -> ParamElab
elaborateParam param = 
    ParamElab
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

elaborateControl :: Control -> StatementElab
elaborateControl c = 
    case c of
        RET_CTRL e -> RET_ELAB (RetElab (elaborateExp e))
        IF_CTRL i -> IF_ELAB (elaborateIf i)
        WHILE_CTRL w -> WHILE_ELAB (elaborateWhile w)
        FOR_CTRL f -> SEQ_ELAB (elaborateFor f)

elaborateBlock :: Block -> StatementElab
elaborateBlock b = SEQ_ELAB (elaborateStmts b)

-- individual statement elaboration

elaborateStmts :: Statements -> SeqElab
elaborateStmts ss = map elaborateStmt ss

elaborateAsn :: Asn -> AsnElab
elaborateAsn (Asn as (Lval id) e) =
    case (tokenCat as) of
        EQUAL -> AsnElab id (elaborateExp e)
        _ -> AsnElab id (elaborateExp (BINOP_EXP (Binop (decomposeAsnOp as) (IDENTIFIER_EXP id) e)))

elaborateDecl :: Decl -> DeclElab
elaborateDecl decl =
    case decl of
        Decl id ty Nothing Nothing ->
            DeclElab (VariableElab id (elaborateType ty)) Nothing
        Decl id ty (Just as) (Just e) ->
            DeclElab (VariableElab id (elaborateType ty)) (Just (elaborateAsn (Asn as (Lval id) e)))

elaboratePost :: Post -> AsnElab
elaboratePost (Post op (Lval id)) = 
    AsnElab id 
        (elaborateExp (BINOP_EXP (Binop 
            (decomposeAsnOp op) 
            (IDENTIFIER_EXP id) 
            (DECNUM_EXP (wrapConstExp "1" op))
        )))

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

elaborateExp :: Exp -> ExpElab
elaborateExp e =
    case e of
        HEXNUM_EXP h -> CONST_ELAB (elaborateConst h)
        DECNUM_EXP d -> CONST_ELAB (elaborateConst d)
        BOOL_EXP b -> CONST_ELAB (elaborateConst b)
        IDENTIFIER_EXP id -> IDENTIFIER_ELAB id
        TERN_EXP t -> TERN_ELAB (elaborateTernop t)
        BINOP_EXP b -> 
            case (elaborateBinop b) of
                Left be -> BINOP_ELAB be
                Right lbe -> LOG_BINOP_ELAB lbe
        UNOP_EXP u -> UNOP_ELAB (elaborateUnop u)
        FN_CALL_EXP f -> FN_CALL_ELAB (elaborateFunctionCall f)

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
    case (tokenCat . typeToken $ ty) of
        TYPE typeCat -> TypeElab typeCat (typeToken ty)
        _ -> error . compilerError $ "Attempted to elaborate non-type token as type: " ++ (show . typeToken $ ty)
    
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
