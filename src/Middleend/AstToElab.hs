module Middleend.AstToElab (
    elaborate,
) where

import Model.Ast
import Model.Elaborated
import Common.Errors
import Model.Tokens
import Model.Types

import qualified Data.Map as Map
import qualified Numeric

{-

ELABORATION

-}

-- TODO: update program after adding functions + multiple statements allowed per program
elaborate :: Function -> FunctionElab
elaborate fn =
    let seq = elaborateBlock (functionBlock fn)
     in FunctionElab (functionName fn) (elaborateType (functionReturnType fn)) seq

elaborateBlock :: Block -> SeqElab
elaborateBlock = elaborateStmts

elaborateStmts :: Statements -> SeqElab
elaborateStmts ss =
    case ss of
        [] -> []
        SIMP_STMT s : _ -> 
            case s of
                ASN_SIMP a -> (ASN_ELAB (elaborateAsn a)) : (elaborateStmts (tail ss))
                DECL_SIMP d -> (DECL_ELAB (elaborateDecl d)) : (elaborateStmts (tail ss))
                POST_SIMP p -> (ASN_ELAB (elaboratePost p)) : (elaborateStmts (tail ss))
                EXP_SIMP e -> (EXP_ELAB (elaborateExp e)) : (elaborateStmts (tail ss))
        CONTROL_STMT (RET_CTRL e) : _ -> (RET_ELAB (RetElab (elaborateExp e))) : (elaborateStmts (tail ss))
        BLOCK_STMT b : _ -> (SEQ_ELAB (elaborateBlock b)) : (elaborateStmts (tail ss))

-- TODO: fix comb assignment operators
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

elaborateExp :: Exp -> ExpElab
elaborateExp e =
    case e of
        HEXNUM_EXP h -> CONST_ELAB (elaborateConst h)
        DECNUM_EXP d -> CONST_ELAB (elaborateConst d)
        IDENTIFIER_EXP id -> IDENTIFIER_ELAB id
        BINOP_EXP b -> BINOP_ELAB (elaborateBinop b)
        UNOP_EXP u -> UNOP_ELAB (elaborateUnop u)

elaborateBinop :: Binop -> BinopElab
elaborateBinop (Binop op e1 e2) =
    BinopElab (translateBinop (tokenCat op)) op (elaborateExp e1) (elaborateExp e2)

elaborateUnop :: Unop -> UnopElab
elaborateUnop (Unop op e) =
    UnopElab (translateUnop (tokenCat op)) op (elaborateExp e)

elaborateType :: Type -> TypeElab
elaborateType t = TypeElab (typeCategory t) (typeToken t)

elaborateConst :: Token -> Const
elaborateConst tok =
    case (tokenCat tok) of
        HEXNUM h ->
            case (Numeric.readHex h) of
                [(i, _)] -> INT_CONST i
        DECNUM d ->
            case (Numeric.readDec d) of
                [(i, _)] -> INT_CONST i

translateBinop :: TokenCategory -> BinopCatElab
translateBinop cat = 
    case cat of
        PLUS -> ADD_EXP_ELAB
        DASH -> SUB_EXP_ELAB
        STAR -> MUL_EXP_ELAB
        SLASH -> DIV_EXP_ELAB
        PERC -> MOD_EXP_ELAB
        AMP -> AND_EXP_ELAB
        CARET -> XOR_EXP_ELAB
        PIPE -> OR_EXP_ELAB
        LEFT_LEFT -> SLA_EXP_ELAB
        RIGHT_RIGHT -> SRA_EXP_ELAB
        LEFT -> LT_EXP_ELAB
        RIGHT -> GT_EXP_ELAB
        LEFT_EQ -> LTE_EXP_ELAB
        RIGHT_EQ -> GTE_EXP_ELAB
        EQ_EQ -> EQ_EXP_ELAB
        EXCL_EQ -> NEQ_EXP_ELAB
        AMP_AMP -> LOGAND_EXP_ELAB
        PIPE_PIPE -> LOGOR_EXP_ELAB
        _ -> error (compilerError "Attempt to translate non-binop exp token.")

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
