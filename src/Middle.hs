module Middle (
    elaborate,
) where

import Ast
import qualified Data.Map as Map
import Elaborated
import Errors
import qualified Numeric
import Tokens
import Types

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
        DECL_STMT d : _ -> (DECL_ELAB (elaborateDecl d)) : (elaborateStmts (tail ss))
        SIMP_STMT s : _ -> (ASN_ELAB (elaborateSimp s)) : (elaborateStmts (tail ss))
        RET_STMT e : _ -> (RET_ELAB (RetElab (elaborateExp e))) : (elaborateStmts (tail ss))

elaborateDecl :: Decl -> DeclElab
elaborateDecl decl =
    case decl of
        Decl id ty Nothing Nothing ->
            DeclElab (VariableElab id (elaborateType ty)) Nothing
        Decl id ty (Just as) (Just e) ->
            DeclElab (VariableElab id (elaborateType ty)) (Just (elaborateSimp (Simp as (Lval id) e)))

elaborateSimp :: Simp -> AsnElab
elaborateSimp (Simp as (Lval id) e) =
    case (tokenCat as) of
        EQUAL -> AsnElab id (elaborateExp e)
        _ -> AsnElab id (elaborateExp (BINOP_EXP (Binop as (IDENTIFIER_EXP id) e)))

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
    case (tokenCat op) of
        PLUS -> BinopElab ADD_EXP_ELAB op (elaborateExp e1) (elaborateExp e2)
        DASH -> BinopElab SUB_EXP_ELAB op (elaborateExp e1) (elaborateExp e2)
        STAR -> BinopElab MUL_EXP_ELAB op (elaborateExp e1) (elaborateExp e2)
        SLASH -> BinopElab DIV_EXP_ELAB op (elaborateExp e1) (elaborateExp e2)
        PERC -> BinopElab MOD_EXP_ELAB op (elaborateExp e1) (elaborateExp e2)

elaborateUnop :: Unop -> UnopElab
elaborateUnop (Unop op e) =
    case (tokenCat op) of
        DASH -> UnopElab NEG_EXP_ELAB op (elaborateExp e)

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
