module Middle (
    elaborate,
    verifyDeclaration,
    verifyReturn,
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
        HEXNUM_EXP h -> CONST_ELAB (elaborateConst i)
        DECNUM_EXP d -> CONST_ELAB (elaborateConst d)
        IDENTIFIER_EXP id -> IDENTIFIER_ELAB id
        BINOP_EXP b -> BINOP_ELAB (elaborateBinop b)
        UNOP_EXP u -> UNOP_ELAB (elaborateUnop u)

elaborateBinop :: Binop -> BinopElab
elaborateBinop (Binop op e1 e2) =
    case (tokenCat op) of
        PLUS -> ADD_EXP_ELAB (elaborateExp e1) (elaborateExp e2)
        DASH -> SUB_EXP_ELAB (elaborateExp e1) (elaborateExp e2)
        STAR -> MUL_EXP_ELAB (elaborateExp e1) (elaborateExp e2)
        SLASH -> DIV_EXP_ELAB (elaborateExp e1) (elaborateExp e2)
        PERC -> MOD_EXP_ELAB (elaborateExp e1) (elaborateExp e2)

elaborateUnop :: Unop -> UnopElab
elaborateUnop (Unop op e) =
    case (tokenCat op) of
        DASH -> NEG_EXP_ELAB (elaborateExp e)

elaborateType :: Type -> TypeElab
elaborateType t = TypeElab (typeCategory t) (typeToken t)

elaborateConst :: Token -> ConstElab
elaborateConst i =
    case i of
        HEXNUM_CONST (Token (HEXNUM h) _) ->
            case (Numeric.readHex h) of
                [(i, _)] -> INT_CONST_ELAB i
        DECNUM_CONST (Token (DECNUM d) _) ->
            case (Numeric.readDec d) of
                [(i, _)] -> INT_CONST_ELAB i

{-

INITIALIZATION VERIFICATION

-}

verifyDeclaration :: FunctionElab -> [VerificationError]
verifyDeclaration fn =
    let (_, errors) = verifyDeclarationSeq (functionElabBlock fn) Map.empty
     in errors

verifyDeclarationStmt :: StatementElab -> Map.Map String Token -> (Map.Map String Token, [VerificationError])
verifyDeclarationStmt stmt initialized =
    case stmt of
        DECL_ELAB d -> verifyDeclarationDecl d initialized
        ASN_ELAB a -> verifyDeclarationAsn a initialized
        RET_ELAB r -> verifyDeclarationRet r initialized
        SEQ_ELAB s -> verifyDeclarationSeq s initialized

verifyDeclarationSeq :: SeqElab -> Map.Map String Token -> (Map.Map String Token, [VerificationError])
verifyDeclarationSeq seq initialized =
    case seq of
        [] -> (initialized, [])
        s : _ ->
            let (newInitialized, headErrors) = verifyDeclarationStmt s initialized
                (_, tailErrors) = verifyDeclarationSeq (tail seq) newInitialized
             in (initialized, headErrors ++ tailErrors)

verifyDeclarationDecl :: DeclElab -> Map.Map String Token -> (Map.Map String Token, [VerificationError])
verifyDeclarationDecl decl initialized =
    let identifier = (variableElabIdentifier (declElabVariable decl))
        asn = (declElabAsn decl)
        (declErrors, newInitialized) =
            let name = extractIdentifierName identifier
             in case (Map.lookup name initialized) of
                    Just prevIdentifier ->
                        ([DOUBLE_DECL (DoubleDeclarationError prevIdentifier identifier)], initialized)
                    _ ->
                        ([], (Map.insert name identifier initialized))
        asnErrors =
            case asn of
                Just a -> snd (verifyDeclarationAsn a newInitialized)
                Nothing -> []
     in (newInitialized, declErrors ++ asnErrors)

verifyDeclarationAsn :: AsnElab -> Map.Map String Token -> (Map.Map String Token, [VerificationError])
verifyDeclarationAsn asn initialized =
    ( initialized
    , (verifyDeclarationIdentifier (asnElabIdentifier asn) initialized)
        ++ (verifyDeclarationExp (asnElabExpression asn) initialized)
    )

verifyDeclarationRet :: RetElab -> Map.Map String Token -> (Map.Map String Token, [VerificationError])
verifyDeclarationRet ret initialized =
    (initialized, verifyDeclarationExp (retElabExpression ret) initialized)

verifyDeclarationExp :: ExpElab -> Map.Map String Token -> [VerificationError]
verifyDeclarationExp exp initialized =
    case exp of
        CONST_ELAB _ -> []
        IDENTIFIER_ELAB identifier -> verifyDeclarationIdentifier identifier initialized
        BINOP_ELAB binop -> verifyDeclarationBinop binop initialized
        UNOP_ELAB unop -> verifyDeclarationUnop unop initialized

verifyDeclarationIdentifier :: Token -> Map.Map String Token -> [VerificationError]
verifyDeclarationIdentifier identifier initialized =
    case (Map.lookup (extractIdentifierName identifier) initialized) of
        Nothing -> [(USE_BEFORE_DECL (UseBeforeDeclarationError identifier))]
        _ -> []

verifyDeclarationBinop :: BinopElab -> Map.Map String Token -> [VerificationError]
verifyDeclarationBinop binop initialized =
    case binop of
        ADD_EXP_ELAB e1 e2 -> (verifyDeclarationExp e1 initialized) ++ (verifyDeclarationExp e2 initialized)
        SUB_EXP_ELAB e1 e2 -> (verifyDeclarationExp e1 initialized) ++ (verifyDeclarationExp e2 initialized)
        MUL_EXP_ELAB e1 e2 -> (verifyDeclarationExp e1 initialized) ++ (verifyDeclarationExp e2 initialized)
        DIV_EXP_ELAB e1 e2 -> (verifyDeclarationExp e1 initialized) ++ (verifyDeclarationExp e2 initialized)
        MOD_EXP_ELAB e1 e2 -> (verifyDeclarationExp e1 initialized) ++ (verifyDeclarationExp e2 initialized)

verifyDeclarationUnop :: UnopElab -> Map.Map String Token -> [VerificationError]
verifyDeclarationUnop unop initialized =
    case unop of
        NEG_EXP_ELAB e -> verifyDeclarationExp e initialized

{-

RETURN VERIFICATION

-}

-- TODO: add type check on return value

verifyReturn :: FunctionElab -> [VerificationError]
verifyReturn fn =
    if verifyReturnSeq (functionElabBlock fn)
        then []
        else [INVALID_RET (InvalidReturnError (functionElabName fn))]

verifyReturnSeq :: SeqElab -> Bool
verifyReturnSeq seq =
    case seq of
        [] -> False
        s : _ -> (verifyReturnStmt s) || (verifyReturnSeq (tail seq))

verifyReturnStmt :: StatementElab -> Bool
verifyReturnStmt stmt =
    case stmt of
        DECL_ELAB d -> False
        ASN_ELAB a -> False
        RET_ELAB r -> True
        SEQ_ELAB s -> False
