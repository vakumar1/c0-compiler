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
     in FunctionElab (functionName fn) (functionReturnType fn) seq

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
            DeclElab (Variable id ty) Nothing
        Decl id ty (Just as) (Just e) ->
            DeclElab (Variable id ty) (Just (elaborateSimp (Simp as (Lval id) e)))

-- (SEQ_ELAB (elaborateStmts ((SIMP_STMT (Simp as (Lval id) e)) : remaining)))

elaborateSimp :: Simp -> AsnElab
elaborateSimp (Simp as (Lval id) e) =
    case (tokenCat as) of
        EQUAL -> AsnElab id (elaborateExp e)
        _ -> AsnElab id (elaborateExp (BINOP_EXP (Binop as (IDENTIFIER_EXP id) e)))

elaborateExp :: Exp -> ExpElab
elaborateExp e =
    case e of
        INTCONST_EXP i -> CONST_ELAB (elaborateConst i)
        IDENTIFIER_EXP id -> IDENTIFIER_ELAB id
        BINOP_EXP b ->
            let (be, effectful) = elaborateBinop b
             in if effectful
                    then IMPURE_BINOP_ELAB be
                    else PURE_BINOP_ELAB be
        UNOP_EXP u ->
            let (ue, effectful) = elaborateUnop u
             in if effectful
                    then IMPURE_UNOP_ELAB ue
                    else PURE_UNOP_ELAB ue

elaborateConst :: Intconst -> ConstElab
elaborateConst i =
    case i of
        HEXNUM_INTCONST (Token (HEXNUM h) _) ->
            case (Numeric.readHex h) of
                [(i, _)] -> INT_CONST_ELAB i
        DECNUM_INTCONST (Token (DECNUM d) _) ->
            case (Numeric.readDec d) of
                [(i, _)] -> INT_CONST_ELAB i

elaborateBinop :: Binop -> (BinopElab, Bool)
elaborateBinop (Binop op e1 e2) =
    case (tokenCat op) of
        PLUS -> (ADD_EXP_ELAB (elaborateExp e1) (elaborateExp e2), False)
        DASH -> (SUB_EXP_ELAB (elaborateExp e1) (elaborateExp e2), False)
        STAR -> (MUL_EXP_ELAB (elaborateExp e1) (elaborateExp e2), False)
        SLASH -> (DIV_EXP_ELAB (elaborateExp e1) (elaborateExp e2), True)
        PERC -> (MOD_EXP_ELAB (elaborateExp e1) (elaborateExp e2), False)

elaborateUnop :: Unop -> (UnopElab, Bool)
elaborateUnop (Unop op e) =
    case (tokenCat op) of
        DASH -> (NEG_EXP_ELAB (elaborateExp e), False)

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
    let identifier = (variableIdentifier (declElabVariable decl))
        asn = (declElabAsn decl)
        (declErrors, newInitialized) =
            case (extractIdentifierName identifier) of
                Just name ->
                    case (Map.lookup name initialized) of
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
        PURE_BINOP_ELAB binop -> verifyDeclarationBinop binop initialized
        IMPURE_BINOP_ELAB binop -> verifyDeclarationBinop binop initialized
        PURE_UNOP_ELAB unop -> verifyDeclarationUnop unop initialized
        IMPURE_UNOP_ELAB unop -> verifyDeclarationUnop unop initialized

verifyDeclarationIdentifier :: Token -> Map.Map String Token -> [VerificationError]
verifyDeclarationIdentifier identifier initialized =
    case (extractIdentifierName identifier) of
        Just name ->
            case (Map.lookup name initialized) of
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
