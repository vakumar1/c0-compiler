module Middle (
    elaborate,
    verifyInitialization,
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
elaborate :: Program -> ProgramElab
elaborate = elaborateBlock

elaborateBlock :: Block -> SeqElab
elaborateBlock = elaborateStmts

elaborateStmts :: Statements -> SeqElab
elaborateStmts ss =
    case ss of
        [] -> []
        DECL_STMT d : _ -> [(DECL_ELAB (elaborateDecl d (tail ss)))]
        SIMP_STMT s : _ -> (ASN_ELAB (elaborateSimp s)) : (elaborateStmts (tail ss))
        RET_STMT e : _ -> (RET_ELAB (RetElab (elaborateExp e))) : (elaborateStmts (tail ss))

elaborateDecl :: Decl -> Statements -> DeclElab
elaborateDecl decl remaining =
    case decl of
        Decl id ty Nothing Nothing ->
            DeclElab (Variable id ty) (SEQ_ELAB (elaborateStmts remaining))
        Decl id ty (Just as) (Just e) ->
            DeclElab (Variable id ty) (SEQ_ELAB (elaborateStmts ((SIMP_STMT (Simp as (Lval id) e)) : remaining)))

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

VERIFICATION

-}

verifyInitialization :: ProgramElab -> [VerificationError]
verifyInitialization program = verifyInitializationSeq program Map.empty

verifyInitializationStmt :: StatementElab -> Map.Map String Token -> [VerificationError]
verifyInitializationStmt stmt initialized =
    case stmt of
        DECL_ELAB d -> verifyInitializationDecl d initialized
        ASN_ELAB a -> verifyInitializationAsn a initialized
        RET_ELAB r -> verifyInitializationRet r initialized
        SEQ_ELAB s -> verifyInitializationSeq s initialized

verifyInitializationSeq :: SeqElab -> Map.Map String Token -> [VerificationError]
verifyInitializationSeq seq initialized =
    case seq of
        [] -> []
        s : _ -> (verifyInitializationStmt s initialized) ++ (verifyInitializationSeq (tail seq) initialized)

verifyInitializationDecl :: DeclElab -> Map.Map String Token -> [VerificationError]
verifyInitializationDecl decl initialized =
    let stmt = declElabStatement decl
        identifier = (variableIdentifier . declVariable) decl
     in case (extractIdentifierName identifier) of
            Just name ->
                case (Map.lookup name initialized) of
                    Just prevIdentifier -> (DOUBLE_INIT (DoubleInitializationError prevIdentifier identifier)) : (verifyInitializationStmt stmt initialized)
                    _ -> verifyInitializationStmt stmt (Map.insert name identifier initialized)

verifyInitializationAsn :: AsnElab -> Map.Map String Token -> [VerificationError]
verifyInitializationAsn asn initialized =
    (verifyInitializationIdentifier (asnElabIdentifier asn) initialized)
        ++ (verifyInitializationExp (asnElabExpression asn) initialized)

verifyInitializationRet :: RetElab -> Map.Map String Token -> [VerificationError]
verifyInitializationRet ret initialized =
    (verifyInitializationExp (retElabExpression ret) initialized)

verifyInitializationExp :: ExpElab -> Map.Map String Token -> [VerificationError]
verifyInitializationExp exp initialized =
    case exp of
        CONST_ELAB _ -> []
        IDENTIFIER_ELAB identifier -> verifyInitializationIdentifier identifier initialized
        PURE_BINOP_ELAB binop -> verifyInitializationBinop binop initialized
        IMPURE_BINOP_ELAB binop -> verifyInitializationBinop binop initialized
        PURE_UNOP_ELAB unop -> verifyInitializationUnop unop initialized
        IMPURE_UNOP_ELAB unop -> verifyInitializationUnop unop initialized

verifyInitializationIdentifier :: Token -> Map.Map String Token -> [VerificationError]
verifyInitializationIdentifier identifier initialized =
    case (extractIdentifierName identifier) of
        Just name ->
            case (Map.lookup name initialized) of
                Nothing -> [(USE_BEFORE_INIT (UseBeforeInitializationError identifier))]
                _ -> []

verifyInitializationBinop :: BinopElab -> Map.Map String Token -> [VerificationError]
verifyInitializationBinop binop initialized =
    case binop of
        ADD_EXP_ELAB e1 e2 -> (verifyInitializationExp e1 initialized) ++ (verifyInitializationExp e2 initialized)
        SUB_EXP_ELAB e1 e2 -> (verifyInitializationExp e1 initialized) ++ (verifyInitializationExp e2 initialized)
        MUL_EXP_ELAB e1 e2 -> (verifyInitializationExp e1 initialized) ++ (verifyInitializationExp e2 initialized)
        DIV_EXP_ELAB e1 e2 -> (verifyInitializationExp e1 initialized) ++ (verifyInitializationExp e2 initialized)
        MOD_EXP_ELAB e1 e2 -> (verifyInitializationExp e1 initialized) ++ (verifyInitializationExp e2 initialized)

verifyInitializationUnop :: UnopElab -> Map.Map String Token -> [VerificationError]
verifyInitializationUnop unop initialized =
    case unop of
        NEG_EXP_ELAB e -> verifyInitializationExp e initialized
