module Middle (

) where

import Elaborated
import Types
import Errors
import qualified Data.Map as Map

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
        SIMP_STMT s : _ -> (SIMP_ELAB (elaborateSimp s)) : (elaborateStmts (tail ss))
        RET_STMT e : _ -> (RET_ELAB (elaborateExp e)) : (elaborateStmts (tail ss))

elaborateDecl :: Decl -> Statements -> DeclElab
elaborateDecl decl remaining = 
    case decl of
        Decl id type Nothing
            -> DeclElab (Variable id type) (elaborateStmts remaining)
        Decl id type (Just e)
            -> DeclElab (Variable id type) (elaborateStmts ((SIMP_STMT (Simp (EQUAL) (Lval (IDENTIFIER s)) e)) : remaining))

elaborateSimp :: Simp -> AsnElab
elaborateSimp simp = 
    case simp of
        Simp (EQUAL) (Lval (IDENTIFIER s)) e -> 
            AsnElab s (elaborateExp e)
        Simp (PLUS_EQ) (Lval (IDENTIFIER s)) e ->
            AsnElab s (elaborateExp (BINOP_EXP (Binop (PLUS) (IDENTIFIER_EXP (IDENTIFER s)) e)))
        Simp (DASH_EQ) (Lval (IDENTIFIER s)) e ->
            AsnElab s (elaborateExp (BINOP_EXP (Binop (DASH) (IDENTIFIER_EXP (IDENTIFER s)) e)))
        Simp (STAR_EQ) (Lval (IDENTIFIER s)) e ->
            AsnElab s (elaborateExp (BINOP_EXP (Binop (STAR) (IDENTIFIER_EXP (IDENTIFER s)) e)))
        Simp (SLASH_EQ) (Lval (IDENTIFIER s)) e ->
            AsnElab s (elaborateExp (BINOP_EXP (Binop (SLASH) (IDENTIFIER_EXP (IDENTIFER s)) e)))
        Simp (PERC_EQ) (Lval (IDENTIFIER s)) e ->
            AsnElab s (elaborateExp (BINOP_EXP (Binop (PERC) (IDENTIFIER_EXP (IDENTIFER s)) e)))

elaborateExp :: Exp -> ExpElab
elaborateExp e = 
    case e of
        INTCONST_EXP i -> CONST_ELAB (elaborateConst i)
        IDENTIFIER_EXP (IDENTIFIER s) -> IDENTIFIER_ELAB s
        BINOP_EXP b -> let (be, effectful) = elaborateBinop b
                        in if effectful
                            then IMPURE_BINOP_ELAB be
                            else PURE_BINOP_ELAB be
        UNOP_EXP u -> let (ue, effectful) = elaborateUnop u
                        in if effectful
                            then IMPURE_UNOP_ELAB ue
                            else PURE_UNOP_ELAB ue


elaborateConst :: Intconst -> ConstElab
elaborateConst i = 
    case i of
        HEXNUM_INTCONST (HEXNUM h) -> INT_CONST_ELAB (hexStringToInt h)
        DECNUM_INTCONST (DECNUM d) -> INT_CONST_ELAB (decimalStringToInt d)

elaborateBinop :: Binop -> (BinopElab, bool)
elaborateBinop b = 
    case b of
        Binop (PLUS) e1 e2 -> (ADD_EXP_ELAB (elaborateExp e1) (elaborateExp e2), false)
        Binop (DASH) e1 e2 -> (SUB_EXP_ELAB (elaborateExp e1) (elaborateExp e2), false)
        Binop (STAR) e1 e2 -> (MUL_EXP_ELAB (elaborateExp e1) (elaborateExp e2), false)
        Binop (SLASH) e1 e2 -> (DIV_EXP_ELAB (elaborateExp e1) (elaborateExp e2), true)
        Binop (PERC) e1 e2 -> (MOD_EXP_ELAB (elaborateExp e1) (elaborateExp e2), false)
        
elaborateUnop :: Unop -> (UnopElb, bool)
elaborateUnop u = 
    case u of
        Unop (DASH) e -> (NEG_EXP_ELAB (elaborateExp e), false)

{-

VERIFICATION

-}

verifyInitialization :: ProgramElab -> [VerificationError]
verifyInitialization program = verifyInitializationSeq program empty

verifyInitializationSeq :: SeqElab -> Map String Token -> [VerificationError]
verifyInitializationSeq seq initialized = 
    case seq of
        [] -> (initialized, [])
        DECL_ELAB d : _ -> (verifyInitializationDecl d initialized) : (verifyInitializationSeq (tail seq) initialized)
        ASN_ELAB a : _ -> (verifyInitializationAsn a initialized) : (verifyInitializationSeq (tail seq) initialized)
        RET_ELAB r : _ -> (verifyInitializationRet r initialized) : (verifyInitializationSeq (tail seq) initialized)

verifyInitializationDecl :: DeclElab -> Map String Token -> [VerificationError]
verifyInitializationDecl decl initialized = 
    let subSeq = declElabStatement decl
        identifier = (variableIdentifier . declVariable) decl
    in case (extractIdentifierName identifier) of
        Just name -> 
            let (_, subErrors) = verifyInitializationSeq seq (insert name identifier initialized)
            in case (lookup name initialized) of
                Just prevIdentifier -> (DOUBLE_INIT (DoubleInitializationError prevIdentifier identifer)) : subErrors
                _ -> subErrors

verifyInitializationAsn :: AsnElab -> Map String Token -> [VerificationError]
verifyInitializationAsn asn initialized = 
    (verifyInitializationIdentifier (asnElabIdentifier asn) initialized) ++ 
    (verifyInitializationExp (asnElabExpression asn) initialized)

verifyInitializationRet :: RetElab -> Map String Token -> [VerificationError]
verifyInitializationRet ret initialized = 
    (verifyInitializationExp (retElabExpression ret) initialized)

verifyInitializationExp :: ExpElab -> Map String Token -> [VerificationError]
verifyInitializationExp exp initialized = 
    case exp of
        CONST_ELAB _ -> []
        IDENTIFIER_ELAB identifier -> verifyInitializationIdentifier identifier initialized
        PURE_BINOP_ELAB binop -> verifyInitializationBinop binop initialized
        IMPURE_BINOP_ELAB binop -> verifyInitializationBinop binop initialized
        PURE_UNOP_ELAB unop -> verifyInitializationUnop unop initialized
        IMPURE_UNOP_ELAB unop -> verifyInitializationUnop unop initialized

verifyInitializationIdentifier :: Token -> Map String Token -> [VerificationError]
verifyInitializationIdentifier identifier initialized = 
    case (extractIdentifierName identifier) of
        Just name ->
            case (lookup name initialized) of
                Nothing -> [(USE_BEFORE_INIT (UseBeforeInitializationError identifier))]
                _ -> []

verifyInitializationBinop :: BinopElab -> Map String Token -> [VerificationError]
verifyInitializationBinop binop initialized = 
    case binop of
        _ e1 e2 -> (verifyInitializationExp e1 initialized) ++ (verifyInitializationExp e2 initialized)

verifyInitializationUnop :: UnopElab -> Map String Token -> [VerificationError]
verifyInitializationUnop unop initialized = 
    case unop of
        _ e -> verifyInitializationExp e initialized