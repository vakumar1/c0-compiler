module Middle (

) where

import Types
import Elaborated

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
        Decl (IDENTIFIER s) Nothing
            -> DeclElab (declIdentifier s) (INT_TYPE) (elaborateStmts remaining)
        Decl (IDENTIFIER s) (Just e)
            -> DeclElab (declIdentifier s) (INT_TYPE) (elaborateStmts ((SIMP_STMT (Simp (EQUAL) (Lval (IDENTIFIER s)) e)) : remaining))

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
