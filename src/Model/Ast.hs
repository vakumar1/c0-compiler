module Model.Ast (
    Program (..),
    GlobalDecl (..),
    Typedef (..),
    Function (..),
    FunctionSignature (..),
    Param (..),
    Block,
    Statements,
    Statement (..),
    Simp (..),
    Control (..),
    Asn (..),
    Decl (..),
    Post (..),
    Assert (..),
    If (..),
    While (..),
    For (..),
    Lval (..),
    Exp (..),
    Const (..),
    Unop (..),
    Binop (..),
    Ternop (..),
    FunctionCall (..),
    Type (..),
) where

import Model.Tokens
import Model.Types

type Program = [GlobalDecl]

data GlobalDecl
    = TYPEDEF_GDECL Typedef
    | FNDECL_GDECL FunctionSignature
    | FNDEFN_GDECL Function

data Typedef = Typedef
    { typedefType :: Type
    , typedefAlias :: Token
    }

data FunctionSignature = FunctionSignature
    { functionSignatureName :: Token
    , functionSignatureArgs :: [Param]
    , functionSignatureRetType :: Type
    }

data Param = Param
    { paramIdentifier :: Token
    , paramType :: Type
    }

data Function = Function
    { functionSignature :: FunctionSignature
    , functionBlock :: Block
    }

type Block = Statements

type Statements = [Statement]

-- high-level statement operations
data Statement
    = SIMP_STMT Simp
    | CONTROL_STMT Control
    | BLOCK_STMT Block
    deriving (Show)

data Simp 
    = ASN_SIMP Asn
    | DECL_SIMP Decl
    | POST_SIMP Post
    | EXP_SIMP Exp
    | ASSERT_SIMP Assert
    deriving (Show)

data Control
    = IF_CTRL If
    | WHILE_CTRL While
    | FOR_CTRL For
    | RET_CTRL (Maybe Exp)
    deriving (Show)

-- low-level statement operations
data Asn = Asn
    { asnAsnop :: Token
    , asnLvalue :: Lval
    , asnExp :: Exp
    }
    deriving (Show)

data Decl = Decl
    { declIdentifier :: Token
    , declType :: Type
    , declEqual :: Maybe Token
    , declExpression :: Maybe Exp
    }
    deriving (Show)

data Post = Post
    { postSimpOp :: Token
    , postSimpLvalue :: Lval
    }
    deriving (Show)

data Assert = Assert
    { assertTok :: Token
    , assertExp :: Exp
    }
    deriving (Show)

data If = If
    { ifExp :: Exp
    , ifStmt :: Statement
    , ifElseoptStmt :: Maybe Statement
    }
    deriving (Show)

data While = While
    { whileExp :: Exp
    , whileStmt :: Statement
    }
    deriving (Show)

data For = For
    { forInitSimp :: Maybe Simp
    , forTermExp :: Exp
    , forInterSimp :: Maybe Simp
    , forStmt :: Statement
    }
    deriving (Show)

-- statement objects
data Lval 
    = IDENT_LVAL Token
    | DEREF_LVAL Lval
    deriving (Show)

data Exp
    = HEXNUM_EXP Token
    | DECNUM_EXP Token
    | BOOL_EXP Token
    | NULL_EXP Token
    | IDENTIFIER_EXP Token
    | BINOP_EXP Binop
    | UNOP_EXP Unop
    | TERN_EXP Ternop
    | FN_CALL_EXP FunctionCall
    deriving (Show)

data Unop = Unop
    { unop :: Token
    , unopExpression :: Exp
    }
    deriving (Show)

data Binop = Binop
    { binop :: Token
    , binopLeftExpression :: Exp
    , binopRightExpression :: Exp
    }
    deriving (Show)

data Ternop = Ternop
    { ternop :: Token
    , ternopCondExpression :: Exp
    , ternopLeftExpression :: Exp
    , ternopRightExpression :: Exp
    }
    deriving (Show)

data FunctionCall = FunctionCall
    { functionCallName :: Token
    , functionCallArgs :: [Exp]
    }
    deriving (Show)

data Type 
    = BASE_TYPE_AST Token
    | POINTER_TYPE_AST Type
    deriving (Show)

getBaseTypeToken :: Type -> Token
getBaseTypeToken ty = 
    case ty of
        BASE_TYPE_AST tok -> tok
        POINTER_TYPE_AST t -> getBaseTypeToken t
