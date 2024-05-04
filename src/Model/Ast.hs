module Model.Ast (
    Function (..),
    Block,
    Statements,
    Statement (..),
    Simp (..),
    Control (..),
    Asn (..),
    Decl (..),
    Post (..),
    If (..),
    While (..),
    For (..),
    Lval (..),
    Exp (..),
    Const (..),
    Unop (..),
    Binop (..),
    Ternop (..),
    Type (..),
) where

import Model.Tokens
import Model.Types

data Function = Function
    { functionName :: Token
    , functionReturnType :: Type
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
    deriving (Show)

data Control
    = IF_CTRL If
    | WHILE_CTRL While
    | FOR_CTRL For
    | RET_CTRL Exp
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
data Lval = Lval
    { lvalIdent :: Token
    }
    deriving (Show)

data Exp
    = HEXNUM_EXP Token
    | DECNUM_EXP Token
    | BOOL_EXP Token
    | IDENTIFIER_EXP Token
    | BINOP_EXP Binop
    | UNOP_EXP Unop
    | TERN_EXP Ternop
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

data Type = Type
    { typeCategory :: TypeCategory
    , typeToken :: Token
    }
    deriving (Show)
