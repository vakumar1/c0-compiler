module Ast (
    Function (..),
    Block,
    Statements,
    Statement (..),
    Simp (..),
    Asn (..),
    Decl (..),
    Post (..),
    Lval (..),
    Exp (..),
    Const (..),
    Unop (..),
    Binop (..),
    Type (..),
) where

import Tokens
import Types

data Function = Function
    { functionName :: Token
    , functionReturnType :: Type
    , functionBlock :: Block
    }

type Block = Statements

type Statements = [Statement]

data Statement
    = SIMP_STMT Simp
    | RET_STMT Exp
    | BLOCK_STMT Block
    deriving (Show)

data Simp 
    = ASN_SIMP Asn
    | DECL_SIMP Decl
    | POST_SIMP Post
    | EXP_SIMP Exp
    deriving (Show)

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
    | TERN_EXP Exp Exp Exp
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

data Type = Type
    { typeCategory :: TypeCategory
    , typeToken :: Token
    }
    deriving (Show)
