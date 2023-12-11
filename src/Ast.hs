module Ast (
    Function (..),
    Block,
    Statements,
    Statement (..),
    Simp (..),
    Lval (..),
    Decl (..),
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
    = DECL_STMT Decl
    | SIMP_STMT Simp
    | RET_STMT Exp
    deriving (Show)

data Decl = Decl
    { declIdentifier :: Token
    , declType :: Type
    , declEqual :: Maybe Token
    , declExpression :: Maybe Exp
    }
    deriving (Show)

data Simp = Simp
    { simpAsnop :: Token
    , simpLvalue :: Lval
    , simpExp :: Exp
    }
    deriving (Show)

data Lval = Lval
    { lvalIdent :: Token
    }
    deriving (Show)

data Exp
    = HEXNUM_EXP Token
    | DECNUM_EXP Token
    | IDENTIFIER_EXP Token
    | BINOP_EXP Binop
    | UNOP_EXP Unop
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
