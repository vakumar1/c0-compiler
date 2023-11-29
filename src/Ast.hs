module Ast (
    Program,
    Block,
    Statements,
    Statement (..),
    Simp (..),
    Lval (..),
    Decl (..),
    Exp (..),
    Intconst (..),
    Unop (..),
    Binop (..),
    TypeCategory (..),
    Type (..)
) where

import Tokens
import Types

type Program = Block

type Block = Statements

type Statements = [Statement]

data Statement = DECL_STMT Decl
    | SIMP_STMT Simp
    | RET_STMT Exp
    deriving (Show)

data Decl = Decl
    { declIdentifier :: Token
    , declType :: Type
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

data Exp = INTCONST_EXP Intconst 
    | IDENTIFIER_EXP Token 
    | BINOP_EXP Binop 
    | UNOP_EXP Unop
    deriving (Show)

data Intconst = HEXNUM_INTCONST Token
    | DECNUM_INTCONST Token
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
