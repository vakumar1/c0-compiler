module Ast (
    Program,
    Statements,
    Statement (..),
    Decl (..),
    Exp (..),
    Intconst (..),
    Unop (..),
    Binop (..),
) where

import Tokens

type Program = Statements

type Statements = [Statement]

data Statement = STMT_BLOCK Statements | STMT_DECL Decl | STMT_RET Exp
    deriving (Show)

data Decl = Decl
    { declIdentifier :: Token
    , declExpression :: Maybe Exp
    }
    deriving (Show)

-- data Simp = Simp
--     { simpAsnop :: Token
--     , simpLvalue :: Lval
--     , simpExp :: Exp
--     }

-- type Lval = Token

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
