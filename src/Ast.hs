module Ast (
    Node (..),
    -- Program,
    -- Block,
    -- BlockElem (..),
    Statements,
    Statement (..),
    Decl,
    Simp,
    Lval,
    Exp (..),
    Unop (..),
    Binop (..),
) where

import Tokens

-- TODO: include all types into AST node
data Node = TOKEN_NODE Token | EXP_NODE Exp | STMT_NODE Statement | STMTS_NODE Statements
    deriving (Show)

-- type Program = Block

-- type Block = Statements

type Statements = [Statement]

-- data Statement = DECL Decl | SIMP Simp | RET Exp
data Statement = STMT_EXP Exp
    deriving (Show)

data Decl = Decl
    { simpIdentifier :: Token
    , simpExpression :: Exp
    }

data Simp = Simp
    { simpAsnop :: Token
    , simpLvalue :: Lval
    , simpExp :: Exp
    }

type Lval = Token

data Exp = INTCONST_EXP Intconst | IDENTIFIER_EXP Token | BINOP_EXP Binop | UNOP_EXP Unop
    deriving (Show)

type Intconst = Token

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
