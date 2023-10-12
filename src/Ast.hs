module Ast (
    Node (..),
    Program,
    Block,
    BlockElem (..),
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
data Node = TOKEN_NODE Token | EXP_NODE Exp

type Program = Block

type Block = [BlockElem]

data BlockElem = BLOCK Block | STATEMENT Statement

data Statement = DECL Decl | SIMP Simp | RET Exp

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

type Intconst = Token

data Unop = Unop
    { unop :: Token
    , unopExpression :: Exp
    }

data Binop = Binop
    { binop :: Token
    , binopLeftExpression :: Exp
    , binopRightExpression :: Exp
    }
