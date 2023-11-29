module Elaborated (
    ProgramElab,
    StatementElab (..),
    DeclElab (..),
    AsnElab (..),
    SeqElab,
    ExpElab (..),
    ConstElab (..),
    BinopElab (..),
    UnopElab (..),
) where

import Types

type ProgramElab = SeqElab

data StatementElab = DECL_ELAB DeclElab
    | ASN_ELAB AsnElab
    | RET_ELAB RetElab
    | SEQ_ELAB SeqElab

data DeclElab = DeclElab
    { declVariable :: Variable
    , declElabStatement :: StatementElab
    }

data AsnElab = AsnElab
    { asnElabIdentifier :: Token
    , asnElabExpression :: ExpElab
    }

data RetElab = RetElab
    { retElabExpression :: ExpElab
    }

type SeqElab = [StatementElab]

data ExpElab = CONST_ELAB ConstElab
    | IDENTIFIER_ELAB Token
    | PURE_BINOP_ELAB BinopElab
    | IMPURE_BINOP_ELAB BinopElab
    | PURE_UNOP_ELAB UnopElab
    | IMPURE_UNOP_ELAB UnopElab

data ConstElab = INT_CONST_ELAB Int

data BinopElab = ADD_EXP_ELAB ExpElab ExpElab
    | SUB_EXP_ELAB ExpElab ExpElab
    | MUL_EXP_ELAB ExpElab ExpElab
    | DIV_EXP_ELAB ExpElab ExpElab
    | MOD_EXP_ELAB ExpElab ExpElab

data UnopElab = NEG_EXP_ELAB ExpElab

data Variable = Variable
    { variableIdentifier :: Token
    , variableType :: Type
    }

