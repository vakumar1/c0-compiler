module Elaborated (
    FunctionElab (..),
    StatementElab (..),
    DeclElab (..),
    AsnElab (..),
    RetElab (..),
    SeqElab,
    ExpElab (..),
    BinopElab (..),
    UnopElab (..),
    VariableElab (..),
    TypeElab (..),
    extractIdentifierName,
) where

import Errors
import Tokens
import Types

data FunctionElab = FunctionElab
    { functionElabName :: Token
    , functionElabReturnType :: TypeElab
    , functionElabBlock :: SeqElab
    }

data StatementElab
    = DECL_ELAB DeclElab
    | ASN_ELAB AsnElab
    | RET_ELAB RetElab
    | SEQ_ELAB SeqElab

data DeclElab = DeclElab
    { declElabVariable :: VariableElab
    , declElabAsn :: Maybe AsnElab
    }

data AsnElab = AsnElab
    { asnElabIdentifier :: Token
    , asnElabExpression :: ExpElab
    }

data RetElab = RetElab
    { retElabExpression :: ExpElab
    }

type SeqElab = [StatementElab]

data ExpElab
    = CONST_ELAB Const
    | IDENTIFIER_ELAB Token
    | BINOP_ELAB BinopElab
    | UNOP_ELAB UnopElab

data BinopElab
    = ADD_EXP_ELAB ExpElab ExpElab
    | SUB_EXP_ELAB ExpElab ExpElab
    | MUL_EXP_ELAB ExpElab ExpElab
    | DIV_EXP_ELAB ExpElab ExpElab
    | MOD_EXP_ELAB ExpElab ExpElab

data UnopElab = NEG_EXP_ELAB ExpElab

data VariableElab = VariableElab
    { variableElabIdentifier :: Token
    , variableElabType :: TypeElab
    }

data TypeElab = TypeElab
    { typeElabType :: TypeCategory
    , typeElabToken :: Token
    }
    deriving (Show)

-- DATA STRUCTURE HELPERS

extractIdentifierName :: Token -> String
extractIdentifierName token =
    case (tokenCat token) of
        IDENTIFIER name -> name
        _ -> compilerError "Expected an identifer token but got token=" ++ (show token)
