module Model.Elaborated (
    FunctionElab (..),
    StatementElab (..),
    DeclElab (..),
    AsnElab (..),
    RetElab (..),
    SeqElab,
    ExpElab (..),
    BinopElab (..),
    BinopCatElab (..),
    UnopElab (..),
    UnopCatElab (..),
    VariableElab (..),
    TypeElab (..),
    extractIdentifierName,
) where

import Common.Errors
import Model.Tokens
import Model.Types

import qualified Data.Map as Map

data FunctionElab = FunctionElab
    { functionElabName :: Token
    , functionElabReturnType :: TypeElab
    , functionElabBlock :: SeqElab
    }

data StatementElab
    = DECL_ELAB DeclElab
    | ASN_ELAB AsnElab
    | RET_ELAB RetElab
    | EXP_ELAB ExpElab
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
    | TERN_ELAB ExpElab ExpElab ExpElab

data BinopElab = BinopElab
    { binopElabCat :: BinopCatElab
    , binopElabOp :: Token
    , binopElabExp1 :: ExpElab
    , binopElabExp2 :: ExpElab
    }

data BinopCatElab
    = ADD_EXP_ELAB
    | SUB_EXP_ELAB
    | MUL_EXP_ELAB
    | DIV_EXP_ELAB
    | MOD_EXP_ELAB
    | AND_EXP_ELAB
    | XOR_EXP_ELAB
    | OR_EXP_ELAB
    | SLA_EXP_ELAB
    | SRA_EXP_ELAB
    | LT_EXP_ELAB
    | GT_EXP_ELAB
    | LTE_EXP_ELAB
    | GTE_EXP_ELAB
    | EQ_EXP_ELAB
    | NEQ_EXP_ELAB
    | LOGAND_EXP_ELAB
    | LOGOR_EXP_ELAB

data UnopElab = UnopElab
    { unopElabCat :: UnopCatElab
    , unopElabOp :: Token
    , unopElabExp :: ExpElab
    }

data UnopCatElab
    = NEG_EXP_ELAB
    | NOT_EXP_ELAB
    | LOGNOT_EXP_ELAB

data VariableElab = VariableElab
    { variableElabIdentifier :: Token
    , variableElabType :: TypeElab
    }

data TypeElab = TypeElab
    { typeElabType :: TypeCategory
    , typeElabToken :: Token
    }
    deriving (Show)

-- HELPERS

extractIdentifierName :: Token -> String
extractIdentifierName token =
    case (tokenCat token) of
        IDENTIFIER name -> name
        _ -> error (compilerError "Expected an identifer token but got token=" ++ (show token))
