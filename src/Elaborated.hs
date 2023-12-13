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

data UnopElab = UnopElab
    { unopElabCat :: UnopCatElab
    , unopElabOp :: Token
    , unopElabExp :: ExpElab
    }

data UnopCatElab
    = NEG_EXP_ELAB

data OpElabCat
    = BINOP_CAT_ELAB BinopCatElab
    | UNOP_CAT_ELAB UnopCatElab

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

data Scope = Scope
    { scopeMaps :: [Map.Map String VariableElab]
    , scopeRegCtr :: Int
    }

extractIdentifierName :: Token -> String
extractIdentifierName token =
    case (tokenCat token) of
        IDENTIFIER name -> name
        _ -> compilerError "Expected an identifer token but got token=" ++ (show token)
