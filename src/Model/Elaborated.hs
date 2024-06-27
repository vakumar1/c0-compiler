module Model.Elaborated (
    ProgramElab (..),
    GlobalDeclElab (..),
    FunctionElab (..),
    FunctionSignatureElab (..),
    StatementElab (..),
    DeclElab (..),
    AsnElab (..),
    IfElab (..),
    WhileElab (..),
    RetElab (..),
    AbortElab (..),
    SeqElab,
    LvalElab (..),
    LvalElabOpCat (..),
    ExpElab (..),
    TernopElab (..),
    BinopElab (..),
    BinopCatElab (..),
    LogBinopElab (..),
    LogBinopCatElab (..),
    UnopElab (..),
    UnopCatElab (..),
    FunctionCallElab (..),
    VariableElab (..),
    TypeElab (..),
    extractIdentifierName,
    isMainFunction,
    generateFnIdentifier,
) where

import Common.Errors
import Model.Tokens
import Model.Types

import qualified Data.Map as Map

type ProgramElab = [GlobalDeclElab]

data GlobalDeclElab 
    = FNDECL_GDECL_ELAB FunctionSignatureElab
    | FNDEFN_GDECL_ELAB FunctionElab
    deriving Show

data FunctionElab = FunctionElab
    { functionElabSignature :: FunctionSignatureElab
    , functionElabBlock :: SeqElab
    }
    deriving Show

data FunctionSignatureElab = FunctionSignatureElab
    { functionSignatureElabName :: Token
    , functionSignatureElabArgs :: [VariableElab]
    , functionSignatureElabRetType :: TypeElab
    }
    deriving Show
instance Eq FunctionSignatureElab where
    fnSign1 == fnSign2 = 
        (extractIdentifierName . functionSignatureElabName $ fnSign1) == (extractIdentifierName . functionSignatureElabName $ fnSign2)
        && (functionSignatureElabRetType fnSign1) == (functionSignatureElabRetType fnSign2)
        && (length . functionSignatureElabArgs $ fnSign1) == (length . functionSignatureElabArgs $ fnSign2)
        && (all 
                (\(param1, param2) -> (variableElabType param1) == (variableElabType param2)) 
                (zip (functionSignatureElabArgs fnSign1) (functionSignatureElabArgs fnSign2)))

data StatementElab
    = DECL_ELAB DeclElab
    | ASN_ELAB AsnElab
    | IF_ELAB IfElab
    | WHILE_ELAB WhileElab
    | RET_ELAB RetElab
    | ABORT_ELAB AbortElab
    | EXP_ELAB ExpElab
    | SEQ_ELAB SeqElab
    deriving Show

data DeclElab = DeclElab
    { declElabVariable :: VariableElab
    , declElabAsn :: Maybe AsnElab
    }
    deriving Show

data AsnElab = AsnElab
    { asnElabLval :: LvalElab
    , asnElabExpression :: ExpElab
    }
    deriving Show

data IfElab = IfElab
    { ifElabExp :: ExpElab
    , ifElabStmt :: StatementElab
    , ifElabElseoptStmt :: Maybe StatementElab
    }
    deriving Show

data WhileElab = WhileElab
    { whileElabExp :: ExpElab
    , whileElabStmt :: StatementElab
    }
    deriving Show

data RetElab = RetElab
    { retElabExpression :: Maybe ExpElab
    }
    deriving Show

data AbortElab = AbortElab
    { abortElabTok :: Token
    }
    deriving Show

type SeqElab = [StatementElab]

data LvalElab = LvalElab
    { lvalElabIdentifier :: Token
    , lvalElabOps :: [LvalElabOpCat]
    }
    deriving Show

data LvalElabOpCat
    = DEREF_LVALOP_ELAB
    deriving Show

data ExpElab
    = CONST_ELAB Const
    | IDENTIFIER_ELAB Token
    | BINOP_ELAB BinopElab
    | LOG_BINOP_ELAB LogBinopElab
    | UNOP_ELAB UnopElab
    | TERN_ELAB TernopElab
    | FN_CALL_ELAB FunctionCallElab
    | REF_LVAL_EXP_ELAB
    deriving Show

data BinopElab = BinopElab
    { binopElabCat :: BinopCatElab
    , binopElabOp :: Token
    , binopElabExp1 :: ExpElab
    , binopElabExp2 :: ExpElab
    }
    deriving Show

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
    deriving Show

data LogBinopElab = LogBinopElab
    { logBinopElabCat :: LogBinopCatElab
    , logBinopElabOp :: Token
    , logBinopElabExp1 :: ExpElab
    , logBinopElabExp2 :: ExpElab
    }
    deriving Show

data LogBinopCatElab
    = LOGAND_EXP_ELAB
    | LOGOR_EXP_ELAB
    deriving Show


data UnopElab = UnopElab
    { unopElabCat :: UnopCatElab
    , unopElabOp :: Token
    , unopElabExp :: ExpElab
    }
    deriving Show

data UnopCatElab
    = NEG_EXP_ELAB
    | NOT_EXP_ELAB
    | LOGNOT_EXP_ELAB
    | DEREF_EXP_ELAB
    deriving Show

data TernopElab = TernopElab
    { ternopElabOp :: Token
    , ternopElabCondExp :: ExpElab
    , ternopElabExp1 :: ExpElab
    , ternopElabExp2 :: ExpElab
    }
    deriving Show

data FunctionCallElab = FunctionCallElab
    { functionCallElabName :: Token
    , functionCallElabArgs :: [ExpElab]
    }
    deriving Show

data VariableElab = VariableElab
    { variableElabIdentifier :: Token
    , variableElabType :: TypeElab
    }
    deriving Show

data TypeElab = TypeElab
    { typeElabType :: TypeCategory
    , typeElabToken :: Token
    }
    deriving (Show)
instance Eq TypeElab where
    t1 == t2 = (typeElabType t1) == (typeElabType t2)

-- HELPERS

extractIdentifierName :: Token -> String
extractIdentifierName token =
    case (tokenCat token) of
        IDENTIFIER name -> name
        _ -> error (compilerError "Expected an identifer token but got token=" ++ (show token))

isMainFunction :: FunctionSignatureElab -> Bool
isMainFunction fnSignElab = 
    ((extractIdentifierName . functionSignatureElabName $ fnSignElab) == "main")
        && (length . functionSignatureElabArgs $ fnSignElab) == 0
        && (typeElabType . functionSignatureElabRetType $ fnSignElab) == INT_TYPE

generateFnIdentifier :: String -> String -> Int -> String
generateFnIdentifier progIdentifier fnName fnIndex = progIdentifier ++ "_" ++ fnName ++ "_" ++ (show fnIndex)
