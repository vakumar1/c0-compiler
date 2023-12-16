module Ir (
    BasicBlockIr (..),
    CommandIr (..),
    PureIr (..),
    ImpureBinopIr (..),
    PureBinopIr (..),
    PureUnopIr (..),
    VariableIr (..),
)
where

import Types

data BasicBlockIr = BasicBlockIr
    { bbIrArgs :: [VariableIr]
    , bbIrCommands :: [CommandIr]
    }

data CommandIr
    = INIT_IR VariableIr
    | ASN_PURE_IR VariableIr PureIr
    | ASN_IMPURE_BINOP_IR VariableIr ImpureBinopIr
    | GOTO_BB_IR BasicBlockIr
    | RET_PURE_IR PureIr

data PureIr
    = CONST_IR Const
    | VAR_IR VariableIr
    | PURE_BINOP_IR PureBinopIr
    | PURE_UNOP_IR PureUnopIr

data ImpureBinopIr
    = DIV_IR PureIr PureIr
    | MOD_IR PureIr PureIr

data PureBinopIr
    = ADD_IR PureIr PureIr
    | SUB_IR PureIr PureIr
    | MUL_IR PureIr PureIr

data PureUnopIr
    = NEG_IR PureIr

data VariableIr = VariableIr
    { variableIrName :: String
    , variableIrType :: TypeCategory
    , variableIrTemp :: Bool
    }
