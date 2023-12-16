module Ir (
    BasicBlockIr (..),
    CommandIr (..),
    PureIr (..),
    PureBaseIr (..),
    PureBinopIr (..),
    PureBinopCatIr (..),
    PureUnopIr (..),
    PureUnopCatIr (..),
    ImpureIr (..),
    ImpureBinopIr (..),
    ImpureBinopCatIr (..),
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
    | ASN_IMPURE_IR VariableIr ImpureIr
    | GOTO_BB_IR BasicBlockIr
    | RET_PURE_IR PureIr

-- pure operations
data PureIr
    = PURE_BASE_IR PureBaseIr
    | PURE_BINOP_IR PureBinopIr
    | PURE_UNOP_IR PureUnopIr

data PureBaseIr
    = CONST_IR Const
    | VAR_IR VariableIr

data PureBinopIr = PureBinopIr
    { pureBinopCatIr :: PureBinopCatIr
    , pureBinopInfType :: TypeCategory
    , pureBinopBaseIr1 :: PureBaseIr
    , pureBinopBaseIr2 :: PureBaseIr
    }
data PureBinopCatIr
    = ADD_IR
    | SUB_IR
    | MUL_IR

data PureUnopIr = PureUnopIr
    { pureUnopCatIr :: PureUnopCatIr
    , pureUnopInfType :: TypeCategory
    , pureUnopBaseIr :: PureBaseIr
    }
data PureUnopCatIr
    = NEG_IR 

-- impure operations
data ImpureIr
    = IMPURE_BINOP_IR ImpureBinopIr

data ImpureBinopIr = ImpureBinopIr
    { impureBinopCatIr :: ImpureBinopCatIr
    , impureBinopInfType :: TypeCategory
    , impureBinopBaseIr1 :: PureBaseIr
    , impureBinopBaseIr2 :: PureBaseIr
    }
data ImpureBinopCatIr
    = DIV_IR
    | MOD_IR
    

data VariableIr = VariableIr
    { variableIrName :: String
    , variableIrType :: TypeCategory
    , variableIrTemp :: Bool
    }
