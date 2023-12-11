module Ir (
    Program,
    BasicBlock,
    Command (..),
    ImpureBinopIr (..),
    PureBinopIr (..),
    PureIr (..),
)

where

import qualified Data.Map as Map

type ProgramIr = Map LineIr BasicBlockIr

type BasicBlockIr = [Command]

data CommandIr
    = ASN_PURE_IR VariableIr PureIr
    | ASN_IMPURE_BINOP_IR VariableIr ImpureBinopIr
    | GOTO_LINE_IR LineIr
    | DEF_LINE_IR LineIr
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



-- command-level types

type VariableIr = Int

type LineIr = String
