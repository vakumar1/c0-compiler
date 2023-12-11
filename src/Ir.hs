module Ir (

)

where

import Elaborated
import IrTree

import qualified Data.Map as Map

type RegData = RegData 
    { regCtr :: Int
    , regMap :: Map String Int
    }


-- EXPRESSIONS IR

translateIrExp :: ExpElab -> RegData -> ([Command], PureIr, RegData)
translateIrExp e rData = 
    case e of
        CONST_ELAB const -> translateIrConst const rData
        IDENTIFIER_ELAB identTok -> translateIrIdentifier rData
        PURE_BINOP_ELAB binop -> translateIrBinop binop rData
        IMPURE_BINOP_ELAB binop -> translateIrBinop binop rData
        PURE_UNOP_ELAB unop -> translateIrUnop unop rData
        IMPURE_UNOP_ELAB unop -> translateIrUnop unop rData

translateIrConst :: Const -> RegData -> ([Command], PureIr, RegData)
translateIrConst const rData = ([], CONST_IR const, rData)

translateIrIdentifier :: Token -> RegData -> ([Command], PureIr, RegData)
translateIrIdentifier identTok rData = 
    case (extractIdentifierName identifier) of
        Just name -> 
            case (Map.lookup name (regMap rData)) of
                Just i -> ([], VAR_IR i, rData)
                Nothing -> 
                    let var = (regCtr rData)
                        newRegCtr = (regCtr rData) + 1
                        newRegMap = (Map.insert name (regCtr rData))
                    in ([], VAR_IR var, RegData newRegCtr newRegMap)

translateIrBinop :: BinopElab -> RegData -> ([Command], PureIr, RegData)
translateIrBinop binop rData = 
    case binop of
        ADD_EXP_ELAB e1 e2 -> 
            let (c1, p1, p2, r1) = generateAndConcatSubCommands e1 e2 rData
                p = PURE_BINOP_IR (PureBinopIr ADD_IR p1 p2)
            in (c1, p, r1)
        SUB_EXP_ELAB e1 e2 -> 
            let (c1, p1, p2, r1) = generateAndConcatSubCommands e1 e2 rData
                p = PURE_BINOP_IR (PureBinopIr SUB_IR p1 p2)
            in (c1, p, r1)
        MUL_EXP_ELAB e1 e2 -> 
            let (c1, p1, p2, r1) = generateAndConcatSubCommands e1 e2 rData
                p = PURE_BINOP_IR (PureBinopIr MUL_IR p1 p2)
            in (c1, p, r1)
        DIV_EXP_ELAB e1 e2 -> 
            let (c1, p1, p2, r1) = generateAndConcatSubCommands e1 e2 rData
                de = DIV_IR p1 p2
                v = (regCtr rData)
                c2 = (ASN_IMPURE_BINOP (v de)) : c1
            in (c2, VAR_IR v, RegData ((regCtr rData) + 1) (regMap rData))
        MOD_EXP_ELAB e1 e2 -> 
            let (c1, p1, p2, r1) = generateAndConcatSubCommands e1 e2 rData
                me = MOD_IR p1 p2
                v = (regCtr rData)
                c2 = (ASN_IMPURE_BINOP (v me)) : c1
            in (c2, VAR_IR v, RegData ((regCtr rData) + 1) (regMap rData))

translateIrUnop :: UnopElab -> RegData -> ([Command], PureIr, RegData)
translateIrUnop unop rData = 
    case unop of
        NEG_EXP_ELAB e ->
            let (c1, p1, r1) = translateIrExp e rData
                p = PURE_UNOP_IR (PureUnopIr NEG_IR p1)


-- HELPERS

generateAndConcatSubCommands :: ExpElab -> ExpElab -> RegData -> ([Command], PureIr, PureIr, RegData)
generateAndConcatSubCommands e1 e2 rData = 
    let (c1, p1, r1) = translateIrExp e1 rData
        (c2, p2, r2) = translateIrExp e2 r1
    in (c2 ++ c1, p1, p2, r2)
