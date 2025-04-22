module Middleend.Peephole.ConstantFolding (
    constantFoldingCycle,
)
where

import Model.Ir
import Model.Types
import Common.Aliasing
import Common.Errors
import Common.IrProcessor

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import qualified Data.Bits as Bits

-- TODO: add NOP command IR and replace all constant-replaced assignments with NOP

constantFoldingCycle :: AliasingCtx -> FunctionIr -> FunctionIr
constantFoldingCycle aliasingCtx fnIr = 
    let (fnIr', foldingMd) = constantFoldingPass fnIr
        (_, idMd) = constantIdentificationPass aliasingCtx fnIr'
        (fnIr'', _) = constantReplacementPass (constantIdentificationMetadataConstantVars idMd) fnIr'
    in
        if ((not foldingMd) || (Map.null $ constantIdentificationMetadataConstantVars idMd))
            then fnIr
            else constantFoldingCycle aliasingCtx fnIr''

-- CONSTANT FOLDING PASS - replace all constant binops/unops with reduced constants

type ConstantFoldingMetadata = Bool

constantFoldingPass :: FunctionIr -> (FunctionIr, Bool)
constantFoldingPass fnIr = 
    let handlers = 
            IrProcessorHandlers
                Nothing
                Nothing
                (Just constantFoldingCommandIrProcessor)
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
    in processorPass handlers fnIr False

constantFoldingCommandIrProcessor :: CommandIr -> ConstantFoldingMetadata -> (CommandIr, ConstantFoldingMetadata)
constantFoldingCommandIrProcessor comm md = 
    case comm of
        ASN_PURE_IR memop asnVar asnPure -> 
            let (asnPure', md') = constantFoldingPureIrProcessor asnPure md
            in (ASN_PURE_IR memop asnVar asnPure', md')
        ASN_IMPURE_IR asnVar asnImpure -> 
            let (asnImpure', md') = constantFoldingImpureIrProcessor asnImpure md
            in 
                case asnImpure' of
                    Left pu ->
                        (ASN_PURE_IR emptyMemopIr asnVar pu, md')
                    Right impure ->
                        (ASN_IMPURE_IR asnVar impure, md')
        SPLIT_BB_IR condPure splitLeft splitRight -> 
            let (condPure', md') = constantFoldingPureIrProcessor condPure md
            in (SPLIT_BB_IR condPure' splitLeft splitRight, md')
        RET_PURE_IR retPure -> 
            let (retPure', md') = constantFoldingPureIrProcessor retPure md
            in (RET_PURE_IR retPure', md')
        _ ->
            (comm, md)

constantFoldingPureIrProcessor :: PureIr -> ConstantFoldingMetadata -> (PureIr, ConstantFoldingMetadata)
constantFoldingPureIrProcessor pure md = 
    case pure of
        PURE_BINOP_IR (PureBinopIr cat ty base1 base2) -> 
            case (base1, base2) of
                (CONST_IR c1, CONST_IR c2) ->
                    (PURE_BASE_IR (CONST_IR (binopReduce cat c1 c2)), True)
                _ ->
                    (pure, md)
        PURE_UNOP_IR (PureUnopIr cat ty base) -> 
            case base of
                CONST_IR c ->
                    (PURE_BASE_IR (CONST_IR (unopReduce cat c)), True)
                _ ->
                    (pure, md)
        _ ->
            (pure, md)

constantFoldingImpureIrProcessor :: ImpureIr -> ConstantFoldingMetadata -> (Either PureIr ImpureIr, ConstantFoldingMetadata)
constantFoldingImpureIrProcessor impure md = 
    case impure of
        IMPURE_BINOP_IR (ImpureBinopIr cat ty base1 base2) -> do
            case (base1, base2) of
                (CONST_IR c1, CONST_IR c2) ->
                    if (impureBinopValidReduce cat c1 c2)
                        then
                            (Left (PURE_BASE_IR (CONST_IR (impureBinopReduce cat c1 c2))), True)
                        else
                            (Right impure, md)
                _ ->
                    (Right impure, md)
        _ ->
            (Right impure, md)
            

-- CONSTANT REPLACEMENT PASS - replace all single constant vars (e.g. t = 0) for atomic non-referenced vars with the constant:
-- - identify all atomic non-referenced constant vars
-- - replace all instances of atomic non-referenced constant vars with the constant

data ConstantIdentificationMetadata = ConstantIdentificationMetadata
    { constantIdentificationMetadataConstantVars :: Map.Map VariableIr Const
    , constantIdentificationMetadataAliasingCtx :: AliasingCtx
    }

constantIdentificationPass :: AliasingCtx -> FunctionIr -> (FunctionIr, ConstantIdentificationMetadata)
constantIdentificationPass aliasingCtx fnIr = 
    let handlers = 
            IrProcessorHandlers
                Nothing
                Nothing
                (Just constantIdentificationCommandIrProcessor)
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
    in processorPass handlers fnIr (ConstantIdentificationMetadata Map.empty aliasingCtx)

constantIdentificationCommandIrProcessor :: CommandIr -> ConstantIdentificationMetadata -> (CommandIr, ConstantIdentificationMetadata)
constantIdentificationCommandIrProcessor comm md = 
    case comm of
        ASN_PURE_IR memop asnVar asnPure -> 
            if (Set.member asnVar (aliasingCtxStackVars $ constantIdentificationMetadataAliasingCtx md))
                || (memopIrIsDeref memop)
                || (Maybe.isJust $ memopIrOffset memop)
                then
                    (comm, md)
                else
                    case asnPure of
                        PURE_BASE_IR (CONST_IR c) ->
                            (comm,
                                ConstantIdentificationMetadata
                                    (Map.insert asnVar c (constantIdentificationMetadataConstantVars md))
                                    (constantIdentificationMetadataAliasingCtx md))
                        _ ->
                            (comm, md)
        _ ->
            (comm, md)

data ConstantReplacementMetadata = ConstantReplacementMetadata
    { constantReplacementMetadataConstantVars :: Map.Map VariableIr Const
    }

constantReplacementPass :: Map.Map VariableIr Const -> FunctionIr -> (FunctionIr, ConstantReplacementMetadata)
constantReplacementPass constantVars fnIr = 
    let handlers = 
            IrProcessorHandlers
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                (Just constantReplacementPureBaseIrProcessor)
                Nothing
    in processorPass handlers fnIr (ConstantReplacementMetadata constantVars)

constantReplacementPureBaseIrProcessor :: PureBaseIr -> ConstantReplacementMetadata -> (PureBaseIr, ConstantReplacementMetadata)
constantReplacementPureBaseIrProcessor base md = 
    case base of
        CONST_IR _ ->
            (base, md)
        VAR_IR baseVar -> 
            case (Map.lookup baseVar (constantReplacementMetadataConstantVars md)) of
                Just c ->
                    (CONST_IR c, md)
                Nothing ->
                    (base, md)

-- BINOP/UNOP REDUCTION HELPERS

impureBinopValidReduce :: ImpureBinopCatIr -> Const -> Const -> Bool
impureBinopValidReduce cat c1 c2 = 
    case (c1, c2) of
        (INT_CONST i1, INT_CONST i2) ->
            i2 /= 0
        _ ->
            error . compilerError $ "Attempted to reduce impure binop for unsupported constant types"

impureBinopReduce :: ImpureBinopCatIr -> Const -> Const -> Const
impureBinopReduce cat c1 c2 = 
    case (impureBinopReduceHelper cat c1 c2) of
        Just c -> c
        Nothing -> error . compilerError $ "Attempted to reduce impure binop for unsupported constant types"

impureBinopReduceHelper :: ImpureBinopCatIr -> Const -> Const -> Maybe Const
impureBinopReduceHelper cat c1 c2 =
    case cat of
        DIV_IR ->
            case (c1, c2) of
                (INT_CONST i1, INT_CONST i2) -> Just (INT_CONST (quot i1 i2))
                _ -> Nothing
        MOD_IR ->
            case (c1, c2) of
                (INT_CONST i1, INT_CONST i2) -> Just (INT_CONST (rem i1 i2))
                _ -> Nothing

binopReduce :: PureBinopCatIr -> Const -> Const -> Const
binopReduce cat c1 c2 = 
    case (binopReduceHelper cat c1 c2) of
        Just c -> c
        Nothing -> error . compilerError $ "Attempted to reduce binop for unsupported constant types"

binopReduceHelper :: PureBinopCatIr -> Const -> Const -> Maybe Const
binopReduceHelper cat c1 c2 =
    case cat of
        ADD_IR ->
            case (c1, c2) of
                (INT_CONST i1, INT_CONST i2) -> Just (INT_CONST (i1 + i2))
                _ -> Nothing
        SUB_IR ->
            case (c1, c2) of
                (INT_CONST i1, INT_CONST i2) -> Just (INT_CONST (i1 - i2))
                _ -> Nothing
        MUL_IR ->
            case (c1, c2) of
                (INT_CONST i1, INT_CONST i2) -> Just (INT_CONST (i1 * i2))
                _ -> Nothing
        AND_IR ->
            case (c1, c2) of
                (INT_CONST i1, INT_CONST i2) -> Just (INT_CONST (i1 Bits..&. i2))
                _ -> Nothing
        XOR_IR ->
            case (c1, c2) of
                (INT_CONST i1, INT_CONST i2) -> Just (INT_CONST (Bits.xor i1 i2))
                _ -> Nothing
        OR_IR ->
            case (c1, c2) of
                (INT_CONST i1, INT_CONST i2) -> Just (INT_CONST (i1 Bits..|. i2))
                _ -> Nothing
        SAL_IR ->
            case (c1, c2) of
                (INT_CONST i1, INT_CONST i2) -> Just (INT_CONST (Bits.shiftL i1 i2))
                _ -> Nothing
        SAR_IR ->
            case (c1, c2) of
                (INT_CONST i1, INT_CONST i2) -> Just (INT_CONST (Bits.shiftR i1 i2))
                _ -> Nothing
        LT_IR ->
            case (c1, c2) of
                (INT_CONST i1, INT_CONST i2) -> Just (BOOL_CONST (i1 < i2))
                _ -> Nothing
        GT_IR ->
            case (c1, c2) of
                (INT_CONST i1, INT_CONST i2) -> Just (BOOL_CONST (i1 > i2))
                _ -> Nothing
        LTE_IR ->
            case (c1, c2) of
                (INT_CONST i1, INT_CONST i2) -> Just (BOOL_CONST (i1 <= i2))
                _ -> Nothing
        GTE_IR ->
            case (c1, c2) of
                (INT_CONST i1, INT_CONST i2) -> Just (BOOL_CONST (i1 >= i2))
                _ -> Nothing
        EQ_IR ->
            case (c1, c2) of
                (INT_CONST i1, INT_CONST i2) -> Just (BOOL_CONST (i1 == i2))
        NEQ_IR ->
            case (c1, c2) of
                (INT_CONST i1, INT_CONST i2) -> Just (BOOL_CONST (i1 /= i2))
                _ -> Nothing

unopReduce :: PureUnopCatIr -> Const -> Const
unopReduce cat c1 = 
    case (unopReduceHelper cat c1) of
        Just c -> c
        Nothing -> error . compilerError $ "Attempted to reduce unop for unsupported constant types"

unopReduceHelper :: PureUnopCatIr -> Const -> Maybe Const
unopReduceHelper cat c1 =
    case cat of
        NEG_IR ->
            case c1 of
                INT_CONST i1 -> Just (INT_CONST (-i1))
                _ -> Nothing
        NOT_IR ->
            case c1 of
                INT_CONST i1 -> Just (INT_CONST (Bits.complement i1))
                _ -> Nothing
        LOGNOT_IR ->
            case c1 of
                BOOL_CONST b1 -> Just (BOOL_CONST (not b1))
                _ -> Nothing
        REF_IR -> 
            Nothing
