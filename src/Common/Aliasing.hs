module Common.Aliasing (
    AliasingCtx (..),
    aliasingAnalysisPass,
)
where

import Model.Ir
import Model.Types
import Common.Errors
import Common.IrUtils

import qualified Data.Set as Set

data AliasingCtx = AliasingCtx
    { aliasingCtxStackVars :: Set.Set VariableIr
    }
    deriving Show


aliasingAnalysisPass :: FunctionIr -> AliasingCtx
aliasingAnalysisPass fnIr = 
    AliasingCtx
        (forceStackPass fnIr)

-- REFERENCED VARS PASS
forceStackPass :: FunctionIr -> Set.Set VariableIr
forceStackPass fnIr =
    foldr
        (\index interVars -> 
            Set.union interVars (forceStackBBIr $ getBB fnIr index)
        )
        Set.empty
        [0..((length . functionIrBlocks $ fnIr) - 1)]

forceStackBBIr :: BasicBlockIr -> Set.Set VariableIr
forceStackBBIr bbIr =
    foldr
        (\commIr interVars ->
            case forceStackCommIr commIr of
                Just var -> Set.insert var interVars
                Nothing -> interVars
        )
        Set.empty
        (bbIrCommands bbIr)

forceStackCommIr :: CommandIr -> Maybe VariableIr
forceStackCommIr commIr = 
    case commIr of
        -- select all declared non-atomic vars
        INIT_IR varIr ->
            case (variableIrType varIr) of
                ARRAY_TYPE _ _ -> Just varIr
                STRUCT_TYPE _ -> Just varIr
                _ -> Nothing
        -- select all referenced vars
        ASN_PURE_IR _ _ (PURE_UNOP_IR (PureUnopIr REF_IR _ puB)) ->
            case puB of
                VAR_IR var -> Just var
                _ -> error . compilerError $ ("Created reference unop on non-variable pure base=" ++ (show puB))
        _ -> Nothing
