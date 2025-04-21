module Common.IrProcessor (
    processorPass,
    IrProcessorHandlers (..),
)
where

import Model.Ir
import Common.IrUtils

import qualified Control.Monad.State as State
import qualified Data.Map as Map

data IrProcessorHandlers metadata = IrProcessorHandlers
    { irProcessorHandlersFnIr           :: Maybe (FunctionIr -> metadata -> (FunctionIr, metadata))
    , irProcessorHandlersBbIr           :: Maybe (BasicBlockIr -> metadata -> (BasicBlockIr, metadata))
    , irProcessorHandlersCommandIr      :: Maybe (CommandIr -> metadata -> (CommandIr, metadata))
    , irProcessorHandlersPureIr         :: Maybe (PureIr -> metadata -> (PureIr, metadata))
    , irProcessorHandlersMemopIr        :: Maybe (MemopIr -> metadata -> (MemopIr, metadata))
    , irProcessorHandlersImpureIr       :: Maybe (ImpureIr -> metadata -> (ImpureIr, metadata))
    , irProcessorHandlersPureBaseIr     :: Maybe (PureBaseIr -> metadata -> (PureBaseIr, metadata))
    , irProcessorHandlersVarIr          :: Maybe (VariableIr -> metadata -> (VariableIr, metadata))
    }

processorPass :: IrProcessorHandlers metadata -> FunctionIr -> metadata -> (FunctionIr, metadata)
processorPass handlers fnIr initState = 
    State.runState (fnIrProcessor handlers fnIr) initState

fnIrProcessor :: IrProcessorHandlers metadata -> FunctionIr -> State.State metadata FunctionIr
fnIrProcessor handlers fnIr = 
    case (irProcessorHandlersFnIr handlers) of
        Just f ->
            State.state (f fnIr)
        Nothing -> do
            bs <- mapM (bbIrProcessor handlers) (Map.elems $ functionIrBlocks fnIr)
            return $ addBbsToFunction bs fnIr

bbIrProcessor :: IrProcessorHandlers metadata -> BasicBlockIr -> State.State metadata BasicBlockIr
bbIrProcessor handlers bbIr = 
    case (irProcessorHandlersBbIr handlers) of
        Just f ->
            State.state (f bbIr)
        Nothing -> do
            cs <- mapM (commandIrProcessor handlers) (bbIrCommands bbIr)
            return
                (BasicBlockIr
                    (bbIrFnName bbIr)
                    (bbIndex bbIr)
                    (bbIrPhiFn bbIr)
                    cs)

commandIrProcessor :: IrProcessorHandlers metadata -> CommandIr -> State.State metadata CommandIr
commandIrProcessor handlers comm = 
    case (irProcessorHandlersCommandIr handlers) of
        Just f ->
            State.state (f comm)
        Nothing ->
            case comm of
                INIT_IR var -> do
                    v <- varIrProcessor handlers var
                    return (INIT_IR v)
                ASN_PURE_IR memop asnVar asnPure -> do
                    m <- memopIrProcessor handlers memop
                    av <- varIrProcessor handlers asnVar
                    ap <- pureIrProcessor handlers asnPure
                    return (ASN_PURE_IR m av ap)
                ASN_IMPURE_IR asnVar asnImpure -> do
                    av <- varIrProcessor handlers asnVar
                    ai <- impureIrProcessor handlers asnImpure
                    return (ASN_IMPURE_IR av ai)
                GOTO_BB_IR _ ->
                    return comm
                SPLIT_BB_IR condPure splitLeft splitRight -> do
                    cp <- pureIrProcessor handlers condPure
                    return (SPLIT_BB_IR cp splitLeft splitRight)
                RET_PURE_IR retPure -> do
                    rp <- pureIrProcessor handlers retPure
                    return (RET_PURE_IR rp)
                RET_IR ->
                    return comm
                ABORT_IR ->
                    return comm

pureIrProcessor :: IrProcessorHandlers metadata -> PureIr -> State.State metadata PureIr
pureIrProcessor handlers pure = 
    case (irProcessorHandlersPureIr handlers) of
        Just f ->
            State.state (f pure)
        Nothing -> 
            case pure of
                PURE_BASE_IR base -> do
                    b <- pureBaseIrProcessor handlers base
                    return (PURE_BASE_IR b)
                PURE_BINOP_IR (PureBinopIr cat ty base1 base2) -> do
                    b1 <- pureBaseIrProcessor handlers base1
                    b2 <- pureBaseIrProcessor handlers base2
                    return (PURE_BINOP_IR (PureBinopIr cat ty b1 b2))
                PURE_UNOP_IR (PureUnopIr cat ty base) -> do
                    b <- pureBaseIrProcessor handlers base
                    return (PURE_UNOP_IR (PureUnopIr cat ty b))
                PURE_MEMOP_IR var memop -> do
                    v <- varIrProcessor handlers var
                    m <- memopIrProcessor handlers memop
                    return (PURE_MEMOP_IR v m)

memopIrProcessor :: IrProcessorHandlers metadata -> MemopIr -> State.State metadata MemopIr
memopIrProcessor handlers memop = 
    case (irProcessorHandlersMemopIr handlers) of
        Just f ->
            State.state (f memop)
        Nothing ->
            case memopIrOffset memop of
                Just offsetPuB -> do
                    opb <- pureBaseIrProcessor handlers offsetPuB
                    return
                        (MemopIr
                            (memopIrIsDeref memop)
                            (Just opb)
                            (memopIrRetType memop))
                Nothing ->
                    return memop

impureIrProcessor :: IrProcessorHandlers metadata -> ImpureIr -> State.State metadata ImpureIr
impureIrProcessor handlers impure = 
    case (irProcessorHandlersImpureIr handlers) of
        Just f ->
            State.state (f impure)
        Nothing ->
            case impure of
                IMPURE_BINOP_IR (ImpureBinopIr cat ty base1 base2) -> do
                    b1 <- pureBaseIrProcessor handlers base1
                    b2 <- pureBaseIrProcessor handlers base2
                    return (IMPURE_BINOP_IR (ImpureBinopIr cat ty b1 b2))
                IMPURE_FNCALL_IR (ImpureFnCallIr name argPuB ty) -> do
                    apbs <- mapM (pureBaseIrProcessor handlers) argPuB
                    return (IMPURE_FNCALL_IR (ImpureFnCallIr name apbs ty))

pureBaseIrProcessor :: IrProcessorHandlers metadata -> PureBaseIr -> State.State metadata PureBaseIr
pureBaseIrProcessor handlers base = 
    case (irProcessorHandlersPureBaseIr handlers) of
        Just f ->
            State.state (f base)
        Nothing ->
            case base of
                CONST_IR _ ->
                    return base
                VAR_IR baseVar -> do
                    v <- varIrProcessor handlers baseVar
                    return (VAR_IR v)

varIrProcessor :: IrProcessorHandlers metadata -> VariableIr -> State.State metadata VariableIr
varIrProcessor handlers var = 
    case (irProcessorHandlersVarIr handlers) of
        Just f ->
            State.state (f var)
        Nothing ->
            return var
