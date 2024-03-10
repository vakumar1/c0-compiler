module Backend.Codegen (
    irToX86,
)
where

import Model.Ir
import Common.Liveness
import Model.Types
import Model.X86
import Common.Errors

import qualified Data.Map as Map
import qualified Data.Set as Set

irToX86 :: Map.Map VariableIr Int -> FunctionIr -> [X86Instruction]
irToX86 coloring fnIr =
    let blocks = bfsSuccessors fnIr
        -- initialize function spillover at SP - 8 and w/ all available registers
        initAlloc = AllocState Map.empty 8 availableRegisters
        (instBlockMap, _) =
            foldl
                ( \(interBlockMap, interAlloc) bbIndex ->
                    case Map.lookup bbIndex (functionIrBlocks fnIr) of
                        Just bb -> bbIrToX86 coloring bb interBlockMap interAlloc
                        Nothing -> (interBlockMap, interAlloc)
                )
                (Map.empty, initAlloc)
                blocks
        finalInstr = 
            foldl
                (\interInstr index ->
                    case Map.lookup index instBlockMap of
                        Just inst -> interInstr ++ inst
                        Nothing -> interInstr
                )
                []
                blocks
     in finalInstr

bbIrToX86 :: Map.Map VariableIr Int -> BasicBlockIr -> Map.Map Int [X86Instruction] -> AllocState -> (Map.Map Int [X86Instruction], AllocState)
bbIrToX86 coloring bb initBlockMap initAlloc =
    let (updatePhiBlockMap, updatePhiAlloc) = phiFnIrToX86 coloring (bbIrPhiFn bb) initBlockMap initAlloc
        (bbInst, updateBbAlloc) = 
            foldr
                ( \comm (interInstr, interAlloc) ->
                    let (commInstr, newAlloc) = commIrToX86 coloring comm interAlloc
                    in (interInstr ++ commInstr, newAlloc)
                )
                ([LABEL_X86 (bbToLabel (bbIndex bb))], updatePhiAlloc)
                (bbIrCommands bb)
        updateBbBlockMap = Map.insert (bbIndex bb) bbInst updatePhiBlockMap
    in (updateBbBlockMap, updateBbAlloc)

phiFnIrToX86 :: Map.Map VariableIr Int -> PhiFnIr -> Map.Map Int [X86Instruction] -> AllocState -> (Map.Map Int [X86Instruction], AllocState)
phiFnIrToX86 coloring phi initBlockMap initAlloc = 
    foldr
        (\(var, varPredMap) (interBlockMap, interAlloc) ->
            foldr
                (\(predIndex, predVar) (predInterBlockMap, predInterAlloc)->
                    phiFnArgToX86 coloring var (predIndex, predVar) predInterBlockMap predInterAlloc
                )
                (interBlockMap, interAlloc)
                (Map.toList varPredMap)
        )
        (initBlockMap, initAlloc)
        (Map.toList phi)

phiFnArgToX86 :: Map.Map VariableIr Int -> VariableIr -> (Int, VariableIr) -> Map.Map Int [X86Instruction] -> AllocState -> (Map.Map Int [X86Instruction], AllocState)
phiFnArgToX86 coloring asnVar (predIndex, predVar) initBlockMap initAlloc = 
    case Map.lookup predIndex initBlockMap of
        Just currInst ->
            let (asnVarLoc, asnAlloc) =
                    case Map.lookup asnVar coloring of
                        Just color ->
                            case Map.lookup color (allocStateRegMap initAlloc) of
                                Just argLoc -> (argLoc, initAlloc)
                                Nothing -> allocColor color initAlloc
                predVarLoc =
                    case Map.lookup predVar coloring of
                        Just color -> getColorReg color asnAlloc
                asnInst = 
                    case predVarLoc of
                        REG_ARGLOC pureVarReg ->
                            [ MOV_X86 asnVarLoc predVarLoc
                            ]
                        STACK_ARGLOC pureVarStackLoc ->
                            [ MOV_X86 (REG_ARGLOC DX) predVarLoc
                            , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                            ]
                
                newInst = (init currInst) ++ asnInst ++ [(last currInst)]
                newBlockMap = Map.insert predIndex newInst initBlockMap
            in (newBlockMap, asnAlloc)
        Nothing -> error (compilerError ("Invalid phi-function generated for predecessor without translation. " ++
                        "Predecessor" ++ (show predIndex) ++ " Successor var=" ++ (show asnVar)))

commIrToX86 :: Map.Map VariableIr Int -> CommandIr -> AllocState -> ([X86Instruction], AllocState)
commIrToX86 coloring comm initAlloc =
    case comm of
        INIT_IR var -> ([], initAlloc)
        ASN_PURE_IR asnVar asnPure -> asnPureIrToX86 coloring asnVar asnPure initAlloc
        ASN_IMPURE_IR asnVar asnImpure -> asnImpureIrToX86 coloring asnVar asnImpure initAlloc
        GOTO_BB_IR bbIndex ->
            let instr = gotoToX86 bbIndex
             in (instr, initAlloc)
        RET_PURE_IR retPure ->
            let instr = retToX86 coloring retPure initAlloc
             in (instr, initAlloc)

asnPureIrToX86 :: Map.Map VariableIr Int -> VariableIr -> PureIr -> AllocState -> ([X86Instruction], AllocState)
asnPureIrToX86 coloring asnVar asnPure initAlloc =
    let (asnVarLoc, asnAlloc) =
            case Map.lookup asnVar coloring of
                Just color ->
                    case Map.lookup color (allocStateRegMap initAlloc) of
                        Just argLoc -> (argLoc, initAlloc)
                        Nothing -> allocColor color initAlloc
        inst =
            case asnPure of
                PURE_BASE_IR base ->
                    case base of
                        -- x = CONST
                        CONST_IR const ->
                            case const of
                                INT_CONST int ->
                                    [ MOV_X86 asnVarLoc (CONST_ARGLOC int)
                                    ]
                        -- x = VAR
                        VAR_IR pureVar ->
                            let pureVarLoc =
                                    case Map.lookup pureVar coloring of
                                        Just color -> getColorReg color asnAlloc
                             in case pureVarLoc of
                                    REG_ARGLOC pureVarReg ->
                                        [ MOV_X86 asnVarLoc pureVarLoc
                                        ]
                                    STACK_ARGLOC pureVarStackLoc ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        ]
                -- x = y ? z
                PURE_BINOP_IR (PureBinopIr cat ty base1 base2) ->
                    let pureVarLoc1 =
                            case base1 of
                                CONST_IR const ->
                                    case const of
                                        INT_CONST int -> CONST_ARGLOC int
                                VAR_IR pureVar ->
                                    case Map.lookup pureVar coloring of
                                        Just color -> getColorReg color asnAlloc
                        pureVarLoc2 =
                            case base2 of
                                CONST_IR const ->
                                    case const of
                                        INT_CONST int -> CONST_ARGLOC int
                                VAR_IR pureVar ->
                                    case Map.lookup pureVar coloring of
                                        Just color -> getColorReg color asnAlloc
                     in case cat of
                            ADD_IR ->
                                case asnVarLoc of
                                    -- reg = y + z -> perform binop in reg
                                    REG_ARGLOC asnVarReg ->
                                        [ MOV_X86 asnVarLoc pureVarLoc1
                                        , ADD_X86 asnVarLoc pureVarLoc2
                                        ]
                                    -- S[i] = y + z -> perform binop in DX and move to S[i]
                                    STACK_ARGLOC asnVarStackLoc ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , ADD_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        ]
                            SUB_IR ->
                                case asnVarLoc of
                                    -- reg = y - z -> perform binop in reg
                                    REG_ARGLOC asnVarReg ->
                                        [ MOV_X86 asnVarLoc pureVarLoc1
                                        , SUB_X86 asnVarLoc pureVarLoc2
                                        ]
                                    -- S[i] = y - z -> perform binop in DX and move to S[i]
                                    STACK_ARGLOC asnVarStackLoc ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , SUB_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        ]
                            MUL_IR ->
                                case asnVarLoc of
                                    -- reg = y * z -> perform binop in reg
                                    REG_ARGLOC asnVarReg ->
                                        [ MOV_X86 asnVarLoc pureVarLoc1
                                        , IMUL_X86 asnVarLoc pureVarLoc2
                                        ]
                                    -- S[i] = y * z -> perform binop in DX and move to S[i]
                                    STACK_ARGLOC asnVarStackLoc ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , IMUL_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        ]
                PURE_UNOP_IR (PureUnopIr cat ty base) ->
                    let pureVarLoc =
                            case base of
                                CONST_IR const ->
                                    case const of
                                        INT_CONST int -> CONST_ARGLOC int
                                VAR_IR pureVar ->
                                    case Map.lookup pureVar coloring of
                                        Just color -> getColorReg color asnAlloc
                     in case cat of
                            NEG_IR ->
                                [ MOV_X86 asnVarLoc pureVarLoc
                                , NEG_X86 asnVarLoc
                                ]
     in (inst, asnAlloc)

asnImpureIrToX86 :: Map.Map VariableIr Int -> VariableIr -> ImpureIr -> AllocState -> ([X86Instruction], AllocState)
asnImpureIrToX86 coloring asnVar asnImpure initAlloc =
    let (asnVarLoc, asnAlloc) =
            case Map.lookup asnVar coloring of
                Just color ->
                    case Map.lookup color (allocStateRegMap initAlloc) of
                        Just argLoc -> (argLoc, initAlloc)
                        Nothing -> allocColor color initAlloc
        inst =
            case asnImpure of
                IMPURE_BINOP_IR (ImpureBinopIr cat ty base1 base2) ->
                    let pureVarLoc1 =
                            case base1 of
                                CONST_IR const ->
                                    case const of
                                        INT_CONST int -> CONST_ARGLOC int
                                VAR_IR pureVar ->
                                    case Map.lookup pureVar coloring of
                                        Just color -> getColorReg color asnAlloc
                        pureVarLoc2 =
                            case base2 of
                                CONST_IR const ->
                                    case const of
                                        INT_CONST int -> CONST_ARGLOC int
                                VAR_IR pureVar ->
                                    case Map.lookup pureVar coloring of
                                        Just color -> getColorReg color asnAlloc
                     in case cat of
                            DIV_IR ->
                                case pureVarLoc2 of
                                    -- if divisor is a const temporarily push onto stack
                                    CONST_ARGLOC int ->
                                        [ XOR_X86 (REG_ARGLOC DX) (REG_ARGLOC DX) -- 0 out DX
                                        , MOV_X86 (REG_ARGLOC AX) pureVarLoc1 -- mov dividend to AX
                                        , PUSH_X86 pureVarLoc2 -- push divisor to stack
                                        , IDIV_X86 (STACK_ARGLOC 0) -- divide AX / S[0]
                                        , ADD_X86 (REG_ARGLOC SP) (CONST_ARGLOC registerSize) -- pop divisor from stack
                                        , MOV_X86 asnVarLoc (REG_ARGLOC AX) -- move quotient to result
                                        ]
                                    -- o/w use existing location
                                    _ ->
                                        [ XOR_X86 (REG_ARGLOC DX) (REG_ARGLOC DX) -- 0 out DX
                                        , MOV_X86 (REG_ARGLOC AX) pureVarLoc1 -- mov dividend to AX
                                        , IDIV_X86 pureVarLoc2 -- divide AX / divisor (in reg/on stack)
                                        , MOV_X86 asnVarLoc (REG_ARGLOC AX) -- move quotient to result
                                        ]
                            MOD_IR ->
                                case pureVarLoc2 of
                                    -- if divisor is a const temporarily push onto stack
                                    CONST_ARGLOC int ->
                                        [ XOR_X86 (REG_ARGLOC DX) (REG_ARGLOC DX) -- 0 out DX
                                        , MOV_X86 (REG_ARGLOC AX) pureVarLoc1 -- mov dividend to AX
                                        , PUSH_X86 pureVarLoc2 -- push divisor to stack
                                        , IDIV_X86 (STACK_ARGLOC 0) -- divide AX / S[0]
                                        , ADD_X86 (REG_ARGLOC SP) (CONST_ARGLOC registerSize) -- pop divisor from stack
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX) -- move remainder to result
                                        ]
                                    -- o/w use existing location
                                    _ ->
                                        [ XOR_X86 (REG_ARGLOC DX) (REG_ARGLOC DX) -- 0 out DX
                                        , MOV_X86 (REG_ARGLOC AX) pureVarLoc1 -- mov dividend to AX
                                        , IDIV_X86 pureVarLoc2 -- divide AX / divisor (in reg/on stack)
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX) -- move remainder to result
                                        ]
     in (inst, asnAlloc)

gotoToX86 :: Int -> [X86Instruction]
gotoToX86 bbIndex = [JMP_X86 (bbToLabel bbIndex)]

retToX86 :: Map.Map VariableIr Int -> PureIr -> AllocState -> [X86Instruction]
retToX86 coloring retPure initAlloc =
    case retPure of
        PURE_BASE_IR base ->
            case base of
                -- ret CONST
                CONST_IR const ->
                    case const of
                        INT_CONST int ->
                            [ MOV_X86 (REG_ARGLOC AX) (CONST_ARGLOC int)
                            , RET_X86
                            ]
                -- ret VAR
                VAR_IR pureVar ->
                    let pureVarLoc =
                            case Map.lookup pureVar coloring of
                                Just color -> getColorReg color initAlloc
                     in [ MOV_X86 (REG_ARGLOC AX) pureVarLoc
                        , RET_X86
                        ]
        -- ret y ? z
        PURE_BINOP_IR (PureBinopIr cat ty base1 base2) ->
            let pureVarLoc1 =
                    case base1 of
                        CONST_IR const ->
                            case const of
                                INT_CONST int -> CONST_ARGLOC int
                        VAR_IR pureVar ->
                            case Map.lookup pureVar coloring of
                                Just color -> getColorReg color initAlloc
                pureVarLoc2 =
                    case base2 of
                        CONST_IR const ->
                            case const of
                                INT_CONST int -> CONST_ARGLOC int
                        VAR_IR pureVar ->
                            case Map.lookup pureVar coloring of
                                Just color -> getColorReg color initAlloc
             in case cat of
                    ADD_IR ->
                        [ MOV_X86 (REG_ARGLOC AX) pureVarLoc1
                        , ADD_X86 (REG_ARGLOC AX) pureVarLoc2
                        , RET_X86
                        ]
                    SUB_IR ->
                        [ MOV_X86 (REG_ARGLOC AX) pureVarLoc1
                        , SUB_X86 (REG_ARGLOC AX) pureVarLoc2
                        , RET_X86
                        ]
                    MUL_IR ->
                        [ MOV_X86 (REG_ARGLOC AX) pureVarLoc1
                        , IMUL_X86 (REG_ARGLOC AX) pureVarLoc2
                        , RET_X86
                        ]
        PURE_UNOP_IR (PureUnopIr cat ty base) ->
            let pureVarLoc =
                    case base of
                        CONST_IR const ->
                            case const of
                                INT_CONST int -> CONST_ARGLOC int
                        VAR_IR pureVar ->
                            case Map.lookup pureVar coloring of
                                Just color -> getColorReg color initAlloc
             in case cat of
                    NEG_IR ->
                        [ MOV_X86 (REG_ARGLOC AX) pureVarLoc
                        , NEG_X86 (REG_ARGLOC AX)
                        , RET_X86
                        ]

-- HELPERS
data AllocState = AllocState
    { allocStateRegMap :: Map.Map Int ArgLocation
    , allocStateStackCtr :: Int
    , allocStateAvailableReg :: [Register]
    }

-- explicitly allocate a register/stack loc for new color on assignment
allocColor :: Int -> AllocState -> (ArgLocation, AllocState)
allocColor color initAlloc =
    case (allocStateAvailableReg initAlloc) of
        [] ->
            let argLoc = STACK_ARGLOC (allocStateStackCtr initAlloc)
                newAlloc =
                    AllocState
                        (Map.insert color argLoc (allocStateRegMap initAlloc))
                        ((allocStateStackCtr initAlloc) + registerSize)
                        (allocStateAvailableReg initAlloc)
             in (argLoc, newAlloc)
        reg : _ ->
            let argLoc = REG_ARGLOC reg
                newAlloc =
                    AllocState
                        (Map.insert color argLoc (allocStateRegMap initAlloc))
                        (allocStateStackCtr initAlloc)
                        (tail (allocStateAvailableReg initAlloc))
             in (argLoc, newAlloc)

-- lookup previously allocated color in reg alloc (errors if not found)
getColorReg :: Int -> AllocState -> ArgLocation
getColorReg color alloc =
    case Map.lookup color (allocStateRegMap alloc) of
        Just argLoc -> argLoc

bbToLabel :: Int -> Label
bbToLabel bbIndex =
    if bbIndex == 0
        then "main"
        else "l" ++ (show bbIndex)
