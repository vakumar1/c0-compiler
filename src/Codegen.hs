module Codegen (

)

where

import Ir
import Liveness
import X86

import qualified Data.Set as Set
import qualified Data.Map as Map


bbIrToX86 :: BasicBlockIr -> (Map.Map String Int, AllocState, [X86Instruction]) -> (Map.Map String Int, AllocState, [X86Instruction])
bbIrToX86 bb (coloring, initAlloc, initInstr) = 
    foldr (commIrToX86 coloring) (initAlloc, initInstr) (bbIrCommands bb)


commIrToX86 :: Map.Map String Int -> CommandIr -> AllocState -> (AllocState, [X86Instruction])
commIrToX86 coloring comm initAlloc = 
    case comm of
        INIT_IR var -> initIrToX86 var initAlloc
        ASN_PURE_IR asnVar asnPure -> asnPureIrToX86 asnVar asnPure
        ASN_IMPURE_IR asnVar asnImpure
        GOTO_BB_IR bbIndex
        RET_PURE_IR retPure

initIrToX86 :: VariableIr -> AllocState -> (AllocState, [X86Instruction])
initIrToX86 var initAlloc = 
    case Map.lookup (variableIrName var) coloring of
        Just color -> 
            case Map.lookup color (allocStateRegMap initAlloc) of
                Just _ -> (initAlloc, [])
                Nothing -> ((allocColor color initAlloc), [])

asnPureIrToX86 :: VariableIr -> PureIr -> AllocState -> [X86Instruction]
asnPureIrToX86 asnVar asnPure initAlloc = 
    let asnVarLoc = 
            case Map.lookup (variableIrName var) coloring of
                Just color -> getColorReg color initAlloc
    in case asnPure of
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
                        case Map.lookup (variableIrName var) coloring of
                            Just color -> getColorReg color initAlloc
                    in case pureVarLoc of
                        REG_ARGLOC pureVarReg ->
                            [ MOV_X86 asnVarLoc pureVarLoc
                            ]
                        STACK_ARGLOC pureVarStackLoc ->
                            [ MOV_X86 (REG_ARGLOC EDX) pureVarLoc
                            , MOV_X86 asnVarLoc (REG_ARGLOC EDX)
                            ]
                
        -- x = y ? z
        PURE_BINOP_IR (PureBinopIr cat ty base1 base2) ->
            let pureVarLoc1 = 
                    case base1 of
                        CONST_IR const ->
                            case const of
                                INT_CONST int -> CONST_ARGLOC int
                        VAR_IR pureVar ->
                            let pureVarLoc =
                                    case Map.lookup (variableIrName var) coloring of
                                        Just color -> getColorReg color initAlloc
                pureVarLoc2 = 
                    case base2 of
                        CONST_IR const ->
                            case const of
                                INT_CONST int -> CONST_ARGLOC int
                        VAR_IR pureVar ->
                            let pureVarLoc =
                                    case Map.lookup (variableIrName var) coloring of
                                        Just color -> getColorReg color initAlloc
            in case cat of
                ADD_IR -> 
                    case asnVarLoc of
                        -- reg = y + z -> perform binop in reg
                        REG_ARGLOC asnVarReg -> 
                            [ MOV_X86 asnVarLoc pureVarLoc1
                            , ADD_X86 asnVarLoc pureVarLoc2
                            ]
                        -- S[i] = y + z -> perform binop in EDX and move to S[i]
                        STACK_ARGLOC asnVarStackLoc ->
                            [ MOV_X86 (REG_ARGLOC EDX) pureVarLoc1
                            , ADD_X86 (REG_ARGLOC EDX) pureVarLoc2
                            , MOV_X86 asnVarLoc (REG_ARGLOC EDX)
                            ]
                SUB_IR -> 
                    case asnVarLoc of
                        -- reg = y - z -> perform binop in reg
                        REG_ARGLOC asnVarReg -> 
                            [ MOV_X86 asnVarLoc pureVarLoc1
                            , SUB_X86 asnVarLoc pureVarLoc2
                            ]
                        -- S[i] = y - z -> perform binop in EDX and move to S[i]
                        STACK_ARGLOC asnVarStackLoc ->
                            [ MOV_X86 (REG_ARGLOC EDX) pureVarLoc1
                            , SUB_X86 (REG_ARGLOC EDX) pureVarLoc2
                            , MOV_X86 asnVarLoc (REG_ARGLOC EDX)
                            ]
                MUL_IR ->
                    case asnVarLoc of
                        -- reg = y * z -> perform binop in reg
                        REG_ARGLOC asnVarReg -> 
                            [ MOV_X86 asnVarLoc pureVarLoc1
                            , IMUL_X86 asnVarLoc pureVarLoc2
                            ]
                        -- S[i] = y * z -> perform binop in EDX and move to S[i]
                        STACK_ARGLOC asnVarStackLoc ->
                            [ MOV_X86 (REG_ARGLOC EDX) pureVarLoc1
                            , IMUL_X86 (REG_ARGLOC EDX) pureVarLoc2
                            , MOV_X86 asnVarLoc (REG_ARGLOC EDX)
                            ]

        PURE_UNOP_IR (PureUnopIr cat ty base) ->
            let pureVarLoc = 
                    case base of
                        CONST_IR const ->
                            case const of
                                INT_CONST int -> CONST_ARGLOC int
                        VAR_IR pureVar ->
                            let pureVarLoc =
                                    case Map.lookup (variableIrName var) coloring of
                                        Just color -> getColorReg color initAlloc
            in case cat of
                NEG_IR ->
                    [ MOV_X86 asnVarLoc pureVarLoc
                    , NEG_X86 asnVarLoc
                    ]

asnImpureIrToX86 :: VariableIr -> ImpureIr -> AllocState -> [X86Instruction]
asnImpureIrToX86 asnVar asnImpure initAlloc = 
    let asnVarLoc = 
            case Map.lookup (variableIrName var) coloring of
                Just color -> getColorReg color initAlloc
    in case asnImpure of
        IMPURE_BINOP_IR (ImpureBinopIr cat ty base1 base2) ->
            let pureVarLoc1 = 
                    case base1 of
                        CONST_IR const ->
                            case const of
                                INT_CONST int -> CONST_ARGLOC int
                        VAR_IR pureVar ->
                            let pureVarLoc =
                                    case Map.lookup (variableIrName var) coloring of
                                        Just color -> getColorReg color initAlloc
                pureVarLoc2 = 
                    case base2 of
                        CONST_IR const ->
                            case const of
                                INT_CONST int -> CONST_ARGLOC int
                        VAR_IR pureVar ->
                            let pureVarLoc =
                                    case Map.lookup (variableIrName var) coloring of
                                        Just color -> getColorReg color initAlloc
            in case cat of
                DIV_IR -> 
                    case pureVarLoc2 of
                        -- if divisor is a const temporarily push onto stack
                        CONST_ARGLOC int ->
                            [ XOR_X86 (REG_ARGLOC EDX) (REG_ARGLOC EDX) -- 0 out EDX
                            , MOV_X86 (REG_ARGLOC EAX) pureVarLoc1      -- mov dividend to EAX
                            , PUSH_X86 pureVarLoc2                      -- push divisor to stack
                            , IDIV_X86 (STACK_ARGLOC 0)                 -- divide EAX / S[0]
                            , ADD_X86 (REG_ARGLOC ESP) 4                -- pop divisor from stack
                            , MOV_X86 asnVarLoc (REG_ARGLOC EAX)        -- move quotient to result
                            ]
                        -- o/w use existing location
                        _ ->
                            [ XOR_X86 (REG_ARGLOC EDX) (REG_ARGLOC EDX) -- 0 out EDX
                            , MOV_X86 (REG_ARGLOC EAX) pureVarLoc1      -- mov dividend to EAX
                            , IDIV_X86 pureVarLoc2                      -- divide EAX / divisor (in reg/on stack)
                            , MOV_X86 asnVarLoc (REG_ARGLOC EAX)        -- move quotient to result
                            ]

                MOD_IR ->
                    case pureVarLoc2 of
                        -- if divisor is a const temporarily push onto stack
                        CONST_ARGLOC int ->
                            [ XOR_X86 (REG_ARGLOC EDX) (REG_ARGLOC EDX) -- 0 out EDX
                            , MOV_X86 (REG_ARGLOC EAX) pureVarLoc1      -- mov dividend to EAX
                            , PUSH_X86 pureVarLoc2                      -- push divisor to stack
                            , IDIV_X86 (STACK_ARGLOC 0)                 -- divide EAX / S[0]
                            , ADD_X86 (REG_ARGLOC ESP) 4                -- pop divisor from stack
                            , MOV_X86 asnVarLoc (REG_ARGLOC EDX)        -- move remainder to result
                            ]
                        -- o/w use existing location
                        _ ->
                            [ XOR_X86 (REG_ARGLOC EDX) (REG_ARGLOC EDX) -- 0 out EDX
                            , MOV_X86 (REG_ARGLOC EAX) pureVarLoc1      -- mov dividend to EAX
                            , IDIV_X86 pureVarLoc2                      -- divide EAX / divisor (in reg/on stack)
                            , MOV_X86 asnVarLoc (REG_ARGLOC EDX)        -- move remainder to result
                            ]

-- HELPERS
data AllocState = AllocState
    { allocStateRegMap :: Map.Map Int ArgLocation
    , allocStateStackCtr :: Int
    , allocStateAvailableReg :: [Register]
    }

-- explicitly allocate a register/stack loc for new color
allocColor :: Int -> AllocState -> AllocState
allocColor color initAlloc = 
    case (allocStateAvailableReg initAlloc) of
        [] -> 
            AllocState
                (Map.insert color (STACK_ARGLOC (allocStateStackCtr initAlloc)) (allocStateRegMap initAlloc))
                ((allocStateStackCtr initAlloc) + 4) 
                (allocStateAvailableReg initAlloc)
        reg : _ ->
            AllocState 
                (Map.insert color (REG_ARGLOC reg) (allocStateRegMap initAlloc))
                (allocStateStackCtr initAlloc) 
                (tail (allocStateAvailableReg initAlloc))

-- lookup previously allocated color in reg alloc (errors if not found)
getColorReg :: Int -> AllocState -> ArgLocation
getColorReg color alloc = 
    case Map.lookup color (allocStateRegMap alloc) of
        Just argLoc -> argLoc
        