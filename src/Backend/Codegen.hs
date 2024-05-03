module Backend.Codegen (
    irToX86,
)
where

import Model.Ir
import Common.Liveness
import Model.Types
import Model.X86
import Common.Errors
import Common.Constants

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Text.Show.Pretty as Pretty
import qualified Debug.Trace as Trace

irToX86 :: Coloring -> FunctionIr -> [X86Instruction]
irToX86 coloring fnIr
    | debugLogs && (Trace.trace
        ("\n\nirToX86 -- " ++
            "\ncoloring=" ++ (Pretty.ppShow coloring)
        ) False) = undefined
irToX86 coloring fnIr =
    let 
        -- initialize function spillover at SP - 8 and w/ all available registers
        initAlloc = AllocState Map.empty 8 availableRegisters

        -- apply phiFn Ir->x86 translation
        (phiBlockMap, _) = 
            foldl
                ( \(interBlockMap, interAlloc) index ->
                    case Map.lookup index (functionIrBlocks fnIr) of
                        Just bb -> phiFnIrToX86 coloring fnIr index (bbIrPhiFn bb) interBlockMap interAlloc
                        Nothing -> error . compilerError $ "Attempted to access basic block during phi-fn x86 translation: index=" ++ (show index)
                )
                (Map.empty, commAlloc)
                [0..((length . functionIrBlocks $ fnIr) - 1)]

        -- apply command Ir->x86 translation
        (commBlockMap, commAlloc) =
            foldl
                ( \(interBlockMap, interAlloc) index ->
                    case Map.lookup index (functionIrBlocks fnIr) of
                        Just bb -> bbIrCommsToX86 coloring bb interBlockMap interAlloc
                        Nothing -> error . compilerError $ "Attempted to access basic block during command x86 translation: index=" ++ (show index)
                )
                (phiBlockMap, initAlloc)
                [0..((length . functionIrBlocks $ fnIr) - 1)]

        -- concatenate x86 instructions
        finalInstr = 
            foldl
                (\interInstr index ->
                    case Map.lookup index commBlockMap of
                        Just bbX86 -> interInstr ++ (basicBlockX86MainCommands bbX86) ++ (basicBlockX86TailCommands bbX86)
                        Nothing -> error . compilerError $ "Attempted to access basic block during final X86 translation: index=" ++ (show index)
                )
                []
                [0..((length . functionIrBlocks $ fnIr) - 1)]
     in finalInstr

phiFnIrToX86 :: Coloring -> FunctionIr -> Int -> PhiFnIr -> Map.Map Int BasicBlockX86 -> AllocState -> (Map.Map Int BasicBlockX86, AllocState)
phiFnIrToX86 coloring fnIr index phi initBlockMap initAlloc = 
    foldr
        -- generate an X86 inst for each asn var in the phi-fn and
        -- each load var in the predmap
        (\(var, varPredMap) (interBlockMap, interAlloc) ->
            foldr
                (\(predIndex, predVar) (predInterBlockMap, predInterAlloc)->
                    phiFnArgToX86 coloring fnIr (index, var) (predIndex, predVar) predInterBlockMap predInterAlloc
                )
                (interBlockMap, interAlloc)
                (Map.toList varPredMap)
        )
        (initBlockMap, initAlloc)
        (Map.toList phi)

-- converts a phi-fn arg (an entry in the pred map) to x86 + appends to the predecessor block
phiFnArgToX86 :: Coloring -> FunctionIr -> (Int, VariableIr) -> (Int, VariableIr) -> Map.Map Int BasicBlockX86 -> AllocState -> (Map.Map Int BasicBlockX86, AllocState)
phiFnArgToX86 coloring fnIr (succIndex, asnVar) (predIndex, predVar) initBlockMap initAlloc = 
    let currBBX86 = 
            case Map.lookup predIndex initBlockMap of
                Just currBBX86 -> currBBX86
                Nothing -> BasicBlockX86 [] [] [] []

        -- allocate regs for asn var and pred var
        (asnVarLoc, asnAlloc) = getVarLoc asnVar coloring initAlloc
        (predVarLoc, predAlloc) = getVarLoc predVar coloring asnAlloc

        -- set asn var to pred var
        asnInst = 
            case predVarLoc of
                REG_ARGLOC pureVarReg ->
                    [ MOV_X86 asnVarLoc predVarLoc
                    ]
                STACK_ARGLOC pureVarStackLoc ->
                    [ MOV_X86 (REG_ARGLOC DX) predVarLoc
                    , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                    ]

        -- attempt to inject the phi-fn inst to the predecessor bbX86
        newBBX86 = 
            case Map.lookup predIndex (functionIrBlocks fnIr) of
                Just bb -> injectPhiFnPredCommand (head . bbIrCommands $ bb) asnInst succIndex currBBX86
                Nothing -> error . compilerError $ "Attempted to access basic block during phi-fn pred injection: predIndex=" ++ (show predIndex)
        in (Map.insert predIndex newBBX86 initBlockMap, predAlloc)

-- converts bb main + tail commands to x86 
bbIrCommsToX86 :: Coloring -> BasicBlockIr -> Map.Map Int BasicBlockX86 -> AllocState -> (Map.Map Int BasicBlockX86, AllocState)
bbIrCommsToX86 coloring bb initBlockMap initAlloc = 
    let currBBX86 = 
            case Map.lookup (bbIndex bb) initBlockMap of
                Just currBBX86 -> currBBX86
                Nothing -> BasicBlockX86 [] [] [] []

        -- convert main commands first
        (mainInst, mainAlloc) = 
            foldr
                ( \comm (interInstr, interAlloc) ->
                    let (commInstr, newAlloc) = mainCommIrToX86 coloring comm interAlloc
                    in (interInstr ++ commInstr, newAlloc)
                )
                ([LABEL_X86 (bbToLabel (bbIndex bb))], initAlloc)
                (tail . bbIrCommands $ bb)

        -- convert tail command w/ phi-fn succ injection
        (tailInst, tailAlloc) = tailCommIrToX86 coloring (head . bbIrCommands $ bb) currBBX86 mainAlloc

        -- remove phi-fn insts and commit final BBX86
        newBBX86 = BasicBlockX86 mainInst tailInst [] []
        updatedBbBlockMap = Map.insert (bbIndex bb) newBBX86 initBlockMap
    in (updatedBbBlockMap, tailAlloc)

-- MAIN COMM IR->x86

mainCommIrToX86 :: Coloring -> CommandIr -> AllocState -> ([X86Instruction], AllocState)
mainCommIrToX86 coloring comm initAlloc =
    case comm of
        INIT_IR var -> 
            ([], initAlloc)
        ASN_PURE_IR asnVar asnPure -> 
            asnPureIrToX86 coloring asnVar asnPure initAlloc
        ASN_IMPURE_IR asnVar asnImpure -> 
            asnImpureIrToX86 coloring asnVar asnImpure initAlloc
        _ ->
            error . compilerError $ "Attempted to translate ir->X86 non-main command as a main command: comm=" ++ (show comm)

asnPureIrToX86 :: Coloring -> VariableIr -> PureIr -> AllocState -> ([X86Instruction], AllocState)
asnPureIrToX86 coloring asnVar asnPure initAlloc =
    let (asnVarLoc, asnAlloc) = getVarLoc asnVar coloring initAlloc
    in case asnPure of
            PURE_BASE_IR base ->
                case base of
                    -- x = CONST
                    CONST_IR const ->
                        let constLoc = getConstLoc const
                            constInst =
                                [ MOV_X86 asnVarLoc constLoc
                                ]
                        in (constInst, asnAlloc)
                    -- x = VAR
                    VAR_IR pureVar ->
                        let (pureVarLoc, pureAlloc) = getVarLoc pureVar coloring asnAlloc
                            pureInst = 
                                case pureVarLoc of
                                    REG_ARGLOC pureVarReg ->
                                        [ MOV_X86 asnVarLoc pureVarLoc
                                        ]
                                    STACK_ARGLOC pureVarStackLoc ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        ]
                        in (pureInst, pureAlloc)
            -- x = y ? z
            PURE_BINOP_IR (PureBinopIr cat ty base1 base2) ->
                let (pureVarLoc1, pureVar1Alloc) =
                        case base1 of
                            CONST_IR const -> (getConstLoc const, asnAlloc)
                            VAR_IR pureVar -> getVarLoc pureVar coloring asnAlloc
                    (pureVarLoc2, pureVar2Alloc) =
                        case base2 of
                            CONST_IR const -> (getConstLoc const, pureVar1Alloc)
                            VAR_IR pureVar -> getVarLoc pureVar coloring pureVar1Alloc
                    binopInst = 
                        case cat of
                            -- arithmetic binop insts.
                            ADD_IR ->
                                case asnVarLoc of
                                    -- reg = y + z -> perform binop in reg
                                    REG_ARGLOC asnVarReg
                                        | asnVarLoc == pureVarLoc1 ->
                                            [ ADD_X86 asnVarLoc pureVarLoc2
                                            ]
                                        | asnVarLoc == pureVarLoc2 ->
                                            [ ADD_X86 asnVarLoc pureVarLoc1
                                            ]
                                        | otherwise ->
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
                                    REG_ARGLOC asnVarReg
                                        | asnVarLoc == pureVarLoc1 ->
                                            [ SUB_X86 asnVarLoc pureVarLoc2
                                            ]
                                        | asnVarLoc == pureVarLoc2 ->
                                            [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                            , SUB_X86 (REG_ARGLOC DX) pureVarLoc2
                                            , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                            ]
                                        | otherwise ->
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
                                    REG_ARGLOC asnVarReg
                                        | asnVarLoc == pureVarLoc1 ->
                                            [ IMUL_X86 asnVarLoc pureVarLoc2
                                            ]
                                        | asnVarLoc == pureVarLoc2 ->
                                            [ IMUL_X86 asnVarLoc pureVarLoc1
                                            ]
                                        | otherwise ->
                                            [ MOV_X86 asnVarLoc pureVarLoc1
                                            , IMUL_X86 asnVarLoc pureVarLoc2
                                            ]
                                    -- S[i] = y * z -> perform binop in DX and move to S[i]
                                    STACK_ARGLOC asnVarStackLoc ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , IMUL_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        ]
                            AND_IR ->
                                case asnVarLoc of
                                    -- reg = y & z -> perform binop in reg
                                    REG_ARGLOC asnVarReg
                                        | asnVarLoc == pureVarLoc1 ->
                                            [ AND_X86 asnVarLoc pureVarLoc2
                                            ]
                                        | asnVarLoc == pureVarLoc2 ->
                                            [ AND_X86 asnVarLoc pureVarLoc1
                                            ]
                                        | otherwise ->
                                            [ MOV_X86 asnVarLoc pureVarLoc1
                                            , AND_X86 asnVarLoc pureVarLoc2
                                            ]
                                    -- S[i] = y & z -> perform binop in DX and move to S[i]
                                    STACK_ARGLOC asnVarStackLoc ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , AND_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        ]
                            XOR_IR ->
                                case asnVarLoc of
                                    -- reg = y ^ z -> perform binop in reg
                                    REG_ARGLOC asnVarReg
                                        | asnVarLoc == pureVarLoc1 ->
                                            [ XOR_X86 asnVarLoc pureVarLoc2
                                            ]
                                        | asnVarLoc == pureVarLoc2 ->
                                            [ XOR_X86 asnVarLoc pureVarLoc1
                                            ]
                                        | otherwise ->
                                            [ MOV_X86 asnVarLoc pureVarLoc1
                                            , XOR_X86 asnVarLoc pureVarLoc2
                                            ]
                                    -- S[i] = y ^ z -> perform binop in DX and move to S[i]
                                    STACK_ARGLOC asnVarStackLoc ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , XOR_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        ]
                            OR_IR ->
                                case asnVarLoc of
                                    -- reg = y | z -> perform binop in reg
                                    REG_ARGLOC asnVarReg
                                        | asnVarLoc == pureVarLoc1 ->
                                            [ OR_X86 asnVarLoc pureVarLoc2
                                            ]
                                        | asnVarLoc == pureVarLoc2 ->
                                            [ OR_X86 asnVarLoc pureVarLoc1
                                            ]
                                        | otherwise ->
                                            [ MOV_X86 asnVarLoc pureVarLoc1
                                            , OR_X86 asnVarLoc pureVarLoc2
                                            ]
                                    -- S[i] = y * z -> perform binop in DX and move to S[i]
                                    STACK_ARGLOC asnVarStackLoc ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , OR_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        ]
                            SAL_IR ->
                                case asnVarLoc of
                                    -- reg = y << z -> perform binop in reg
                                    REG_ARGLOC asnVarReg
                                        | asnVarLoc == pureVarLoc1 ->
                                            [ SAL_X86 asnVarLoc pureVarLoc2
                                            ]
                                        | asnVarLoc == pureVarLoc2 ->
                                            [ SAL_X86 asnVarLoc pureVarLoc1
                                            ]
                                        | otherwise ->
                                            [ MOV_X86 asnVarLoc pureVarLoc1
                                            , SAL_X86 asnVarLoc pureVarLoc2
                                            ]
                                    -- S[i] = y << z -> perform binop in DX and move to S[i]
                                    STACK_ARGLOC asnVarStackLoc ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , SAL_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        ]
                            SAR_IR ->
                                case asnVarLoc of
                                    -- reg = y >> z -> perform binop in reg
                                    REG_ARGLOC asnVarReg
                                        | asnVarLoc == pureVarLoc1 ->
                                            [ SAR_X86 asnVarLoc pureVarLoc2
                                            ]
                                        | asnVarLoc == pureVarLoc2 ->
                                            [ SAR_X86 asnVarLoc pureVarLoc1
                                            ]
                                        | otherwise ->
                                            [ MOV_X86 asnVarLoc pureVarLoc1
                                            , SAR_X86 asnVarLoc pureVarLoc2
                                            ]
                                    -- S[i] = y >> z -> perform binop in DX and move to S[i]
                                    STACK_ARGLOC asnVarStackLoc ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , SAR_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        ]
                            -- boolean binop insts.
                            LT_IR ->
                                case asnVarLoc of
                                    -- reg = y < z -> perform binop in reg
                                    REG_ARGLOC asnVarReg
                                        | asnVarLoc == pureVarLoc1 ->
                                            [ SUB_X86 asnVarLoc pureVarLoc2
                                            , SHR_X86 asnVarLoc (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                                            ]
                                        | asnVarLoc == pureVarLoc2 ->
                                            [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                            , SUB_X86 (REG_ARGLOC DX) pureVarLoc2
                                            , SHR_X86 (REG_ARGLOC DX) (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                                            , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                            ]
                                        | otherwise ->
                                            [ MOV_X86 asnVarLoc pureVarLoc1
                                            , SUB_X86 asnVarLoc pureVarLoc2
                                            , SHR_X86 asnVarLoc (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                                            ]
                                    -- S[i] = y < z -> perform binop in DX and move to S[i]
                                    STACK_ARGLOC asnVarStackLoc ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , SUB_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , SHR_X86 (REG_ARGLOC DX) (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        ]
                            GT_IR ->
                                case asnVarLoc of
                                    -- reg = y > z -> perform binop in reg
                                    REG_ARGLOC asnVarReg
                                        | asnVarLoc == pureVarLoc1 ->
                                            [ MOV_X86 (REG_ARGLOC DX) pureVarLoc2
                                            , SUB_X86 (REG_ARGLOC DX) pureVarLoc1
                                            , SHR_X86 (REG_ARGLOC DX) (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                                            , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                            ]
                                        | asnVarLoc == pureVarLoc2 ->
                                            [ SUB_X86 asnVarLoc pureVarLoc1
                                            , SHR_X86 asnVarLoc (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                                            ]
                                        | otherwise ->
                                            [ MOV_X86 asnVarLoc pureVarLoc2
                                            , SUB_X86 asnVarLoc pureVarLoc1
                                            , SHR_X86 asnVarLoc (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                                            ]
                                    -- S[i] = y < z -> perform binop in DX and move to S[i]
                                    STACK_ARGLOC asnVarStackLoc ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , SUB_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , SHR_X86 (REG_ARGLOC DX) (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        ]
                            LTE_IR ->
                                case asnVarLoc of
                                    -- reg = y <= z -> perform binop in reg
                                    REG_ARGLOC asnVarReg
                                        | asnVarLoc == pureVarLoc1 ->
                                            [ MOV_X86 (REG_ARGLOC DX) pureVarLoc2
                                            , SUB_X86 (REG_ARGLOC DX) pureVarLoc1
                                            , NOT_X86 (REG_ARGLOC DX)
                                            , SHR_X86 (REG_ARGLOC DX) (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                                            , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                            ]
                                        | asnVarLoc == pureVarLoc2 ->
                                            [ SUB_X86 asnVarLoc pureVarLoc1
                                            , NOT_X86 asnVarLoc
                                            , SHR_X86 asnVarLoc (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                                            ]
                                        | otherwise ->
                                            [ MOV_X86 asnVarLoc pureVarLoc2
                                            , SUB_X86 asnVarLoc pureVarLoc1
                                            , NOT_X86 asnVarLoc
                                            , SHR_X86 asnVarLoc (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                                            ]
                                    -- S[i] = y <= z -> perform binop in DX and move to S[i]
                                    STACK_ARGLOC asnVarStackLoc ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , SUB_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , NOT_X86 (REG_ARGLOC DX)
                                        , SHR_X86 (REG_ARGLOC DX) (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        ]
                            GTE_IR ->
                                case asnVarLoc of
                                    -- reg = y >= z -> perform binop in reg
                                    REG_ARGLOC asnVarReg
                                        | asnVarLoc == pureVarLoc1 ->
                                            [ SUB_X86 asnVarLoc pureVarLoc2
                                            , NOT_X86 asnVarLoc
                                            , SHR_X86 asnVarLoc (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                                            ]
                                        | asnVarLoc == pureVarLoc2 ->
                                            [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                            , SUB_X86 (REG_ARGLOC DX) pureVarLoc2
                                            , NOT_X86 (REG_ARGLOC DX)
                                            , SHR_X86 (REG_ARGLOC DX) (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                                            , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                            ]
                                        | otherwise ->
                                            [ MOV_X86 asnVarLoc pureVarLoc1
                                            , SUB_X86 asnVarLoc pureVarLoc2
                                            , NOT_X86 asnVarLoc
                                            , SHR_X86 asnVarLoc (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                                            ]
                                    -- S[i] = y >= z -> perform binop in DX and move to S[i]
                                    STACK_ARGLOC asnVarStackLoc ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , SUB_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , NOT_X86 (REG_ARGLOC DX)
                                        , SHR_X86 (REG_ARGLOC DX) (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        ]
                            EQ_IR ->
                                case asnVarLoc of
                                    -- reg = y == z -> perform binop in reg
                                    REG_ARGLOC asnVarReg
                                        | asnVarLoc == pureVarLoc1 ->
                                            [ MOV_X86 (REG_ARGLOC DX) pureVarLoc2
                                            , SUB_X86 (REG_ARGLOC DX) pureVarLoc1
                                            , SUB_X86 asnVarLoc pureVarLoc2
                                            , OR_X86 asnVarLoc (REG_ARGLOC DX)
                                            , NOT_X86 asnVarLoc
                                            , SHR_X86 asnVarLoc (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                                            ]
                                        | asnVarLoc == pureVarLoc2 ->
                                            [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                            , SUB_X86 (REG_ARGLOC DX) pureVarLoc2
                                            , SUB_X86 asnVarLoc pureVarLoc1
                                            , OR_X86 asnVarLoc (REG_ARGLOC DX)
                                            , NOT_X86 asnVarLoc
                                            , SHR_X86 asnVarLoc (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                                            ]
                                        | otherwise ->
                                            [ MOV_X86 asnVarLoc pureVarLoc1
                                            , MOV_X86 (REG_ARGLOC DX) pureVarLoc2
                                            , SUB_X86 asnVarLoc pureVarLoc2
                                            , SUB_X86 (REG_ARGLOC DX) pureVarLoc1
                                            , OR_X86 asnVarLoc (REG_ARGLOC DX)
                                            , NOT_X86 asnVarLoc
                                            , SHR_X86 asnVarLoc (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                                            ]
                                    -- S[i] = y == z -> perform binop in DX and move to S[i]
                                    STACK_ARGLOC asnVarStackLoc ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , MOV_X86 (REG_ARGLOC AX) pureVarLoc2
                                        , SUB_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , SUB_X86 (REG_ARGLOC AX) pureVarLoc1
                                        , OR_X86 (REG_ARGLOC DX) (REG_ARGLOC AX)
                                        , NOT_X86 (REG_ARGLOC DX)
                                        , SHR_X86 (REG_ARGLOC DX) (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        ]
                            NEQ_IR ->
                                case asnVarLoc of
                                    -- reg = y != z -> perform binop in reg
                                    REG_ARGLOC asnVarReg
                                        | asnVarLoc == pureVarLoc1 ->
                                            [ MOV_X86 (REG_ARGLOC DX) pureVarLoc2
                                            , SUB_X86 (REG_ARGLOC DX) pureVarLoc1
                                            , SUB_X86 asnVarLoc pureVarLoc2
                                            , OR_X86 asnVarLoc (REG_ARGLOC DX)
                                            , SHR_X86 asnVarLoc (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                                            ]
                                        | asnVarLoc == pureVarLoc2 ->
                                            [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                            , SUB_X86 (REG_ARGLOC DX) pureVarLoc2
                                            , SUB_X86 asnVarLoc pureVarLoc1
                                            , OR_X86 asnVarLoc (REG_ARGLOC DX)
                                            , SHR_X86 asnVarLoc (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                                            ]
                                        | otherwise ->
                                            [ MOV_X86 asnVarLoc pureVarLoc1
                                            , MOV_X86 (REG_ARGLOC DX) pureVarLoc2
                                            , SUB_X86 asnVarLoc pureVarLoc2
                                            , SUB_X86 (REG_ARGLOC DX) pureVarLoc1
                                            , OR_X86 asnVarLoc (REG_ARGLOC DX)
                                            , SHR_X86 asnVarLoc (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                                            ]
                                    -- S[i] = y != z -> perform binop in DX and move to S[i]
                                    STACK_ARGLOC asnVarStackLoc ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , MOV_X86 (REG_ARGLOC AX) pureVarLoc2
                                        , SUB_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , SUB_X86 (REG_ARGLOC AX) pureVarLoc1
                                        , OR_X86 (REG_ARGLOC DX) (REG_ARGLOC AX)
                                        , SHR_X86 (REG_ARGLOC DX) (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        ]
                in (binopInst, pureVar2Alloc)
            PURE_UNOP_IR (PureUnopIr cat ty base) ->
                let (pureVarLoc, pureAlloc) =
                        case base of
                            CONST_IR const -> (getConstLoc const, asnAlloc)
                            VAR_IR pureVar -> getVarLoc pureVar coloring asnAlloc
                    unopInst = 
                        case cat of
                            NEG_IR ->
                                [ MOV_X86 asnVarLoc pureVarLoc
                                , NEG_X86 asnVarLoc
                                ]
                            NOT_IR ->
                                [ MOV_X86 asnVarLoc pureVarLoc
                                , NOT_X86 asnVarLoc
                                ]
                            LOGNOT_IR ->
                                [ MOV_X86 asnVarLoc pureVarLoc
                                , XOR_X86 asnVarLoc (CONST_ARGLOC trueX86)
                                ]
                in (unopInst, pureAlloc)

asnImpureIrToX86 :: Coloring -> VariableIr -> ImpureIr -> AllocState -> ([X86Instruction], AllocState)
asnImpureIrToX86 coloring asnVar asnImpure initAlloc =
    let (asnVarLoc, asnAlloc) = getVarLoc asnVar coloring initAlloc
    in case asnImpure of
            IMPURE_BINOP_IR (ImpureBinopIr cat ty base1 base2) ->
                let (pureVarLoc1, pureVar1Alloc) =
                        case base1 of
                            CONST_IR const -> (getConstLoc const, asnAlloc)
                            VAR_IR pureVar -> getVarLoc pureVar coloring asnAlloc
                    (pureVarLoc2, pureVar2Alloc) =
                        case base2 of
                            CONST_IR const -> (getConstLoc const, pureVar1Alloc)
                            VAR_IR pureVar -> getVarLoc pureVar coloring pureVar1Alloc
                    binopInst = 
                        case cat of
                            DIV_IR ->
                                case pureVarLoc2 of
                                    -- if divisor is a const temporarily push onto stack
                                    CONST_ARGLOC int ->
                                        [ XOR_X86 (REG_ARGLOC DX) (REG_ARGLOC DX)               -- 0 out DX
                                        , MOV_X86 (REG_ARGLOC AX) pureVarLoc1                   -- move dividend to AX
                                        , CQO_X86                                               -- sign-extend dividend to DX:AX
                                        , PUSH_X86 pureVarLoc2                                  -- push divisor to stack
                                        , IDIV_X86 (STACK_ARGLOC 0)                             -- divide AX / S[0]
                                        , ADD_X86 (REG_ARGLOC SP) (CONST_ARGLOC registerSize)   -- pop divisor from stack
                                        , MOV_X86 asnVarLoc (REG_ARGLOC AX)                     -- move quotient to result
                                        ]
                                    -- o/w use existing location
                                    _ ->
                                        [ XOR_X86 (REG_ARGLOC DX) (REG_ARGLOC DX)               -- 0 out DX
                                        , MOV_X86 (REG_ARGLOC AX) pureVarLoc1                   -- mov dividend to AX
                                        , CQO_X86                                               -- sign-extend dividend to DX:AX
                                        , IDIV_X86 pureVarLoc2                                  -- divide AX / divisor (in reg/on stack)
                                        , MOV_X86 asnVarLoc (REG_ARGLOC AX)                     -- move quotient to result
                                        ]
                            MOD_IR ->
                                case pureVarLoc2 of
                                    -- if divisor is a const temporarily push onto stack
                                    CONST_ARGLOC int ->
                                        [ XOR_X86 (REG_ARGLOC DX) (REG_ARGLOC DX)               -- 0 out DX
                                        , MOV_X86 (REG_ARGLOC AX) pureVarLoc1                   -- mov dividend to AX
                                        , CQO_X86                                               -- sign-extend dividend to DX:AX
                                        , PUSH_X86 pureVarLoc2                                  -- push divisor to stack
                                        , IDIV_X86 (STACK_ARGLOC 0)                             -- divide AX / S[0]
                                        , ADD_X86 (REG_ARGLOC SP) (CONST_ARGLOC registerSize)   -- pop divisor from stack
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)                     -- move remainder to result
                                        ]
                                    -- o/w use existing location
                                    _ ->
                                        [ XOR_X86 (REG_ARGLOC DX) (REG_ARGLOC DX)               -- 0 out DX
                                        , MOV_X86 (REG_ARGLOC AX) pureVarLoc1                   -- mov dividend to AX
                                        , CQO_X86                                               -- sign-extend dividend to DX:AX
                                        , IDIV_X86 pureVarLoc2                                  -- divide AX / divisor (in reg/on stack)
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)                     -- move remainder to result
                                        ]
                in (binopInst, pureVar2Alloc)

-- TAIL COMM IR->x86

tailCommIrToX86 :: Coloring -> CommandIr -> BasicBlockX86 -> AllocState -> ([X86Instruction], AllocState)
tailCommIrToX86 coloring comm bbX86 initAlloc =
    case comm of
        GOTO_BB_IR bbIndex ->
            gotoToX86 bbIndex bbX86 initAlloc
        SPLIT_BB_IR condPure splitTrue splitFalse ->
            splitToX86 coloring condPure splitTrue splitFalse bbX86 initAlloc
        RET_PURE_IR retPure ->
            retToX86 coloring retPure bbX86 initAlloc

-- GOTO: prepends phi-fn-1  instrs. to JUMP instr.
gotoToX86 :: Int -> BasicBlockX86 -> AllocState -> ([X86Instruction], AllocState)
gotoToX86 index bbX86 initAlloc = 
    ((basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++ [JMP_X86 (bbToLabel index)], initAlloc)

-- SPLIT: prepends phi-fn-1/2 instrs. to CMP/JMP instrs. in the following order:
--   CMP
--   [true cond x86 insts.]
--   JMP (true)
--   [false cond x86 insts.]
--   JMP (false)
splitToX86 :: Map.Map VariableIr Int -> PureIr -> Int -> Int -> BasicBlockX86 -> AllocState -> ([X86Instruction], AllocState)
splitToX86 coloring condPure splitTrue splitFalse bbX86 initAlloc =         
    case condPure of
        PURE_BASE_IR base ->
            case base of
                CONST_IR const ->
                    let splitInst = 
                            case const of
                                BOOL_CONST True ->
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JMP_X86 (bbToLabel splitTrue)
                                    ]
                                BOOL_CONST False ->
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel splitFalse)
                                    ]
                    in (splitInst, initAlloc)
                VAR_IR pureVar ->
                    let (pureVarLoc, splitAlloc) = getVarLoc pureVar coloring initAlloc
                        splitInst = 
                            [ CMP_X86 pureVarLoc (CONST_ARGLOC falseX86)
                            ] ++
                            (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                            [ JZ_X86 (bbToLabel splitFalse)
                            ] ++
                            (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                            [ JMP_X86 (bbToLabel splitTrue)
                            ]
                    in (splitInst, splitAlloc)
        
        PURE_BINOP_IR (PureBinopIr cat ty base1 base2) ->
            let (pureVarLoc1, pureVar1Alloc) =
                    case base1 of
                        CONST_IR const -> (getConstLoc const, initAlloc)
                        VAR_IR pureVar -> getVarLoc pureVar coloring initAlloc
                (pureVarLoc2, pureVar2Alloc) =
                    case base2 of
                        CONST_IR const -> (getConstLoc const, pureVar1Alloc)
                        VAR_IR pureVar -> getVarLoc pureVar coloring pureVar1Alloc
                splitInst = 
                    case cat of
                        LT_IR -> 
                            case (pureVarLoc1, pureVarLoc2) of
                                -- c1 < c2 -> move c1 to DX
                                (CONST_ARGLOC pureVarConstLoc1, CONST_ARGLOC pureVarConstLoc2) ->
                                    [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                    , CMP_X86 (REG_ARGLOC DX) pureVarLoc2
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JL_X86 (bbToLabel splitTrue)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel splitFalse)
                                    ]
                                -- S[i] < S[j] -> move S[i] to DX
                                (STACK_ARGLOC pureVarStackLoc1, STACK_ARGLOC pureVarStackLoc2) ->
                                    [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                    , CMP_X86 (REG_ARGLOC DX) pureVarLoc2
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JL_X86 (bbToLabel splitTrue)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel splitFalse)
                                    ]
                                -- at least one reg --> direct cmp
                                _ ->
                                    [ CMP_X86 pureVarLoc1 pureVarLoc2
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JL_X86 (bbToLabel splitTrue)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel splitFalse)
                                    ]
                        LTE_IR ->
                            case (pureVarLoc1, pureVarLoc2) of
                                -- c1 <= c2 -> move c1 to DX
                                (CONST_ARGLOC pureVarConstLoc1, CONST_ARGLOC pureVarConstLoc2) ->
                                    [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                    , CMP_X86 (REG_ARGLOC DX) pureVarLoc2
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JLE_X86 (bbToLabel splitTrue)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel splitFalse)
                                    ]
                                -- S[i] <= S[j] -> move S[i] to DX
                                (STACK_ARGLOC pureVarStackLoc1, STACK_ARGLOC pureVarStackLoc2) ->
                                    [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                    , CMP_X86 (REG_ARGLOC DX) pureVarLoc2
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JLE_X86 (bbToLabel splitTrue)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel splitFalse)
                                    ]
                                -- at least one reg --> direct cmp
                                _ ->
                                    [ CMP_X86 pureVarLoc1 pureVarLoc2
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JLE_X86 (bbToLabel splitTrue)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel splitFalse)
                                    ]
                        GT_IR ->
                            case (pureVarLoc1, pureVarLoc2) of
                                -- c1 > c2 -> move c1 to DX
                                (CONST_ARGLOC pureVarConstLoc1, CONST_ARGLOC pureVarConstLoc2) ->
                                    [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                    , CMP_X86 (REG_ARGLOC DX) pureVarLoc2
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JG_X86 (bbToLabel splitTrue)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel splitFalse)
                                    ]
                                -- S[i] > S[j] -> move S[i] to DX
                                (STACK_ARGLOC pureVarStackLoc1, STACK_ARGLOC pureVarStackLoc2) ->
                                    [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                    , CMP_X86 (REG_ARGLOC DX) pureVarLoc2
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JG_X86 (bbToLabel splitTrue)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel splitFalse)
                                    ]
                                -- at least one reg --> direct cmp
                                _ ->
                                    [ CMP_X86 pureVarLoc1 pureVarLoc2
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JG_X86 (bbToLabel splitTrue)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel splitFalse)
                                    ]
                        GTE_IR ->
                            case (pureVarLoc1, pureVarLoc2) of
                                -- c1 >= c2 -> move c1 to DX
                                (CONST_ARGLOC pureVarConstLoc1, CONST_ARGLOC pureVarConstLoc2) ->
                                    [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                    , CMP_X86 (REG_ARGLOC DX) pureVarLoc2
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JGE_X86 (bbToLabel splitTrue)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel splitFalse)
                                    ]
                                -- S[i] >= S[j] -> move S[i] to DX
                                (STACK_ARGLOC pureVarStackLoc1, STACK_ARGLOC pureVarStackLoc2) ->
                                    [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                    , CMP_X86 (REG_ARGLOC DX) pureVarLoc2
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JGE_X86 (bbToLabel splitTrue)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel splitFalse)
                                    ]
                                -- at least one reg --> direct cmp
                                _ ->
                                    [ CMP_X86 pureVarLoc1 pureVarLoc2
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JGE_X86 (bbToLabel splitTrue)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel splitFalse)
                                    ]
                        EQ_IR ->
                            case (pureVarLoc1, pureVarLoc2) of
                                -- c1 == c2 -> move c1 to DX
                                (CONST_ARGLOC pureVarConstLoc1, CONST_ARGLOC pureVarConstLoc2) ->
                                    [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                    , CMP_X86 (REG_ARGLOC DX) pureVarLoc2
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JE_X86 (bbToLabel splitTrue)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel splitFalse)
                                    ]
                                -- S[i] == S[j] -> move S[i] to DX
                                (STACK_ARGLOC pureVarStackLoc1, STACK_ARGLOC pureVarStackLoc2) ->
                                    [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                    , CMP_X86 (REG_ARGLOC DX) pureVarLoc2
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JE_X86 (bbToLabel splitTrue)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel splitFalse)
                                    ]
                                -- at least one reg --> direct cmp
                                _ ->
                                    [ CMP_X86 pureVarLoc1 pureVarLoc2
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JE_X86 (bbToLabel splitTrue)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel splitFalse)
                                    ]
                        NEQ_IR ->
                            case (pureVarLoc1, pureVarLoc2) of
                                -- c1 != c2 -> move c1 to DX
                                (CONST_ARGLOC pureVarConstLoc1, CONST_ARGLOC pureVarConstLoc2) ->
                                    [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                    , CMP_X86 (REG_ARGLOC DX) pureVarLoc2
                                    ] ++ 
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JNE_X86 (bbToLabel splitTrue)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel splitFalse)
                                    ]
                                -- S[i] != S[j] -> move S[i] to DX
                                (STACK_ARGLOC pureVarStackLoc1, STACK_ARGLOC pureVarStackLoc2) ->
                                    [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                    , CMP_X86 (REG_ARGLOC DX) pureVarLoc2
                                    ] ++ 
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JNE_X86 (bbToLabel splitTrue)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel splitFalse)
                                    ]
                                -- at least one reg --> direct cmp
                                _ ->
                                    [ CMP_X86 pureVarLoc1 pureVarLoc2
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JNE_X86 (bbToLabel splitTrue)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel splitFalse)
                                    ]
            in (splitInst, pureVar2Alloc)

        PURE_UNOP_IR (PureUnopIr cat ty base) ->
            let (pureVarLoc, pureVarAlloc) =
                    case base of
                        CONST_IR const -> (getConstLoc const, initAlloc)
                        VAR_IR pureVar -> getVarLoc pureVar coloring initAlloc
                splitInst = 
                    case cat of
                        LOGNOT_IR ->
                            [ CMP_X86 pureVarLoc (CONST_ARGLOC falseX86)
                            ] ++
                            (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                            [ JZ_X86 (bbToLabel splitTrue)
                            ] ++
                            (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                            [ JMP_X86 (bbToLabel splitFalse)
                            ]
            in (splitInst, pureVarAlloc)

-- RET: prepends no phi-fn insts. to ret inst.
retToX86 :: Map.Map VariableIr Int -> PureIr -> BasicBlockX86 -> AllocState -> ([X86Instruction], AllocState)
retToX86 coloring retPure bbX86 initAlloc =
    case retPure of
        PURE_BASE_IR base ->
            let (pureVarLoc, pureVarAlloc) =
                    case base of
                        CONST_IR const -> (getConstLoc const, initAlloc)
                        VAR_IR pureVar -> getVarLoc pureVar coloring initAlloc
                retInst = 
                    [ MOV_X86 (REG_ARGLOC AX) pureVarLoc
                    , RET_X86
                    ]
            in (retInst, pureVarAlloc)
        -- ret y ? z
        PURE_BINOP_IR (PureBinopIr cat ty base1 base2) ->
            let (pureVarLoc1, pureVar1Alloc) =
                    case base1 of
                        CONST_IR const -> (getConstLoc const, initAlloc)
                        VAR_IR pureVar -> getVarLoc pureVar coloring initAlloc
                (pureVarLoc2, pureVar2Alloc) =
                    case base2 of
                        CONST_IR const -> (getConstLoc const, pureVar1Alloc)
                        VAR_IR pureVar -> getVarLoc pureVar coloring pureVar1Alloc
                retInst = 
                    case cat of
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
                        AND_IR ->
                            [ MOV_X86 (REG_ARGLOC AX) pureVarLoc1
                            , AND_X86 (REG_ARGLOC AX) pureVarLoc2
                            , RET_X86
                            ]
                        XOR_IR ->
                            [ MOV_X86 (REG_ARGLOC AX) pureVarLoc1
                            , XOR_X86 (REG_ARGLOC AX) pureVarLoc2
                            , RET_X86
                            ]
                        OR_IR ->
                            [ MOV_X86 (REG_ARGLOC AX) pureVarLoc1
                            , OR_X86 (REG_ARGLOC AX) pureVarLoc2
                            , RET_X86
                            ]
                        SAL_IR ->
                            [ MOV_X86 (REG_ARGLOC AX) pureVarLoc1
                            , SAL_X86 (REG_ARGLOC AX) pureVarLoc2
                            , RET_X86
                            ]
                        SAR_IR ->
                            [ MOV_X86 (REG_ARGLOC AX) pureVarLoc1
                            , SAR_X86 (REG_ARGLOC AX) pureVarLoc2
                            , RET_X86
                            ]
                        LT_IR ->
                            [ MOV_X86 (REG_ARGLOC AX) pureVarLoc1
                            , SUB_X86 (REG_ARGLOC AX) pureVarLoc2
                            , SHR_X86 (REG_ARGLOC AX) (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                            , RET_X86
                            ]
                        GT_IR ->
                            [ MOV_X86 (REG_ARGLOC AX) pureVarLoc2
                            , SUB_X86 (REG_ARGLOC AX) pureVarLoc1
                            , SHR_X86 (REG_ARGLOC AX) (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                            , RET_X86
                            ]
                        LTE_IR ->
                            [ MOV_X86 (REG_ARGLOC AX) pureVarLoc2
                            , SUB_X86 (REG_ARGLOC AX) pureVarLoc1
                            , NOT_X86 (REG_ARGLOC AX)
                            , SHR_X86 (REG_ARGLOC AX) (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                            , RET_X86
                            ]
                        GTE_IR ->
                            [ MOV_X86 (REG_ARGLOC AX) pureVarLoc1
                            , SUB_X86 (REG_ARGLOC AX) pureVarLoc2
                            , NOT_X86 (REG_ARGLOC AX)
                            , SHR_X86 (REG_ARGLOC AX) (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                            , RET_X86
                            ]
                        EQ_IR ->
                            [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                            , MOV_X86 (REG_ARGLOC AX) pureVarLoc2
                            , SUB_X86 (REG_ARGLOC DX) pureVarLoc2
                            , SUB_X86 (REG_ARGLOC AX) pureVarLoc1
                            , OR_X86 (REG_ARGLOC AX) (REG_ARGLOC DX)
                            , NOT_X86 (REG_ARGLOC AX)
                            , SHR_X86 (REG_ARGLOC AX) (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                            , RET_X86
                            ]
                        NEQ_IR ->
                            [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                            , MOV_X86 (REG_ARGLOC AX) pureVarLoc2
                            , SUB_X86 (REG_ARGLOC DX) pureVarLoc2
                            , SUB_X86 (REG_ARGLOC AX) pureVarLoc1
                            , OR_X86 (REG_ARGLOC AX) (REG_ARGLOC DX)
                            , SHR_X86 (REG_ARGLOC AX) (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                            , RET_X86
                            ]
            in (retInst, pureVar2Alloc)
        PURE_UNOP_IR (PureUnopIr cat ty base) ->
            let (pureVarLoc, pureVarAlloc) =
                    case base of
                        CONST_IR const -> (getConstLoc const, initAlloc)
                        VAR_IR pureVar -> getVarLoc pureVar coloring initAlloc
                retInst = 
                    case cat of
                        NEG_IR ->
                            [ MOV_X86 (REG_ARGLOC AX) pureVarLoc
                            , NEG_X86 (REG_ARGLOC AX)
                            , RET_X86
                            ]
                        NOT_IR ->
                            [ MOV_X86 (REG_ARGLOC AX) pureVarLoc
                            , NOT_X86 (REG_ARGLOC AX)
                            , RET_X86
                            ]
                        LOGNOT_IR ->
                            [ MOV_X86 (REG_ARGLOC AX) pureVarLoc
                            , XOR_X86 (REG_ARGLOC AX) (CONST_ARGLOC trueX86)
                            , RET_X86
                            ]
            in (retInst, pureVarAlloc)

-- HELPERS

data BasicBlockX86 = BasicBlockX86
    { basicBlockX86MainCommands :: [X86Instruction] -- non-terminating commands
    , basicBlockX86TailCommands :: [X86Instruction] -- terminating commands with phi-fn pred commands (if applicable)
    , basicBlockX86InjectedPhiFnPredCommands1 :: [X86Instruction] -- injected phi-fn commands from successor 1 (i.e. GOTO or SPLIT true)
    , basicBlockX86InjectedPhiFnPredCommands2 :: [X86Instruction] -- injected phi-fn commands from successor 2 (i.e. SPLIT false)
    }
    deriving (Show)

-- adds phi-fn instrs. to the predecessor BBX86 based on the type of the predecessor's terminating command:
--    GOTO: add to phi-fn-1
--    SPLIT: 
--      successor = true BB -> add to phi-fn-1
--      successor = false BB -> add to phi-fn-2
--    RET: err (no valid injection)
injectPhiFnPredCommand :: CommandIr -> [X86Instruction] -> Int -> BasicBlockX86 -> BasicBlockX86
injectPhiFnPredCommand termComm phiFnComms succBBIndex bbX86
    | debugLogs && (Trace.trace
        ("\n\ninjectPhiFnPredCommand -- " ++
            "\ntermComm=" ++ (Pretty.ppShow termComm) ++
            "\nsuccIndex=" ++ (show succBBIndex) ++
            "\nphiFnComms=" ++ (Pretty.ppShow phiFnComms)
        ) False) = undefined
injectPhiFnPredCommand termComm phiFnComms succBBIndex bbX86 = 
    case termComm of
        GOTO_BB_IR actualSuccIndex
            | succBBIndex == actualSuccIndex ->
                BasicBlockX86
                    (basicBlockX86MainCommands bbX86)
                    (basicBlockX86TailCommands bbX86)
                    ((basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++ phiFnComms)
                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86)
            | otherwise ->
                error . compilerError $ "Attempted to inject phi-fn pred command to a GOTO predecessor without correct successor: " ++ 
                                            "\nactual succ=" ++ (show actualSuccIndex) ++
                                            "\nexpected succ=" ++ (show succBBIndex)
        SPLIT_BB_IR _ trueSuccIndex falseSuccIndex
            | succBBIndex == trueSuccIndex ->
                BasicBlockX86
                    (basicBlockX86MainCommands bbX86)
                    (basicBlockX86TailCommands bbX86)
                    ((basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++ phiFnComms)
                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86)
            | succBBIndex == falseSuccIndex ->
                BasicBlockX86
                    (basicBlockX86MainCommands bbX86)
                    (basicBlockX86TailCommands bbX86)
                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86)
                    ((basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++ phiFnComms)
            | otherwise ->
                error . compilerError $ "Attempted to inject phi-fn pred command to a SPLIT predecessor without correct successor: " ++ 
                                            "\ntrue succ=" ++ (show trueSuccIndex) ++
                                            "\false succ=" ++ (show falseSuccIndex) ++
                                            "\nexpected succ=" ++ (show succBBIndex)
        RET_PURE_IR _ ->
            error . compilerError $ "Attempted to inject phi-fn pred command to a RET predecessor: " ++ 
                                        "\nexpected succ=" ++ (show succBBIndex)

data AllocState = AllocState
    { allocStateRegMap :: Map.Map Int ArgLocation
    , allocStateStackCtr :: Int
    , allocStateAvailableReg :: [Register]
    }

getConstLoc :: Const -> ArgLocation
getConstLoc const =
    case const of
        INT_CONST int -> CONST_ARGLOC int
        BOOL_CONST bool -> 
            if bool
                then CONST_ARGLOC trueX86
                else CONST_ARGLOC falseX86



getVarLoc :: VariableIr -> Map.Map VariableIr Int -> AllocState -> (ArgLocation, AllocState)
getVarLoc var coloring alloc = 
    case Map.lookup var coloring of
        Just color ->
            case Map.lookup color (allocStateRegMap alloc) of
                Just argLoc -> (argLoc, alloc)
                Nothing -> allocColor color alloc
        Nothing -> error . compilerError $ "Attempted to lookup color for var that does not exist: var=" ++ (show var)

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

bbToLabel :: Int -> Label
bbToLabel bbIndex =
    if bbIndex == 0
        then "main"
        else "l" ++ (show bbIndex)
