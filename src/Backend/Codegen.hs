module Backend.Codegen (
    zippedProgIrToX86,
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

zippedProgIrToX86 :: [(FunctionIr, Coloring)] -> [X86Instruction]
zippedProgIrToX86 zippedProgIr
    | debugCodegenLogs && (Trace.trace
        ("\n\nzippedProgIrToX86 -- " ++
            "\nzippedProgIr=" ++ (Pretty.ppShow zippedProgIr)
        ) False) = undefined
zippedProgIrToX86 zippedProgIr = 
    concat $
    map
        (\(fnIr, coloring) -> irToX86 coloring fnIr)
        zippedProgIr

irToX86 :: Coloring -> FunctionIr -> [X86Instruction]
irToX86 coloring fnIr
    | debugCodegenLogs && (Trace.trace
        ("\n\nirToX86 -- " ++
            "\nname=" ++ (Pretty.ppShow (functionIrIdentifier fnIr)) ++ 
            "\ncoloring=" ++ (Pretty.ppShow coloring)
        ) False) = undefined
irToX86 coloring fnIr =
    let 
        (alloc, preprocessingInst) = fnCalleePreprocessing coloring fnIr

        -- apply phiFn Ir->x86 translation
        phiBlockMap = 
            foldl
                ( \interBlockMap index ->
                    case Map.lookup index (functionIrBlocks fnIr) of
                        Just bb -> phiFnIrToX86 coloring fnIr index (bbIrPhiFn bb) interBlockMap alloc
                        Nothing -> error . compilerError $ "Attempted to access basic block during phi-fn x86 translation: index=" ++ (show index)
                )
                Map.empty
                [0..((length . functionIrBlocks $ fnIr) - 1)]

        -- apply command Ir->x86 translation
        commBlockMap =
            foldl
                ( \interBlockMap index ->
                    case Map.lookup index (functionIrBlocks fnIr) of
                        Just bb -> bbIrCommsToX86 coloring bb interBlockMap alloc
                        Nothing -> error . compilerError $ "Attempted to access basic block during command x86 translation: index=" ++ (show index)
                )
                phiBlockMap
                [0..((length . functionIrBlocks $ fnIr) - 1)]

        -- concatenate x86 instructions
        finalInstr = 
            foldl
                (\interInstr index ->
                    case Map.lookup index commBlockMap of
                        Just bbX86 -> interInstr ++ (basicBlockX86MainCommands bbX86) ++ (basicBlockX86TailCommands bbX86)
                        Nothing -> error . compilerError $ "Attempted to access basic block during final X86 translation: index=" ++ (show index)
                )
                preprocessingInst
                [0..((length . functionIrBlocks $ fnIr) - 1)]
     in finalInstr

phiFnIrToX86 :: Coloring -> FunctionIr -> Int -> PhiFnIr -> Map.Map Int BasicBlockX86 -> AllocState -> Map.Map Int BasicBlockX86
phiFnIrToX86 coloring fnIr index phi initBlockMap alloc = 
    foldr
        -- generate an X86 inst for each asn var in the phi-fn and
        -- each load var in the predmap
        (\(var, varPredMap) interBlockMap ->
            foldr
                (\(predIndex, predVar) predInterBlockMap->
                    phiFnArgToX86 coloring fnIr (index, var) (predIndex, predVar) predInterBlockMap alloc
                )
                interBlockMap
                (Map.toList varPredMap)
        )
        initBlockMap
        (Map.toList phi)

-- converts a phi-fn arg (an entry in the pred map) to x86 + appends to the predecessor block
phiFnArgToX86 :: Coloring -> FunctionIr -> (Int, VariableIr) -> (Int, VariableIr) -> Map.Map Int BasicBlockX86 -> AllocState -> Map.Map Int BasicBlockX86
phiFnArgToX86 coloring fnIr (succIndex, asnVar) (predIndex, predVar) initBlockMap alloc = 
    let currBBX86 = 
            case Map.lookup predIndex initBlockMap of
                Just currBBX86 -> currBBX86
                Nothing -> BasicBlockX86 [] [] [] []

        -- allocate regs for asn var and pred var
        asnVarLoc = getVarLoc asnVar coloring alloc
        predVarLoc = getVarLoc predVar coloring alloc

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
        in Map.insert predIndex newBBX86 initBlockMap

-- converts bb main + tail commands to x86 
bbIrCommsToX86 :: Coloring -> BasicBlockIr -> Map.Map Int BasicBlockX86 -> AllocState -> Map.Map Int BasicBlockX86
bbIrCommsToX86 coloring bb initBlockMap alloc = 
    let currBBX86 = 
            case Map.lookup (bbIndex bb) initBlockMap of
                Just currBBX86 -> currBBX86
                Nothing -> BasicBlockX86 [] [] [] []

        -- convert main commands first
        mainInst = 
            foldr
                (\comm interInstr ->
                    let commInstr = mainCommIrToX86 coloring comm alloc
                    in interInstr ++ commInstr
                )
                [LABEL_X86 (bbToLabel (bbIrFnName bb) (bbIndex bb))]
                (tail . bbIrCommands $ bb)

        -- convert tail command w/ phi-fn succ injection
        tailInst = tailCommIrToX86 coloring (bbIrFnName bb) (head . bbIrCommands $ bb) currBBX86 alloc

        -- remove phi-fn insts and commit final BBX86
        newBBX86 = BasicBlockX86 mainInst tailInst [] []
        updatedBbBlockMap = Map.insert (bbIndex bb) newBBX86 initBlockMap
    in updatedBbBlockMap

-- MAIN COMM IR->x86

mainCommIrToX86 :: Coloring -> CommandIr -> AllocState -> [X86Instruction]
mainCommIrToX86 coloring comm alloc =
    case comm of
        INIT_IR var -> 
            []
        ASN_PURE_IR asnVar asnPure -> 
            asnPureIrToX86 coloring asnVar asnPure Nothing alloc
        ASN_IMPURE_IR asnVar asnImpure -> 
            asnImpureIrToX86 coloring asnVar asnImpure Nothing alloc
        DEREF_ASN_PURE_IR asnVar asnPure offset size ->
            asnPureIrToX86 coloring asnVar asnPure (Just (offset, size)) alloc
        ABORT_IR ->
            abortIrToX86 alloc
        _ ->
            error . compilerError $ "Attempted to translate ir->X86 non-main command as a main command: comm=" ++ (show comm)

asnPureIrToX86 :: Coloring -> VariableIr -> PureIr -> Maybe (Int, Int) -> AllocState -> [X86Instruction]
asnPureIrToX86 coloring asnVar asnPure m_OS alloc =
    let baseVarLoc = getVarLoc asnVar coloring alloc
        (asnInst, asnVarLoc) = 
            case baseVarLoc of
                REG_ARGLOC r ->
                    case m_OS of
                        Just (offset, size) ->
                            ([], REFREG_ARGLOC r (offset * size))
                        Nothing ->
                            ([], baseVarLoc)
                STACK_ARGLOC s -> 
                    case m_OS of
                        Just (offset, size) ->
                            ([ MOV_X86 (REG_ARGLOC CX) baseVarLoc
                            ],
                            (REFREG_ARGLOC CX (offset * size))
                            )
                        Nothing ->
                            ([], baseVarLoc)
    in case asnPure of
            PURE_BASE_IR base ->
                case base of
                    -- x = CONST
                    CONST_IR const ->
                        let constLoc = getConstLoc const
                            constInst =
                                [ MOV_X86 asnVarLoc constLoc
                                ]
                        in constInst
                    -- x = VAR
                    VAR_IR pureVar ->
                        let pureVarLoc = getVarLoc pureVar coloring alloc
                            pureInst = 
                                case pureVarLoc of
                                    REG_ARGLOC pureVarReg ->
                                        [ MOV_X86 asnVarLoc pureVarLoc
                                        ]
                                    STACK_ARGLOC pureVarStackLoc ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        ]
                        in pureInst
            -- x = y ? z
            PURE_BINOP_IR (PureBinopIr cat ty base1 base2) ->
                let pureVarLoc1 =
                        case base1 of
                            CONST_IR const -> getConstLoc const
                            VAR_IR pureVar -> getVarLoc pureVar coloring alloc
                    pureVarLoc2 =
                        case base2 of
                            CONST_IR const -> getConstLoc const
                            VAR_IR pureVar -> getVarLoc pureVar coloring alloc
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
                                    -- M[reg] = y + z -> perform binop in DX and move to M[reg]
                                    REFREG_ARGLOC _ _ ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , ADD_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
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
                                    -- M[reg] = y - z -> perform binop in DX and move to M[reg]
                                    REFREG_ARGLOC _ _ ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , SUB_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
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
                                    -- M[reg] = y * z -> perform binop in DX and move to M[reg]
                                    REFREG_ARGLOC _ _ ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , IMUL_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
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
                                    -- M[reg] = y & z -> perform binop in DX and move to M[reg]
                                    REFREG_ARGLOC _ _ ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , AND_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
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
                                    -- M[reg] = y ^ z -> perform binop in DX and move to M[reg]
                                    REFREG_ARGLOC _ _ ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , XOR_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
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
                                    -- M[reg] = y * z -> perform binop in DX and move to M[reg]
                                    REFREG_ARGLOC _ _ ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , OR_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
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
                                    -- M[reg] = y << z -> perform binop in DX and move to M[reg]
                                    REFREG_ARGLOC _ _ ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , SAL_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
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
                                    -- M[reg] = y >> z -> perform binop in DX and move to M[reg]
                                    STACK_ARGLOC asnVarStackLoc ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , SAR_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
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
                                    -- M[reg] = y < z -> perform binop in DX and move to M[reg]
                                    REFREG_ARGLOC _ _ ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , SUB_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , SHR_X86 (REG_ARGLOC DX) (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
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
                                    -- M[reg] = y < z -> perform binop in DX and move to M[reg]
                                    REFREG_ARGLOC _ _ ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , SUB_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , SHR_X86 (REG_ARGLOC DX) (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
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
                                    -- M[reg] = y <= z -> perform binop in DX and move to M[reg]
                                    REFREG_ARGLOC _ _ ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , SUB_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , NOT_X86 (REG_ARGLOC DX)
                                        , SHR_X86 (REG_ARGLOC DX) (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
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
                                    -- M[reg] = y >= z -> perform binop in DX and move to M[reg]
                                    REFREG_ARGLOC _ _ ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , SUB_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , NOT_X86 (REG_ARGLOC DX)
                                        , SHR_X86 (REG_ARGLOC DX) (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
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
                                    -- M[reg] = y == z -> perform binop in DX and move to M[reg]
                                    REFREG_ARGLOC _ _ ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , MOV_X86 (REG_ARGLOC AX) pureVarLoc2
                                        , SUB_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , SUB_X86 (REG_ARGLOC AX) pureVarLoc1
                                        , OR_X86 (REG_ARGLOC DX) (REG_ARGLOC AX)
                                        , NOT_X86 (REG_ARGLOC DX)
                                        , SHR_X86 (REG_ARGLOC DX) (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
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
                                    -- M[reg] = y != z -> perform binop in DX and move to M[reg]
                                    REFREG_ARGLOC _ _ ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , MOV_X86 (REG_ARGLOC AX) pureVarLoc2
                                        , SUB_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , SUB_X86 (REG_ARGLOC AX) pureVarLoc1
                                        , OR_X86 (REG_ARGLOC DX) (REG_ARGLOC AX)
                                        , SHR_X86 (REG_ARGLOC DX) (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
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
                in asnInst ++ binopInst
            PURE_UNOP_IR (PureUnopIr cat ty base) ->
                let pureVarLoc =
                        case base of
                            CONST_IR const -> getConstLoc const
                            VAR_IR pureVar -> getVarLoc pureVar coloring alloc
                    unopInst = 
                        case cat of
                            NEG_IR ->
                                case asnVarLoc of
                                    REG_ARGLOC asnVarReg ->
                                        [ MOV_X86 asnVarLoc pureVarLoc
                                        , NEG_X86 asnVarLoc
                                        ]
                                    REFREG_ARGLOC _ _ ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        , NEG_X86 asnVarLoc
                                        ]
                                    STACK_ARGLOC asnVarStackLock ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        , NEG_X86 asnVarLoc
                                        ]
                            NOT_IR ->
                                case asnVarLoc of
                                    REG_ARGLOC asnVarReg ->
                                        [ MOV_X86 asnVarLoc pureVarLoc
                                        , NOT_X86 asnVarLoc
                                        ]
                                    REFREG_ARGLOC _ _ ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        , NOT_X86 asnVarLoc
                                        ]
                                    STACK_ARGLOC asnVarStackLock ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        , NOT_X86 asnVarLoc
                                        ]
                            LOGNOT_IR ->
                                case asnVarLoc of
                                    REG_ARGLOC asnVarReg ->
                                        [ MOV_X86 asnVarLoc pureVarLoc
                                        , XOR_X86 asnVarLoc (CONST_ARGLOC trueX86)
                                        ]
                                    REFREG_ARGLOC _ _ ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        , XOR_X86 asnVarLoc (CONST_ARGLOC trueX86)
                                        ]
                                    STACK_ARGLOC asnVarStackLoc ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        , XOR_X86 asnVarLoc (CONST_ARGLOC trueX86)
                                        ]
                            REF_IR ->
                                case pureVarLoc of
                                    STACK_ARGLOC pureVarStackLoc ->
                                        [ MOV_X86 asnVarLoc (REG_ARGLOC SP)
                                        , ADD_X86 asnVarLoc (CONST_ARGLOC pureVarStackLoc)
                                        ]
                                    _ ->
                                        error . compilerError $ ("Attempted to get ref on non-stack variable=" ++ (displayArgLoc pureVarLoc))
                            DEREF_IR ->
                                case (asnVarLoc, pureVarLoc) of
                                    (REG_ARGLOC a, REG_ARGLOC p) ->
                                        [ MOV_X86 asnVarLoc (REFREG_ARGLOC p 0)
                                        ]
                                    (REG_ARGLOC a, STACK_ARGLOC p) ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc
                                        , MOV_X86 asnVarLoc (REFREG_ARGLOC DX 0)
                                        ]
                                    (REFREG_ARGLOC _ _, REG_ARGLOC p) ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        ]
                                    (REFREG_ARGLOC _ _, STACK_ARGLOC p) ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc
                                        , MOV_X86 (REG_ARGLOC CX) (REFREG_ARGLOC DX 0)
                                        , MOV_X86 asnVarLoc (REG_ARGLOC CX)
                                        ]
                                    (STACK_ARGLOC a, REG_ARGLOC p) ->
                                        [ MOV_X86 (REG_ARGLOC DX) (REFREG_ARGLOC p 0)
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        ]
                                    (STACK_ARGLOC a, STACK_ARGLOC p) ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc
                                        , MOV_X86 (REG_ARGLOC CX) (REFREG_ARGLOC DX 0)
                                        , MOV_X86 asnVarLoc (REG_ARGLOC CX)
                                        ]
                                    _ ->
                                        error . compilerError $ ("Attempted to apply deref onto non-reg/stack variable=" ++ (show base) ++ " location=" ++ (displayArgLoc pureVarLoc))
                in asnInst ++ unopInst

asnImpureIrToX86 :: Coloring -> VariableIr -> ImpureIr -> Maybe (Int, Int) -> AllocState -> [X86Instruction]
asnImpureIrToX86 coloring asnVar asnImpure m_OS alloc =
    let baseVarLoc = getVarLoc asnVar coloring alloc
        (asnInst, asnVarLoc) = 
            case baseVarLoc of
                REG_ARGLOC r ->
                    case m_OS of
                        Just (offset, size) ->
                            ([], REFREG_ARGLOC r (offset * size))
                        Nothing ->
                            ([], baseVarLoc)
                STACK_ARGLOC s -> 
                    case m_OS of
                        Just (offset, size) ->
                            ([ MOV_X86 (REG_ARGLOC CX) baseVarLoc
                            ],
                            (REFREG_ARGLOC CX (offset * size))
                            )
                        Nothing ->
                            ([], baseVarLoc)
    in case asnImpure of
            IMPURE_FNCALL_IR (ImpureFnCallIr fnIdentifier argBases retTy) ->
                let argVarLocs = 
                        map
                            (\argBase ->
                                case argBase of
                                    CONST_IR const -> getConstLoc const
                                    VAR_IR pureVar -> getVarLoc pureVar coloring alloc
                            )
                            argBases

                    callerSavePushInst = fnCallerPreprocessing alloc
                    argPushInst = 
                        concat $
                        map
                            (\argVarLoc -> 
                                case argVarLoc of
                                    CONST_ARGLOC argVarConst ->
                                        [ PUSH_X86 argVarLoc
                                        ]
                                    REG_ARGLOC argVarReg ->
                                        [ PUSH_X86 argVarLoc
                                        ]
                                    STACK_ARGLOC argVarStackLoc ->
                                        [ MOV_X86 (REG_ARGLOC DX) argVarLoc
                                        , PUSH_X86 (REG_ARGLOC DX)
                                        ]
                            )
                            (reverse argVarLocs)
                    callInst = 
                        [ CALL_X86 fnIdentifier
                        ]
                    copyRetInst = 
                        if retTy == VOID_TYPE
                            then []
                            else 
                                [ MOV_X86 asnVarLoc (REG_ARGLOC AX)
                                ]
                    argClearInst = 
                        [ ADD_X86 (REG_ARGLOC SP) (CONST_ARGLOC (registerSize * (length argVarLocs)))
                        ]
                    callerSavePopInst = fnCallerPostprocessing alloc
                in asnInst ++ callerSavePushInst ++ argPushInst ++ callInst ++ copyRetInst ++ argClearInst ++ callerSavePopInst

            IMPURE_BINOP_IR (ImpureBinopIr cat ty base1 base2) ->
                let pureVarLoc1 =
                        case base1 of
                            CONST_IR const -> getConstLoc const
                            VAR_IR pureVar -> getVarLoc pureVar coloring alloc
                    pureVarLoc2 =
                        case base2 of
                            CONST_IR const -> getConstLoc const
                            VAR_IR pureVar -> getVarLoc pureVar coloring alloc
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
                in asnInst ++ binopInst

abortIrToX86 :: AllocState -> [X86Instruction]
abortIrToX86 alloc = 
    [ CALL_X86 abort
    ]

-- TAIL COMM IR->x86

tailCommIrToX86 :: Coloring -> String -> CommandIr -> BasicBlockX86 -> AllocState -> [X86Instruction]
tailCommIrToX86 coloring fnName comm bbX86 alloc =
    case comm of
        GOTO_BB_IR bbIndex ->
            gotoToX86 fnName bbIndex bbX86 alloc
        SPLIT_BB_IR condPure splitTrue splitFalse ->
            splitToX86 coloring fnName condPure splitTrue splitFalse bbX86 alloc
        RET_PURE_IR retPure ->
            retToX86 coloring retPure bbX86 alloc
        RET_IR ->
            retNoneToX86 alloc


-- GOTO: prepends phi-fn-1  instrs. to JUMP instr.
gotoToX86 :: String -> Int -> BasicBlockX86 -> AllocState -> [X86Instruction]
gotoToX86 fnName index bbX86 alloc = 
    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++ [JMP_X86 (bbToLabel fnName index)]

-- SPLIT: prepends phi-fn-1/2 instrs. to CMP/JMP instrs. in the following order:
--   CMP
--   [true cond x86 insts.]
--   JMP (true)
--   [false cond x86 insts.]
--   JMP (false)
splitToX86 :: Coloring -> String -> PureIr -> Int -> Int -> BasicBlockX86 -> AllocState -> [X86Instruction]
splitToX86 coloring fnName condPure splitTrue splitFalse bbX86 alloc =         
    case condPure of
        PURE_BASE_IR base ->
            case base of
                CONST_IR const ->
                    let splitInst = 
                            case const of
                                BOOL_CONST True ->
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JMP_X86 (bbToLabel fnName splitTrue)
                                    ]
                                BOOL_CONST False ->
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel fnName splitFalse)
                                    ]
                    in splitInst
                VAR_IR pureVar ->
                    let pureVarLoc = getVarLoc pureVar coloring alloc
                        splitInst = 
                            [ CMP_X86 pureVarLoc (CONST_ARGLOC falseX86)
                            ] ++
                            (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                            [ JZ_X86 (bbToLabel fnName splitFalse)
                            ] ++
                            (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                            [ JMP_X86 (bbToLabel fnName splitTrue)
                            ]
                    in splitInst
        
        PURE_BINOP_IR (PureBinopIr cat ty base1 base2) ->
            let pureVarLoc1 =
                    case base1 of
                        CONST_IR const -> getConstLoc const
                        VAR_IR pureVar -> getVarLoc pureVar coloring alloc
                pureVarLoc2 =
                    case base2 of
                        CONST_IR const -> getConstLoc const
                        VAR_IR pureVar -> getVarLoc pureVar coloring alloc
                splitInst = 
                    case cat of
                        LT_IR -> 
                            case (pureVarLoc1, pureVarLoc2) of
                                -- c1 < c2 -> directly compare and jump
                                (CONST_ARGLOC pureVarConstLoc1, CONST_ARGLOC pureVarConstLoc2) ->
                                    if pureVarConstLoc1 < pureVarConstLoc2
                                        then 
                                            (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                            [ JMP_X86 (bbToLabel fnName splitTrue)
                                            ]
                                        else
                                            (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                            [ JMP_X86 (bbToLabel fnName splitFalse)
                                            ]
                                -- S[i] < S[j] -> move S[i] to DX
                                (STACK_ARGLOC pureVarStackLoc1, STACK_ARGLOC pureVarStackLoc2) ->
                                    [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                    , CMP_X86 (REG_ARGLOC DX) pureVarLoc2
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JL_X86 (bbToLabel fnName splitTrue)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel fnName splitFalse)
                                    ]
                                -- at least one reg --> direct cmp
                                _ ->
                                    [ CMP_X86 pureVarLoc1 pureVarLoc2
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JL_X86 (bbToLabel fnName splitTrue)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel fnName splitFalse)
                                    ]
                        LTE_IR ->
                            case (pureVarLoc1, pureVarLoc2) of
                                -- c1 <= c2 -> directly compare and jump
                                (CONST_ARGLOC pureVarConstLoc1, CONST_ARGLOC pureVarConstLoc2) ->
                                    if pureVarConstLoc1 <= pureVarConstLoc2
                                        then 
                                            (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                            [ JMP_X86 (bbToLabel fnName splitTrue)
                                            ]
                                        else
                                            (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                            [ JMP_X86 (bbToLabel fnName splitFalse)
                                            ]
                                -- S[i] <= S[j] -> move S[i] to DX
                                (STACK_ARGLOC pureVarStackLoc1, STACK_ARGLOC pureVarStackLoc2) ->
                                    [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                    , CMP_X86 (REG_ARGLOC DX) pureVarLoc2
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JLE_X86 (bbToLabel fnName splitTrue)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel fnName splitFalse)
                                    ]
                                -- at least one reg --> direct cmp
                                _ ->
                                    [ CMP_X86 pureVarLoc1 pureVarLoc2
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JLE_X86 (bbToLabel fnName splitTrue)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel fnName splitFalse)
                                    ]
                        GT_IR ->
                            case (pureVarLoc1, pureVarLoc2) of
                                -- c1 > c2 -> directly compare and jump
                                (CONST_ARGLOC pureVarConstLoc1, CONST_ARGLOC pureVarConstLoc2) ->
                                    if pureVarConstLoc1 > pureVarConstLoc2
                                        then 
                                            (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                            [ JMP_X86 (bbToLabel fnName splitTrue)
                                            ]
                                        else
                                            (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                            [ JMP_X86 (bbToLabel fnName splitFalse)
                                            ]
                                -- S[i] > S[j] -> move S[i] to DX
                                (STACK_ARGLOC pureVarStackLoc1, STACK_ARGLOC pureVarStackLoc2) ->
                                    [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                    , CMP_X86 (REG_ARGLOC DX) pureVarLoc2
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JG_X86 (bbToLabel fnName splitTrue)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel fnName splitFalse)
                                    ]
                                -- at least one reg --> direct cmp
                                _ ->
                                    [ CMP_X86 pureVarLoc1 pureVarLoc2
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JG_X86 (bbToLabel fnName splitTrue)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel fnName splitFalse)
                                    ]
                        GTE_IR ->
                            case (pureVarLoc1, pureVarLoc2) of
                                -- c1 >= c2 -> directly compare and jump
                                (CONST_ARGLOC pureVarConstLoc1, CONST_ARGLOC pureVarConstLoc2) ->
                                    if pureVarConstLoc1 >= pureVarConstLoc2
                                        then 
                                            (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                            [ JMP_X86 (bbToLabel fnName splitTrue)
                                            ]
                                        else
                                            (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                            [ JMP_X86 (bbToLabel fnName splitFalse)
                                            ]
                                -- S[i] >= S[j] -> move S[i] to DX
                                (STACK_ARGLOC pureVarStackLoc1, STACK_ARGLOC pureVarStackLoc2) ->
                                    [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                    , CMP_X86 (REG_ARGLOC DX) pureVarLoc2
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JGE_X86 (bbToLabel fnName splitTrue)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel fnName splitFalse)
                                    ]
                                -- at least one reg --> direct cmp
                                _ ->
                                    [ CMP_X86 pureVarLoc1 pureVarLoc2
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JGE_X86 (bbToLabel fnName splitTrue)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel fnName splitFalse)
                                    ]
                        EQ_IR ->
                            case (pureVarLoc1, pureVarLoc2) of
                                -- c1 == c2 -> directly compare and jump
                                (CONST_ARGLOC pureVarConstLoc1, CONST_ARGLOC pureVarConstLoc2) ->
                                    if pureVarConstLoc1 == pureVarConstLoc2
                                        then 
                                            (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                            [ JMP_X86 (bbToLabel fnName splitTrue)
                                            ]
                                        else
                                            (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                            [ JMP_X86 (bbToLabel fnName splitFalse)
                                            ]
                                -- S[i] == S[j] -> move S[i] to DX
                                (STACK_ARGLOC pureVarStackLoc1, STACK_ARGLOC pureVarStackLoc2) ->
                                    [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                    , CMP_X86 (REG_ARGLOC DX) pureVarLoc2
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JE_X86 (bbToLabel fnName splitTrue)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel fnName splitFalse)
                                    ]
                                -- at least one reg --> direct cmp
                                _ ->
                                    [ CMP_X86 pureVarLoc1 pureVarLoc2
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JE_X86 (bbToLabel fnName splitTrue)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel fnName splitFalse)
                                    ]
                        NEQ_IR ->
                            case (pureVarLoc1, pureVarLoc2) of
                                -- c1 != c2 -> directly compare and jump
                                (CONST_ARGLOC pureVarConstLoc1, CONST_ARGLOC pureVarConstLoc2) ->
                                    if pureVarConstLoc1 /= pureVarConstLoc2
                                        then 
                                            (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                            [ JMP_X86 (bbToLabel fnName splitTrue)
                                            ]
                                        else
                                            (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                            [ JMP_X86 (bbToLabel fnName splitFalse)
                                            ]
                                -- S[i] != S[j] -> move S[i] to DX
                                (STACK_ARGLOC pureVarStackLoc1, STACK_ARGLOC pureVarStackLoc2) ->
                                    [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                    , CMP_X86 (REG_ARGLOC DX) pureVarLoc2
                                    ] ++ 
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JNE_X86 (bbToLabel fnName splitTrue)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel fnName splitFalse)
                                    ]
                                -- at least one reg --> direct cmp
                                _ ->
                                    [ CMP_X86 pureVarLoc1 pureVarLoc2
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JNE_X86 (bbToLabel fnName splitTrue)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel fnName splitFalse)
                                    ]
            in splitInst

        PURE_UNOP_IR (PureUnopIr cat ty base) ->
            let pureVarLoc =
                    case base of
                        CONST_IR const -> getConstLoc const
                        VAR_IR pureVar -> getVarLoc pureVar coloring alloc
                splitInst = 
                    case cat of
                        LOGNOT_IR ->
                            case pureVarLoc of
                                CONST_ARGLOC pureVarConstLoc ->
                                    if pureVarConstLoc == falseX86
                                        then
                                            (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                            [ JMP_X86 (bbToLabel fnName splitTrue)
                                            ]
                                        else
                                            (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                            [ JMP_X86 (bbToLabel fnName splitFalse)
                                            ]
                                _ ->
                                    [ CMP_X86 pureVarLoc (CONST_ARGLOC falseX86)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JZ_X86 (bbToLabel fnName splitTrue)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel fnName splitFalse)
                                    ]
                        DEREF_IR ->
                            case pureVarLoc of
                                REG_ARGLOC p ->
                                    [ CMP_X86 (REFREG_ARGLOC p 0) (CONST_ARGLOC trueX86)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JZ_X86 (bbToLabel fnName splitTrue)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel fnName splitFalse)
                                    ]
                                STACK_ARGLOC p ->
                                    [ MOV_X86 (REG_ARGLOC DX) pureVarLoc
                                    , CMP_X86 (REFREG_ARGLOC DX 0) (CONST_ARGLOC trueX86)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JZ_X86 (bbToLabel fnName splitTrue)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel fnName splitFalse)
                                    ]
                                _ ->
                                    error . compilerError $ ("Attempted to apply deref onto non-reg/stack variable=" ++ (show base) ++ " location=" ++ (displayArgLoc pureVarLoc))
                    in splitInst

-- RET: prepends no phi-fn insts. to ret inst.
retToX86 :: Coloring -> PureIr -> BasicBlockX86 -> AllocState -> [X86Instruction]
retToX86 coloring retPure bbX86 alloc =
    case retPure of
        PURE_BASE_IR base ->
            let pureVarLoc =
                    case base of
                        CONST_IR const -> getConstLoc const
                        VAR_IR pureVar -> getVarLoc pureVar coloring alloc
                retInst = 
                    [ MOV_X86 (REG_ARGLOC AX) pureVarLoc
                    ] ++
                    (fnCalleePostprocessing alloc)
            in retInst
        -- ret y ? z
        PURE_BINOP_IR (PureBinopIr cat ty base1 base2) ->
            let pureVarLoc1 =
                    case base1 of
                        CONST_IR const -> getConstLoc const
                        VAR_IR pureVar -> getVarLoc pureVar coloring alloc
                pureVarLoc2 =
                    case base2 of
                        CONST_IR const -> getConstLoc const
                        VAR_IR pureVar -> getVarLoc pureVar coloring alloc
                retInst = 
                    case cat of
                        ADD_IR ->
                            [ MOV_X86 (REG_ARGLOC AX) pureVarLoc1
                            , ADD_X86 (REG_ARGLOC AX) pureVarLoc2
                            ] ++
                            (fnCalleePostprocessing alloc)
                        SUB_IR ->
                            [ MOV_X86 (REG_ARGLOC AX) pureVarLoc1
                            , SUB_X86 (REG_ARGLOC AX) pureVarLoc2
                            ] ++
                            (fnCalleePostprocessing alloc)
                        MUL_IR ->
                            [ MOV_X86 (REG_ARGLOC AX) pureVarLoc1
                            , IMUL_X86 (REG_ARGLOC AX) pureVarLoc2
                            ] ++
                            (fnCalleePostprocessing alloc)
                        AND_IR ->
                            [ MOV_X86 (REG_ARGLOC AX) pureVarLoc1
                            , AND_X86 (REG_ARGLOC AX) pureVarLoc2
                            ] ++
                            (fnCalleePostprocessing alloc)
                        XOR_IR ->
                            [ MOV_X86 (REG_ARGLOC AX) pureVarLoc1
                            , XOR_X86 (REG_ARGLOC AX) pureVarLoc2
                            ] ++
                            (fnCalleePostprocessing alloc)
                        OR_IR ->
                            [ MOV_X86 (REG_ARGLOC AX) pureVarLoc1
                            , OR_X86 (REG_ARGLOC AX) pureVarLoc2
                            ] ++
                            (fnCalleePostprocessing alloc)
                        SAL_IR ->
                            [ MOV_X86 (REG_ARGLOC AX) pureVarLoc1
                            , SAL_X86 (REG_ARGLOC AX) pureVarLoc2
                            ] ++
                            (fnCalleePostprocessing alloc)
                        SAR_IR ->
                            [ MOV_X86 (REG_ARGLOC AX) pureVarLoc1
                            , SAR_X86 (REG_ARGLOC AX) pureVarLoc2
                            ] ++
                            (fnCalleePostprocessing alloc)
                        LT_IR ->
                            [ MOV_X86 (REG_ARGLOC AX) pureVarLoc1
                            , SUB_X86 (REG_ARGLOC AX) pureVarLoc2
                            , SHR_X86 (REG_ARGLOC AX) (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                            ] ++
                            (fnCalleePostprocessing alloc)
                        GT_IR ->
                            [ MOV_X86 (REG_ARGLOC AX) pureVarLoc2
                            , SUB_X86 (REG_ARGLOC AX) pureVarLoc1
                            , SHR_X86 (REG_ARGLOC AX) (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                            ] ++
                            (fnCalleePostprocessing alloc)
                        LTE_IR ->
                            [ MOV_X86 (REG_ARGLOC AX) pureVarLoc2
                            , SUB_X86 (REG_ARGLOC AX) pureVarLoc1
                            , NOT_X86 (REG_ARGLOC AX)
                            , SHR_X86 (REG_ARGLOC AX) (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                            ] ++
                            (fnCalleePostprocessing alloc)
                        GTE_IR ->
                            [ MOV_X86 (REG_ARGLOC AX) pureVarLoc1
                            , SUB_X86 (REG_ARGLOC AX) pureVarLoc2
                            , NOT_X86 (REG_ARGLOC AX)
                            , SHR_X86 (REG_ARGLOC AX) (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                            ] ++
                            (fnCalleePostprocessing alloc)
                        EQ_IR ->
                            [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                            , MOV_X86 (REG_ARGLOC AX) pureVarLoc2
                            , SUB_X86 (REG_ARGLOC DX) pureVarLoc2
                            , SUB_X86 (REG_ARGLOC AX) pureVarLoc1
                            , OR_X86 (REG_ARGLOC AX) (REG_ARGLOC DX)
                            , NOT_X86 (REG_ARGLOC AX)
                            , SHR_X86 (REG_ARGLOC AX) (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                            ] ++
                            (fnCalleePostprocessing alloc)
                        NEQ_IR ->
                            [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                            , MOV_X86 (REG_ARGLOC AX) pureVarLoc2
                            , SUB_X86 (REG_ARGLOC DX) pureVarLoc2
                            , SUB_X86 (REG_ARGLOC AX) pureVarLoc1
                            , OR_X86 (REG_ARGLOC AX) (REG_ARGLOC DX)
                            , SHR_X86 (REG_ARGLOC AX) (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                            ] ++
                            (fnCalleePostprocessing alloc)
            in retInst
        PURE_UNOP_IR (PureUnopIr cat ty base) ->
            let pureVarLoc =
                    case base of
                        CONST_IR const -> getConstLoc const
                        VAR_IR pureVar -> getVarLoc pureVar coloring alloc
                retInst = 
                    case cat of
                        NEG_IR ->
                            [ MOV_X86 (REG_ARGLOC AX) pureVarLoc
                            , NEG_X86 (REG_ARGLOC AX)
                            ] ++
                            (fnCalleePostprocessing alloc)
                        NOT_IR ->
                            [ MOV_X86 (REG_ARGLOC AX) pureVarLoc
                            , NOT_X86 (REG_ARGLOC AX)
                            ] ++
                            (fnCalleePostprocessing alloc)
                        LOGNOT_IR ->
                            [ MOV_X86 (REG_ARGLOC AX) pureVarLoc
                            , XOR_X86 (REG_ARGLOC AX) (CONST_ARGLOC trueX86)
                            ] ++
                            (fnCalleePostprocessing alloc)
                        REF_IR ->
                            case pureVarLoc of
                                STACK_ARGLOC pureVarStackLoc ->
                                    [ MOV_X86 (REG_ARGLOC AX) (REG_ARGLOC SP)
                                    , ADD_X86 (REG_ARGLOC AX) (CONST_ARGLOC pureVarStackLoc)
                                    ]
                                _ ->
                                    error . compilerError $ ("Attempted to get ref on non-stack variable=" ++ (displayArgLoc pureVarLoc))
                        DEREF_IR ->
                            case pureVarLoc of
                                REG_ARGLOC p ->
                                    [ MOV_X86 (REG_ARGLOC AX) (REFREG_ARGLOC p 0)
                                    ] ++
                                    (fnCalleePostprocessing alloc)
                                STACK_ARGLOC p ->
                                    [ MOV_X86 (REG_ARGLOC DX) pureVarLoc
                                    , MOV_X86 (REG_ARGLOC AX) (REFREG_ARGLOC DX 0)
                                    ] ++
                                    (fnCalleePostprocessing alloc)
                                _ ->
                                    error . compilerError $ ("Attempted to apply deref onto non-reg/stack variable=" ++ (show base) ++ " location=" ++ (displayArgLoc pureVarLoc))
            in retInst

retNoneToX86 :: AllocState -> [X86Instruction]
retNoneToX86 alloc = 
    fnCalleePostprocessing alloc

-- HELPERS

fnCalleePreprocessing :: Coloring -> FunctionIr -> (AllocState, [X86Instruction])
fnCalleePreprocessing coloring fnIr = 
    let 
        -- initialize function spillover at SP + 0 and w/ all available registers
        -- for each color allocate a register if the color has not been alloc'd yet
        alloc = 
            foldr
                (\(var, color) interAlloc ->
                    case Map.lookup color (allocStateRegMap interAlloc) of
                        Just argLoc -> interAlloc
                        Nothing -> 
                            let isStackColor = Set.member color (coloringStackVars coloring)
                            in allocColor color isStackColor interAlloc
                )
                (AllocState Map.empty 0 availableRegisters)
                (Map.toList . coloringMap $ coloring)

        -- pre-processing: 
        -- - push callee-saved registers (including BP) onto stack
        -- - save old SP in BP
        -- - decrement SP to make room for alloc'd vars
        -- - copy function arguments from SP + 8, 16, ... into their locations on the stack
        fnLabel = [LABEL_X86 (fnToLabel fnIr)]
        calleeSavePushInst = 
            map (\reg -> PUSH_X86 (REG_ARGLOC reg)) .
            filter (\reg -> not (elem reg (allocStateAvailableReg alloc))) $
            calleeSavedRegisters
        saveSPInst = [MOV_X86 (REG_ARGLOC BP) (REG_ARGLOC SP)]
        spDecrInst = [SUB_X86 (REG_ARGLOC SP) (CONST_ARGLOC (allocStateStackCtr alloc))]
        (argAsnInst, _) = 
            foldl
                (\(interInst, interArgSPOffset) asnVar ->
                    let asnVarLoc = getVarLoc asnVar coloring alloc
                        asnInst = 
                            case asnVarLoc of
                                REG_ARGLOC asnVarReg ->
                                    [ MOV_X86 asnVarLoc (BASE_ARGLOC interArgSPOffset)
                                    ]
                                STACK_ARGLOC asnVarStackLoc ->
                                    [ MOV_X86 (REG_ARGLOC DX) (BASE_ARGLOC interArgSPOffset)
                                    , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                    ]
                    in (interInst ++ asnInst, interArgSPOffset + registerSize)
                )
                ([], (1 + (length calleeSavePushInst)) * registerSize)
                (functionIrArgs fnIr)
        preprocessingInst = fnLabel ++ calleeSavePushInst ++ saveSPInst ++ spDecrInst ++ argAsnInst
    in (alloc, preprocessingInst)

fnCalleePostprocessing :: AllocState -> [X86Instruction]
fnCalleePostprocessing alloc = 
    let 
        -- post-processing:
        -- - increment SP to clear alloc'd vars
        -- - restore old SP from BP
        -- - pop callee-saved registers from stack
        spIncrInst = [ADD_X86 (REG_ARGLOC SP) (CONST_ARGLOC (allocStateStackCtr alloc))]
        restoreSPInst = [MOV_X86 (REG_ARGLOC SP) (REG_ARGLOC BP)]
        calleeSavePopInst = 
            map (\reg -> POP_X86 (REG_ARGLOC reg)) .
            filter (\reg -> not (elem reg (allocStateAvailableReg alloc))) .
            reverse $
            calleeSavedRegisters
        retInst = [RET_X86]
        postprocessingInst = spIncrInst ++ restoreSPInst ++ calleeSavePopInst ++ retInst
    in postprocessingInst

fnCallerPreprocessing :: AllocState -> [X86Instruction]
fnCallerPreprocessing alloc = 
    map (\reg -> PUSH_X86 (REG_ARGLOC reg)) .
    filter (\reg -> not (elem reg (allocStateAvailableReg alloc))) $
    callerSavedRegisters

fnCallerPostprocessing :: AllocState -> [X86Instruction]
fnCallerPostprocessing alloc = 
    map (\reg -> POP_X86 (REG_ARGLOC reg)) .
    filter (\reg -> not (elem reg (allocStateAvailableReg alloc))) .
    reverse $
    callerSavedRegisters

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
    | debugCodegenLogs && (Trace.trace
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
        RET_IR ->
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
        NULL_CONST -> CONST_ARGLOC nullX86

getVarLoc :: VariableIr -> Coloring -> AllocState -> ArgLocation
getVarLoc var coloring alloc = 
    case Map.lookup var (coloringMap coloring) of
        Just color ->
            case Map.lookup color (allocStateRegMap alloc) of
                Just argLoc -> argLoc
                Nothing -> error . compilerError $ "Attempted to lookup var alloc for color that was not alloc'd: color=" ++ (show color)
        Nothing -> error . compilerError $ "Attempted to lookup color for var that does not exist: var=" ++ (show var)

-- explicitly allocate a register/stack loc for new color on assignment
allocColor :: Int -> Bool -> AllocState -> AllocState 
allocColor color isStackColor initAlloc =
    if isStackColor || (null . allocStateAvailableReg $ initAlloc)
        then
            let argLoc = STACK_ARGLOC (allocStateStackCtr initAlloc)
                newAlloc =
                    AllocState
                        (Map.insert color argLoc (allocStateRegMap initAlloc))
                        ((allocStateStackCtr initAlloc) + registerSize)
                        (allocStateAvailableReg initAlloc)
             in newAlloc
        else
            let reg = head . allocStateAvailableReg $ initAlloc
                argLoc = REG_ARGLOC reg
                newAlloc =
                    AllocState
                        (Map.insert color argLoc (allocStateRegMap initAlloc))
                        (allocStateStackCtr initAlloc)
                        (tail (allocStateAvailableReg initAlloc))
             in newAlloc

fnToLabel :: FunctionIr -> Label
fnToLabel fnIr = (functionIrIdentifier fnIr)

bbToLabel :: String -> Int -> Label
bbToLabel fnName bbIndex = fnName ++ "_l_" ++ (show bbIndex)
