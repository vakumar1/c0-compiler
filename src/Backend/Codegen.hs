module Backend.Codegen (
    zippedProgIrToX86,
)
where

import Model.Ir
import Model.Types
import Model.X86
import Common.Liveness
import Common.IrUtils
import Common.Errors
import Common.Constants

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe

import qualified Text.Show.Pretty as Pretty
import qualified Debug.Trace as Trace

zippedProgIrToX86 :: StructContext -> [(FunctionIr, Coloring)] -> [X86Instruction]
zippedProgIrToX86 structCtx zippedProgIr
    | debugCodegenLogs && (Trace.trace
        ("\n\nzippedProgIrToX86 -- " ++
            "\nzippedProgIr=" ++ (Pretty.ppShow zippedProgIr)
        ) False) = undefined
zippedProgIrToX86 structCtx zippedProgIr = 
    concat $
    map
        (\(fnIr, coloring) -> irToX86 structCtx coloring fnIr)
        zippedProgIr

irToX86 :: StructContext -> Coloring -> FunctionIr -> [X86Instruction]
irToX86 structCtx coloring fnIr
    | debugCodegenLogs && (Trace.trace
        ("\n\nirToX86 -- " ++
            "\nname=" ++ (Pretty.ppShow (functionIrIdentifier fnIr)) ++ 
            "\ncoloring=" ++ (Pretty.ppShow coloring)
        ) False) = undefined
irToX86 structCtx coloring fnIr =
    let 
        (alloc, preprocessingInst) = fnCalleePreprocessing structCtx coloring fnIr

        -- apply phiFn Ir->x86 translation
        phiBlockMap = 
            foldl
                ( \interBlockMap index ->
                    phiFnIrToX86 coloring fnIr index (bbIrPhiFn $ getBB fnIr index) interBlockMap alloc
                )
                Map.empty
                [0..((length . functionIrBlocks $ fnIr) - 1)]

        -- apply command Ir->x86 translation
        commBlockMap =
            foldl
                ( \interBlockMap index ->
                    bbIrCommsToX86 coloring (getBB fnIr index) interBlockMap alloc
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
            case (asnVarLoc, predVarLoc) of
                (REFREG_ARGLOC _ _ _, REFREG_ARGLOC _ _ _) ->
                    [ MOV_X86 (REG_ARGLOC DX) predVarLoc
                    , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                    ]
                _ ->
                    [ MOV_X86 asnVarLoc predVarLoc
                    ]

        -- attempt to inject the phi-fn inst to the predecessor bbX86
        newBBX86 = injectPhiFnPredCommand (head . bbIrCommands $ getBB fnIr predIndex) asnInst succIndex currBBX86
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
        ASN_PURE_IR asnMemop asnVar asnPure -> 
            asnPureIrToX86 coloring asnMemop asnVar asnPure alloc
        ASN_IMPURE_IR asnVar asnImpure -> 
            asnImpureIrToX86 coloring asnVar asnImpure alloc
        ABORT_IR ->
            abortIrToX86 alloc
        _ ->
            error . compilerError $ "Attempted to translate ir->X86 non-main command as a main command: comm=" ++ (show comm)

asnPureIrToX86 :: Coloring -> MemopIr -> VariableIr -> PureIr -> AllocState -> [X86Instruction]
asnPureIrToX86 coloring asnMemop asnVar asnPure alloc =
    let baseVarLoc = getVarLoc asnVar coloring alloc
        (offsetInst, m_superOffset) = 
            case memopIrOffset asnMemop of
                Nothing ->
                    ([], Nothing)
                Just offsetPuB ->
                    case offsetPuB of
                        CONST_IR c ->
                            case c of
                                INT_CONST i ->
                                    ([], Just (Right i))
                                _ ->
                                    error . compilerError $ "Attempted use non-int const in memory offset" ++ (show offsetPuB)
                        VAR_IR offsetVar ->
                            let offsetVarLoc = (getVarLoc offsetVar coloring alloc)
                            in 
                                case offsetVarLoc of
                                    REG_ARGLOC offsetVarReg ->
                                        ([], Just (Left offsetVarReg))
                                    REFREG_ARGLOC _ _ _ ->
                                        ([ MOV_X86 (REG_ARGLOC CX) offsetVarLoc
                                        ],
                                        Just (Left CX))

        (asnInst, asnVarLoc) = 
            if not . memopIrIsDeref $ asnMemop
                then
                    case baseVarLoc of
                        REG_ARGLOC _ ->
                            if (Maybe.isNothing . memopIrOffset $ asnMemop)
                                then
                                    ([], baseVarLoc)
                                else
                                    error . compilerError $ ("Attempted to add offset within register variable=" ++ (show asnVar))
                        REFREG_ARGLOC varReg m_currSuperOffset baseOffset ->
                            case m_currSuperOffset of
                                Just _ ->
                                    error . compilerError $ ("Attempted to overwrite non-empty super offset for mem variable=" ++ (displayArgLoc baseVarLoc))
                                Nothing ->
                                    (offsetInst, REFREG_ARGLOC varReg m_superOffset baseOffset)
                else
                    case baseVarLoc of
                        REG_ARGLOC varReg ->
                            (offsetInst, REFREG_ARGLOC varReg m_superOffset 0)
                        REFREG_ARGLOC _ _ _ ->
                            (offsetInst ++
                            [ MOV_X86 (REG_ARGLOC BX) baseVarLoc
                            ],
                            REFREG_ARGLOC BX m_superOffset 0)

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
                                case (asnVarLoc, pureVarLoc) of
                                    (REFREG_ARGLOC _ _ _, REFREG_ARGLOC _ _ _) ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        ]
                                    _ ->
                                        [ MOV_X86 asnVarLoc pureVarLoc
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
                                    REG_ARGLOC _
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
                                    REFREG_ARGLOC _ _ _ ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , ADD_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        ]
                            SUB_IR ->
                                case asnVarLoc of
                                    -- reg = y - z -> perform binop in reg
                                    REG_ARGLOC _
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
                                    REFREG_ARGLOC _ _ _ ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , SUB_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        ]
                            MUL_IR ->
                                case asnVarLoc of
                                    -- reg = y * z -> perform binop in reg
                                    REG_ARGLOC _
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
                                    REFREG_ARGLOC _ _ _ ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , IMUL_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        ]
                            AND_IR ->
                                case asnVarLoc of
                                    -- reg = y & z -> perform binop in reg
                                    REG_ARGLOC _
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
                                    REFREG_ARGLOC _ _ _ ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , AND_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        ]
                            XOR_IR ->
                                case asnVarLoc of
                                    -- reg = y ^ z -> perform binop in reg
                                    REG_ARGLOC _
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
                                    REFREG_ARGLOC _ _ _ ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , XOR_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        ]
                            OR_IR ->
                                case asnVarLoc of
                                    -- reg = y | z -> perform binop in reg
                                    REG_ARGLOC _
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
                                    REFREG_ARGLOC _ _ _ ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , OR_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        ]
                            SAL_IR ->
                                case asnVarLoc of
                                    -- reg = y << z -> perform binop in reg
                                    REG_ARGLOC _
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
                                    REFREG_ARGLOC _ _ _ ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , SAL_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        ]
                            SAR_IR ->
                                case asnVarLoc of
                                    -- reg = y >> z -> perform binop in reg
                                    REG_ARGLOC _
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
                                    REFREG_ARGLOC _ _ _ ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , SAR_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        ]
                            -- boolean binop insts.
                            LT_IR ->
                                case asnVarLoc of
                                    -- reg = y < z -> perform binop in reg
                                    REG_ARGLOC _
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
                                    REFREG_ARGLOC _ _ _ ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , SUB_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , SHR_X86 (REG_ARGLOC DX) (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        ]
                            GT_IR ->
                                case asnVarLoc of
                                    -- reg = y > z -> perform binop in reg
                                    REG_ARGLOC _
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
                                    REFREG_ARGLOC _ _ _ ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , SUB_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , SHR_X86 (REG_ARGLOC DX) (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        ]
                            LTE_IR ->
                                case asnVarLoc of
                                    -- reg = y <= z -> perform binop in reg
                                    REG_ARGLOC _
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
                                    REFREG_ARGLOC _ _ _ ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , SUB_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , NOT_X86 (REG_ARGLOC DX)
                                        , SHR_X86 (REG_ARGLOC DX) (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        ]
                            GTE_IR ->
                                case asnVarLoc of
                                    -- reg = y >= z -> perform binop in reg
                                    REG_ARGLOC _
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
                                    REFREG_ARGLOC _ _ _ ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc1
                                        , SUB_X86 (REG_ARGLOC DX) pureVarLoc2
                                        , NOT_X86 (REG_ARGLOC DX)
                                        , SHR_X86 (REG_ARGLOC DX) (getConstLoc (INT_CONST (registerSize * 8 - 1)))
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        ]
                            EQ_IR ->
                                case asnVarLoc of
                                    -- reg = y == z -> perform binop in reg
                                    REG_ARGLOC _
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
                                    REFREG_ARGLOC _ _ _ ->
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
                                    REG_ARGLOC _
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
                                    REFREG_ARGLOC _ _ _ ->
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
                                    REFREG_ARGLOC _ _ _ ->
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
                                    REFREG_ARGLOC _ _ _ ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        , NOT_X86 asnVarLoc
                                        ]
                            LOGNOT_IR ->
                                case asnVarLoc of
                                    REG_ARGLOC _ ->
                                        [ MOV_X86 asnVarLoc pureVarLoc
                                        , XOR_X86 asnVarLoc (CONST_ARGLOC trueX86)
                                        ]
                                    REFREG_ARGLOC _ _ _ ->
                                        [ MOV_X86 (REG_ARGLOC DX) pureVarLoc
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        , XOR_X86 asnVarLoc (CONST_ARGLOC trueX86)
                                        ]
                            REF_IR ->
                                case pureVarLoc of
                                    REFREG_ARGLOC reg m_superOffset baseOffset ->
                                        case m_superOffset of
                                            Nothing ->
                                                if baseOffset == 0
                                                    then 
                                                        [ MOV_X86 asnVarLoc (REG_ARGLOC reg)
                                                        ]
                                                    else
                                                        [ MOV_X86 asnVarLoc (REG_ARGLOC reg)
                                                        , ADD_X86 asnVarLoc (CONST_ARGLOC baseOffset)
                                                        ]
                                            Just superOffsetBase ->
                                                case superOffsetBase of
                                                    Left superOffsetReg ->
                                                        if baseOffset == 0
                                                            then 
                                                                [ MOV_X86 asnVarLoc (REG_ARGLOC reg)
                                                                , ADD_X86 asnVarLoc (REG_ARGLOC superOffsetReg)
                                                                ]
                                                            else
                                                                [ MOV_X86 asnVarLoc (REG_ARGLOC reg)
                                                                , ADD_X86 asnVarLoc (CONST_ARGLOC baseOffset)
                                                                , ADD_X86 asnVarLoc (REG_ARGLOC superOffsetReg)
                                                                ]
                                                    Right superOffsetInt ->
                                                        if (superOffsetInt + baseOffset) == 0
                                                            then
                                                                [ MOV_X86 asnVarLoc (REG_ARGLOC reg)
                                                                ]
                                                            else
                                                                [ MOV_X86 asnVarLoc (REG_ARGLOC reg)
                                                                , ADD_X86 asnVarLoc (CONST_ARGLOC (superOffsetInt + baseOffset))
                                                                ]
                                    _ ->
                                        error . compilerError $ ("Attempted to get ref on non-stack variable=" ++ (displayArgLoc pureVarLoc))
                in asnInst ++ unopInst

            PURE_MEMOP_IR var memop ->
                let varLoc = getVarLoc var coloring alloc
                    (offsetInst, m_superOffset) = 
                        case memopIrOffset memop of
                            Nothing ->
                                ([], Nothing)
                            Just offsetPuB ->
                                case offsetPuB of
                                    CONST_IR c -> 
                                        case c of
                                            INT_CONST i ->
                                                ([], Just (Right i))
                                            _ ->
                                                error . compilerError $ ("Attempted use non-int const for memory offset const=" ++ (show offsetPuB))
                                    VAR_IR var -> 
                                        let superOffsetVarLoc = getVarLoc var coloring alloc
                                        in 
                                            case superOffsetVarLoc of
                                                REG_ARGLOC reg ->
                                                    ([], Just (Left reg))
                                                REFREG_ARGLOC _ _ _ ->
                                                    ([ MOV_X86 (REG_ARGLOC AX) superOffsetVarLoc
                                                    ], Just (Left AX))
                    memopInst =
                        if not . memopIrIsDeref $ memop 
                            then
                                if Maybe.isNothing . memopIrOffset $ memop
                                    then
                                        error . compilerError $ "Encountered memopIr pure with not deref or offset: " ++ (show asnPure)
                                    else
                                        case varLoc of
                                            REFREG_ARGLOC reg m_currSuperOffset baseOffset ->
                                                case m_currSuperOffset of
                                                    Just _ ->
                                                        error . compilerError $ ("Attempted to overwrite non-empty super offset for mem variable=" ++ (displayArgLoc baseVarLoc))
                                                    Nothing ->
                                                        case asnVarLoc of
                                                            REG_ARGLOC _ ->
                                                                [ MOV_X86 asnVarLoc (REFREG_ARGLOC reg m_superOffset baseOffset)
                                                                ]
                                                            REFREG_ARGLOC _ _ _ ->
                                                                [ MOV_X86 (REG_ARGLOC DX) (REFREG_ARGLOC reg m_superOffset baseOffset)
                                                                , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                                                ]
                                            _ ->
                                                error . compilerError $ ("Attempted to get ref on non-stack variable=" ++ (show var) ++ " argLoc=" ++ (displayArgLoc varLoc))
                            else
                                case (asnVarLoc, varLoc) of
                                    (REG_ARGLOC _, REG_ARGLOC p) ->
                                        [ MOV_X86 asnVarLoc (REFREG_ARGLOC p m_superOffset 0)
                                        ]
                                    (REG_ARGLOC _, REFREG_ARGLOC _ _ _) ->
                                        [ MOV_X86 (REG_ARGLOC DX) varLoc
                                        , MOV_X86 asnVarLoc (REFREG_ARGLOC DX m_superOffset 0)
                                        ]
                                    (REFREG_ARGLOC _ _ _, REG_ARGLOC p) ->
                                        [ MOV_X86 (REG_ARGLOC DX) (REFREG_ARGLOC p m_superOffset 0)
                                        , MOV_X86 asnVarLoc (REG_ARGLOC DX)
                                        ]
                                    (REFREG_ARGLOC _ _ _, REFREG_ARGLOC _ _ _) ->
                                        [ MOV_X86 (REG_ARGLOC DX) varLoc
                                        , MOV_X86 (REG_ARGLOC AX) (REFREG_ARGLOC DX m_superOffset 0)
                                        , MOV_X86 asnVarLoc (REG_ARGLOC AX)
                                        ]
                in asnInst ++ offsetInst ++ memopInst

asnImpureIrToX86 :: Coloring -> VariableIr -> ImpureIr -> AllocState -> [X86Instruction]
asnImpureIrToX86 coloring asnVar asnImpure alloc =
    let asnVarLoc = getVarLoc asnVar coloring alloc
    in 
        case asnImpure of
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
                                    CONST_ARGLOC _ ->
                                        [ PUSH_X86 argVarLoc
                                        ]
                                    REG_ARGLOC _ ->
                                        [ PUSH_X86 argVarLoc
                                        ]
                                    REFREG_ARGLOC _ _ _ ->
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
                in callerSavePushInst ++ argPushInst ++ callInst ++ copyRetInst ++ argClearInst ++ callerSavePopInst

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
                                        , IDIV_X86 (REFREG_ARGLOC SP Nothing 0)                 -- divide AX / S[0]
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
                                        , IDIV_X86 (REFREG_ARGLOC SP Nothing 0)                 -- divide AX / S[0]
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
                in binopInst

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
                                -- M[i] < M[j] -> move M[i] to DX
                                (REFREG_ARGLOC _ _ _, REFREG_ARGLOC _ _ _) ->
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
                                -- M[i] <= M[j] -> move M[i] to DX
                                (REFREG_ARGLOC _ _ _, REFREG_ARGLOC _ _ _) ->
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
                                -- M[i] > M[j] -> move M[i] to DX
                                (REFREG_ARGLOC _ _ _, REFREG_ARGLOC _ _ _) ->
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
                                -- M[i] >= M[j] -> move M[i] to DX
                                (REFREG_ARGLOC _ _ _, REFREG_ARGLOC _ _ _) ->
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
                                -- M[i] == M[j] -> move M[i] to DX
                                (REFREG_ARGLOC _ _ _, REFREG_ARGLOC _ _ _) ->
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
                                -- M[i] != M[j] -> move M[i] to DX
                                (REFREG_ARGLOC _ _ _, REFREG_ARGLOC _ _ _) ->
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
                    in splitInst
        
        PURE_MEMOP_IR var memop ->
            let varLoc = getVarLoc var coloring alloc
                (offsetInst, m_superOffset) = 
                    case memopIrOffset memop of
                        Nothing ->
                            ([], Nothing)
                        Just offsetPuB ->
                            case offsetPuB of
                                CONST_IR c -> 
                                    case c of
                                        INT_CONST i ->
                                            ([], Just (Right i))
                                        _ ->
                                            error . compilerError $ ("Attempted use non-int const for memory offset const=" ++ (show offsetPuB))
                                VAR_IR var -> 
                                    let superOffsetVarLoc = getVarLoc var coloring alloc
                                    in 
                                        case superOffsetVarLoc of
                                            REG_ARGLOC reg ->
                                                ([], Just (Left reg))
                                            REFREG_ARGLOC _ _ _ ->
                                                ([ MOV_X86 (REG_ARGLOC AX) superOffsetVarLoc
                                                ], Just (Left AX))
                splitInst =
                    if not . memopIrIsDeref $ memop 
                        then
                            if Maybe.isNothing . memopIrOffset $ memop
                                then
                                    error . compilerError $ "Encountered memopIr pure with not deref or offset: " ++ (show memop)
                                else
                                    case varLoc of
                                        REFREG_ARGLOC reg m_currSuperOffset baseOffset ->
                                            case m_currSuperOffset of
                                                Just _ ->
                                                    error . compilerError $ ("Attempted to overwrite non-empty super offset for mem variable=" ++ (displayArgLoc varLoc))
                                                Nothing ->
                                                    [ MOV_X86 (REG_ARGLOC DX) (REFREG_ARGLOC reg m_superOffset baseOffset)
                                                    , CMP_X86 (REG_ARGLOC DX) (CONST_ARGLOC trueX86)
                                                    ] ++
                                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                                    [ JZ_X86 (bbToLabel fnName splitTrue)
                                                    ] ++
                                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                                    [ JMP_X86 (bbToLabel fnName splitFalse)
                                                    ]
                        else
                            case varLoc of
                                REG_ARGLOC p ->
                                    [ CMP_X86 (REFREG_ARGLOC p m_superOffset 0) (CONST_ARGLOC trueX86)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JZ_X86 (bbToLabel fnName splitTrue)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel fnName splitFalse)
                                    ]
                                REFREG_ARGLOC _ _ _ ->
                                    [ MOV_X86 (REG_ARGLOC DX) varLoc
                                    , CMP_X86 (REFREG_ARGLOC DX m_superOffset 0) (CONST_ARGLOC trueX86)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands1 bbX86) ++
                                    [ JZ_X86 (bbToLabel fnName splitTrue)
                                    ] ++
                                    (basicBlockX86InjectedPhiFnPredCommands2 bbX86) ++
                                    [ JMP_X86 (bbToLabel fnName splitFalse)
                                    ]
            in offsetInst ++ splitInst


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
                                REFREG_ARGLOC reg m_superOffset baseOffset ->
                                    case m_superOffset of
                                        Nothing ->
                                            if baseOffset == 0
                                                then 
                                                    [ MOV_X86 (REG_ARGLOC AX) (REG_ARGLOC reg)
                                                    ] ++
                                                    (fnCalleePostprocessing alloc)
                                                else
                                                    [ MOV_X86 (REG_ARGLOC AX) (REG_ARGLOC reg)
                                                    , ADD_X86 (REG_ARGLOC AX) (CONST_ARGLOC baseOffset)
                                                    ] ++
                                                    (fnCalleePostprocessing alloc)
                                        Just superOffsetBase ->
                                            case superOffsetBase of
                                                Left superOffsetReg ->
                                                    if baseOffset == 0
                                                        then 
                                                            [ MOV_X86 (REG_ARGLOC AX) (REG_ARGLOC reg)
                                                            , ADD_X86 (REG_ARGLOC AX) (REG_ARGLOC superOffsetReg)
                                                            ]
                                                             ++
                                                            (fnCalleePostprocessing alloc)
                                                        else
                                                            [ MOV_X86 (REG_ARGLOC AX) (REG_ARGLOC reg)
                                                            , ADD_X86 (REG_ARGLOC AX) (CONST_ARGLOC baseOffset)
                                                            , ADD_X86 (REG_ARGLOC AX) (REG_ARGLOC superOffsetReg)
                                                            ] ++
                                                            (fnCalleePostprocessing alloc)
                                                Right superOffsetInt ->
                                                    if (superOffsetInt + baseOffset) == 0
                                                        then
                                                            [ MOV_X86 (REG_ARGLOC AX) (REG_ARGLOC reg)
                                                            ] ++
                                                            (fnCalleePostprocessing alloc)
                                                        else
                                                            [ MOV_X86 (REG_ARGLOC AX) (REG_ARGLOC reg)
                                                            , ADD_X86 (REG_ARGLOC AX) (CONST_ARGLOC (superOffsetInt + baseOffset))
                                                            ] ++
                                                            (fnCalleePostprocessing alloc)
                                _ ->
                                    error . compilerError $ ("Attempted to get ref on non-stack variable=" ++ (displayArgLoc pureVarLoc))
            in retInst

        PURE_MEMOP_IR var memop ->
            let varLoc = getVarLoc var coloring alloc
                (offsetInst, m_superOffset) = 
                    case memopIrOffset memop of
                        Nothing ->
                            ([], Nothing)
                        Just offsetPuB ->
                            case offsetPuB of
                                CONST_IR c -> 
                                    case c of
                                        INT_CONST i ->
                                            ([], Just (Right i))
                                        _ ->
                                            error . compilerError $ ("Attempted use non-int const for memory offset const=" ++ (show offsetPuB))
                                VAR_IR var -> 
                                    let superOffsetVarLoc = getVarLoc var coloring alloc
                                    in 
                                        case superOffsetVarLoc of
                                            REG_ARGLOC reg ->
                                                ([], Just (Left reg))
                                            REFREG_ARGLOC _ _ _ ->
                                                ([ MOV_X86 (REG_ARGLOC AX) superOffsetVarLoc
                                                ], Just (Left AX))
                retInst =
                    if not . memopIrIsDeref $ memop 
                        then
                            if Maybe.isNothing . memopIrOffset $ memop
                                then
                                    error . compilerError $ "Encountered memopIr pure with not deref or offset: " ++ (show memop)
                                else
                                    case varLoc of
                                        REFREG_ARGLOC reg m_currSuperOffset baseOffset ->
                                            case m_currSuperOffset of
                                                Just _ ->
                                                    error . compilerError $ ("Attempted to overwrite non-empty super offset for mem variable=" ++ (displayArgLoc varLoc))
                                                Nothing ->
                                                    [ MOV_X86 (REG_ARGLOC AX) (REFREG_ARGLOC reg m_superOffset baseOffset)
                                                    ] ++
                                                    (fnCalleePostprocessing alloc)
                                        _ ->
                                            error . compilerError $ ("Attempted to access offset within non-stack var=" ++ (displayArgLoc varLoc))
                        else
                            case varLoc of
                                REG_ARGLOC p ->
                                    [ MOV_X86 (REG_ARGLOC AX) (REFREG_ARGLOC p m_superOffset 0)
                                    ] ++
                                    (fnCalleePostprocessing alloc)
                                REFREG_ARGLOC _ _ _ ->
                                    [ MOV_X86 (REG_ARGLOC DX) varLoc
                                    , MOV_X86 (REG_ARGLOC AX) (REFREG_ARGLOC DX m_superOffset 0)
                                    ] ++
                                    (fnCalleePostprocessing alloc)
            in offsetInst ++ retInst

retNoneToX86 :: AllocState -> [X86Instruction]
retNoneToX86 alloc = 
    fnCalleePostprocessing alloc

-- HELPERS

fnCalleePreprocessing :: StructContext -> Coloring -> FunctionIr -> (AllocState, [X86Instruction])
fnCalleePreprocessing structCtx coloring fnIr = 
    let 
        -- initialize function spillover at SP + 0 and w/ all available registers
        -- for each color allocate a register if the color has not been alloc'd yet
        alloc = 
            foldr
                (\(var, color) interAlloc ->
                    case Map.lookup color (allocStateRegMap interAlloc) of
                        Just argLoc -> interAlloc
                        Nothing -> 
                            let isStackColor = Set.member var (coloringStackVars coloring)
                                varSize = (sizeofType structCtx) . variableIrType $ var
                            in allocColor color varSize isStackColor interAlloc
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
                                REG_ARGLOC _ ->
                                    [ MOV_X86 asnVarLoc (REFREG_ARGLOC BP Nothing interArgSPOffset)
                                    ]
                                REFREG_ARGLOC _ _ _ ->
                                    [ MOV_X86 (REG_ARGLOC DX) (REFREG_ARGLOC BP Nothing interArgSPOffset)
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
allocColor :: Int -> Int -> Bool -> AllocState -> AllocState 
allocColor color varSize isStackColor initAlloc =
    if isStackColor || (null . allocStateAvailableReg $ initAlloc)
        then
            let argLoc = REFREG_ARGLOC SP Nothing (allocStateStackCtr initAlloc)
                newAlloc =
                    AllocState
                        (Map.insert color argLoc (allocStateRegMap initAlloc))
                        ((allocStateStackCtr initAlloc) + varSize)
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
