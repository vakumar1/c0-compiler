module Model.X86 (
    Label,
    X86Instruction (..),
    ArgLocation (..),
    Register (..),
    availableRegisters,
    registerSize,
    trueX86,
    falseX86
)
where

import qualified Text.Printf as Printf

data X86Instruction
    = LABEL_X86 Label
    | MOV_X86 ArgLocation ArgLocation
    | ADD_X86 ArgLocation ArgLocation
    | SUB_X86 ArgLocation ArgLocation
    | IMUL_X86 ArgLocation ArgLocation
    | IDIV_X86 ArgLocation
    | XOR_X86 ArgLocation ArgLocation
    | AND_X86 ArgLocation ArgLocation
    | OR_X86 ArgLocation ArgLocation
    | SAL_X86 ArgLocation ArgLocation
    | SAR_X86 ArgLocation ArgLocation
    | SHL_X86 ArgLocation ArgLocation
    | SHR_X86 ArgLocation ArgLocation
    | NEG_X86 ArgLocation
    | NOT_X86 ArgLocation
    | PUSH_X86 ArgLocation
    | JMP_X86 Label
    | CMP_X86 ArgLocation ArgLocation
    | JZ_X86 Label
    | JL_X86 Label
    | JLE_X86 Label
    | JG_X86 Label
    | JGE_X86 Label
    | JE_X86 Label
    | JNE_X86 Label 
    | RET_X86
instance Show X86Instruction where
    show instr =
        let prefix =
                case instr of
                    LABEL_X86 _ -> "\n"
                    _ -> "    "
            suffix = "\n"
            instrStr =
                case instr of
                    LABEL_X86 l ->
                        Printf.printf "%s:" l
                    MOV_X86 r1 r2 ->
                        Printf.printf "mov %s, %s" (displayArgLoc r1) (displayArgLoc r2)
                    ADD_X86 r1 r2 ->
                        Printf.printf "add %s, %s" (displayArgLoc r1) (displayArgLoc r2)
                    SUB_X86 r1 r2 ->
                        Printf.printf "sub %s, %s" (displayArgLoc r1) (displayArgLoc r2)
                    IMUL_X86 r1 r2 ->
                        Printf.printf "imul %s, %s" (displayArgLoc r1) (displayArgLoc r2)
                    IDIV_X86 r ->
                        Printf.printf "idiv %s" (displayArgLoc r)
                    XOR_X86 r1 r2 ->
                        Printf.printf "xor %s, %s" (displayArgLoc r1) (displayArgLoc r2)
                    AND_X86 r1 r2 ->
                        Printf.printf "and %s, %s" (displayArgLoc r1) (displayArgLoc r2)
                    OR_X86 r1 r2 ->
                        Printf.printf "or %s, %s" (displayArgLoc r1) (displayArgLoc r2)
                    SAL_X86 r1 r2 ->
                        Printf.printf "sal %s, %s" (displayArgLoc r1) (displayArgLoc r2)
                    SAR_X86 r1 r2 ->
                        Printf.printf "sar %s, %s" (displayArgLoc r1) (displayArgLoc r2)
                    SHL_X86 r1 r2 ->
                        Printf.printf "shl %s, %s" (displayArgLoc r1) (displayArgLoc r2)
                    SHR_X86 r1 r2 ->
                        Printf.printf "shr %s, %s" (displayArgLoc r1) (displayArgLoc r2)
                    NEG_X86 r ->
                        Printf.printf "neg %s" (displayArgLoc r)
                    NOT_X86 r ->
                        Printf.printf "not %s" (displayArgLoc r)
                    PUSH_X86 r ->
                        Printf.printf "push %s" (displayArgLoc r)
                    JMP_X86 l ->
                        Printf.printf "jmp %s" l
                    CMP_X86 r1 r2 ->
                        Printf.printf "cmp %s, %s" (displayArgLoc r1) (displayArgLoc r2)
                    JZ_X86 l ->
                        Printf.printf "jz %s" l
                    JL_X86 l ->
                        Printf.printf "jl %s" l
                    JLE_X86 l ->
                        Printf.printf "jle %s" l
                    JG_X86 l ->
                        Printf.printf "jg %s" l
                    JGE_X86 l ->
                        Printf.printf "jge %s" l
                    JE_X86 l ->
                        Printf.printf "je %s" l
                    JNE_X86 l ->
                        Printf.printf "jne %s" l
                    RET_X86 ->
                        "ret"
         in prefix ++ instrStr ++ suffix

type Label = String

data ArgLocation
    = REG_ARGLOC Register
    | STACK_ARGLOC Int
    | CONST_ARGLOC Int
instance Eq ArgLocation where
    argLoc1 == argLoc2 = 
        case (argLoc1, argLoc2) of
            (REG_ARGLOC r1, REG_ARGLOC r2) -> r1 == r2
            (STACK_ARGLOC sp1, STACK_ARGLOC sp2) -> sp1 == sp2
            (CONST_ARGLOC c1, CONST_ARGLOC c2) -> c1 == c2
            _ -> False

displayArgLoc :: ArgLocation -> String
displayArgLoc argLoc =
    case argLoc of
        REG_ARGLOC reg -> show reg
        STACK_ARGLOC stackPtr ->
            if (stackPtr >= 0)
                then Printf.printf "QWORD [%s + %s]" (show SP) (show stackPtr)
                else Printf.printf "QWORD [%s - %s]" (show SP) (show (-stackPtr))
        CONST_ARGLOC const -> show const

data Register
    = AX
    | BX
    | CX
    | DX
    | SI
    | DI
    | SP
    | BP
    deriving (Eq)
instance Show Register where
    show reg =
        case reg of
            AX -> "rax"
            BX -> "rbx"
            CX -> "rcx"
            DX -> "rdx"
            SI -> "rsi"
            DI -> "rdi"
            SP -> "rsp"
            BP -> "rbp"

-- returns registers initially available for arguments
availableRegisters :: [Register]
availableRegisters = [BX, CX, SI, DI]

registerSize :: Int
registerSize = 8

-- constant x86 values
trueX86 :: Int
trueX86 = 1

falseX86 :: Int
falseX86 = 0
