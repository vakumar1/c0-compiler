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
    | NEG_X86 ArgLocation
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
                        Printf.printf "mov %s, %s" (show r1) (show r2)
                    ADD_X86 r1 r2 ->
                        Printf.printf "add %s, %s" (show r1) (show r2)
                    SUB_X86 r1 r2 ->
                        Printf.printf "sub %s, %s" (show r1) (show r2)
                    IMUL_X86 r1 r2 ->
                        Printf.printf "imul %s, %s" (show r1) (show r2)
                    IDIV_X86 r ->
                        Printf.printf "idiv dword %s" (show r)
                    XOR_X86 r1 r2 ->
                        Printf.printf "xor %s, %s" (show r1) (show r2)
                    NEG_X86 r ->
                        Printf.printf "neg %s" (show r)
                    PUSH_X86 r ->
                        Printf.printf "push %s" (show r)
                    JMP_X86 l ->
                        Printf.printf "jmp %s" l
                    CMP_X86 r1 r2 ->
                        Printf.printf "cmp %s %s" (show r1) (show r2)
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
instance Show ArgLocation where
    show argLoc =
        case argLoc of
            REG_ARGLOC reg -> show reg
            STACK_ARGLOC stackPtr ->
                if (stackPtr >= 0)
                    then Printf.printf "[%s + %s]" (show SP) (show stackPtr)
                    else Printf.printf "[%s - %s]" (show SP) (show (-stackPtr))
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
