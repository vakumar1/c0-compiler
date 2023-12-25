module X86 (
    Label,
    X86Instruction (..),
    ArgLocation (..),
    Register (..),
    availableRegisters,
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
                        Printf.printf "%s:" (show l)
                    MOV_X86 r1 r2 -> 
                        Printf.printf "mov %s, %s" (show r1) (show r2)
                    ADD_X86 r1 r2 ->
                        Printf.printf "add %s, %s" (show r1) (show r2)
                    SUB_X86 r1 r2 ->
                        Printf.printf "sub %s, %s" (show r1) (show r2)
                    IMUL_X86 r1 r2 ->
                        Printf.printf "imul %s, %s" (show r1) (show r2)
                    IDIV_X86 r ->
                        Printf.printf "div %s" (show r)
                    XOR_X86 r1 r2 ->
                        Printf.printf "xor %s, %s" (show r1) (show r2)
                    NEG_X86 r ->
                        Printf.printf "neg %s" (show r)
                    PUSH_X86 r ->
                        Printf.printf "push %s" (show r)
                    JMP_X86 l ->
                        Printf.printf "jmp %s" (show l)
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
            STACK_ARGLOC stackPtr -> (show (-stackPtr)) ++ "(" ++ (show ESP) ++ ")"
            CONST_ARGLOC const -> "$" ++ (show const)

data Register
    = EAX
    | EBX
    | ECX
    | EDX
    | ESI
    | EDI
    | ESP
    | EBP
instance Show Register where
    show reg = 
        case reg of
            EAX -> "%eax"
            EBX -> "%ebx"
            ECX -> "%ecx"
            EDX -> "%edx"
            ESI -> "%esi"
            EDI -> "%edi"
            ESP -> "%esp"
            EBP -> "%ebp"

-- returns registers initially available for arguments
availableRegisters :: [Register]
availableRegisters = [EBX, ECX, ESI, EDI]
