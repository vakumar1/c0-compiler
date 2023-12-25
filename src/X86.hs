module X86 (
    Label,
    Instruction,
    ArgLocation,
    Register,
    availableRegisters,
)

where

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

type Label = String

data ArgLocation = ArgLocation
    = REG_ARGLOC Register
    | STACK_ARGLOC Int
    | CONST_ARGLOC Int

data Register
    = EAX
    | EBX
    | ECX
    | EDX
    | ESI
    | EDI
    | ESP
    | EBP

-- returns registers initially available for arguments
availableRegisters :: [Register]
availableRegisters = [EBX, ECX, ESI, EDI]
