## C0 Compiler

[C0](https://c0.cs.cmu.edu/docs/c0-reference.pdf) -> x86 compiler

### Usage
```
stack run [<C0 file>] (debug)
```

### Sample commands for compiling and running `test.c`:
```
stack run test.c > test.asm
nasm -f elf64 test.asm -o test.o
ld test.o -o tst -m elf_x86_64
./tst
```

### Notes on Currently Implemented Optimization Passes

- [x] [C0 Compiler Optimizations Part 1: SSA,
-Functions, Register Allocation](https://hackmd.io/@n9vXJ2dWSK-txnWnjmMPGQ/S1ewdXJggx)
- [x] [C0 Compiler Optimizations Part 2: Using the State Monad for a Peephole Optimization](https://hackmd.io/@n9vXJ2dWSK-txnWnjmMPGQ/BkqIumyleg)
