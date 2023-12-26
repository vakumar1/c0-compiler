# c0-compiler

Compilation commands for compiling `test.c`:

```
stack run test.c > test.asm
nasm -f elf64 test.asm -o test.o
ld test.o -o test -m elf_x86_64
./test
```

