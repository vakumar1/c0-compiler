section .text

global _start

_start:
    call    main
    mov     rbx, rax
    mov     rax, 1
    int     0x80
