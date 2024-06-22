section .text

global _start

_start:
    call    main
    mov     rbx, rax
    mov     rax, 1
    int     0x80

abort:
    ; Get the PID
    mov eax, 39
    syscall             ; call kernel to get PID
    mov [pid], rax      ; store the PID
    mov rdi, [pid]
    mov [tid], rdi      ; store the TID

    ; Send the SIGABRT signal using tgkill
    mov eax, 234
    mov edi, [pid]
    mov esi, [tid]
    mov edx, 6
    syscall             ; call kernel to raise signal
