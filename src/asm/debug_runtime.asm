
section .data
    debug_msg db "Result: ", 0    ; Null-terminated string for "Result: "

section .text
    global _start

_start:
    ; Call main and store retval in rax
    call    main

    ; Convert the integer to a string
    jmp     int_to_str              ; Convert rax = integer to string and set rbx = length

finish:
    ; Print "Result: " string
    mov     rax, 1                  ; System call number for write
    mov     rdi, 1                  ; File descriptor 1: standard output
    mov     rsi, debug_msg          ; Pointer to the string
    mov     rdx, 8                  ; Length of the string
    syscall

    ; Print the integer value as a string
    mov     rax, 1                  ; System call number for write
    mov     rdi, 1                  ; File descriptor 1: standard output
    mov     rsi, rsp                ; Pointer to the converted string
    mov     rdx, rbx                ; Length of the converted string
    syscall
    inc     rbx
    add     rsp, rbx                ; delete integer string from stack

    mov     rbx, 0                  ; Exit with code 0
    mov     rax, 1
    int     0x80


int_to_str:
    ; Convert integer to string on stack, expects rax = integer
    mov     rcx, 10                 ; divisor = 10 (decimal representation)
    mov     rbx, 1                  ; Store the curr length of the string (stop at 10)
    dec     rsp
    mov     dl, 0
    mov     [rsp], dl               ; Decr sp and add null terminator
    mov     dl, 0x0A
    mov     [rsp], dl                ; Decr sp and add newline

convert_loop:
    inc     rbx
    dec     rsp                     ; Move buffer pointer backwards
    xor     rdx, rdx                ; Clear any previous remainder
    div     rcx                     ; Divide rax by 10, result in rax, remainder in rdx
    add     rdx, '0'                ; Convert remainder to ASCII
    mov     [rsp], dl               ; Store ASCII character in the buffer

    test    rax, rax
    jz      finish                  ; If quotient is zero return

    cmp     rbx, 10
    je      finish                  ; If loop is over (10) return
    jmp     convert_loop
