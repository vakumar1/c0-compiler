
section .data
    debug_msg db "Result: ", 0    ; Null-terminated string for "Result: "
    pid dq 0
    tid dq 0

section .text
    global _start

_start:
    ; Call main and store retval in rax
    call    main

    ; Convert the integer to a string
    jmp     int_to_str              ; Convert rax = integer to string and set rbx = length

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
    dec     rsp
    mov     dl, 0x0A
    mov     [rsp], dl               ; Decr sp and add newline

pre_check_neg:
    mov     r8, 0                   ; record rax is positive
    test    rax, rax
    jns     convert_loop
    mov     r8, 1                   ; record rax is negative
    neg     rax                     ; negative and add negative sign if negative

convert_loop:
    inc     rbx
    dec     rsp                     ; Move buffer pointer backwards
    xor     rdx, rdx                ; Clear any previous remainder
    div     rcx                     ; Divide rax by 10, result in rax, remainder in rdx
    add     rdx, '0'                ; Convert remainder to ASCII
    mov     [rsp], dl               ; Store ASCII character in the buffer

    test    rax, rax
    jz      post_process_neg        ; If quotient is zero return

    cmp     rbx, 10
    je      post_process_neg        ; If loop is over (10) return
    jmp     convert_loop

post_process_neg:
    test    r8, r8
    jz      finish
    dec     rsp
    mov     dl, '-'
    mov     [rsp], dl               ; if negative decr sp and add '-'
    jmp     finish      