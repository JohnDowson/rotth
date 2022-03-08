section .data
    buffer  times 32 db 0

section .text
    global print

print:
    call ft_itoa
    mov rdi, buffer
    call cstrlen

    mov rdi, 1                   ; fd
    mov rsi, buffer              ; buffer
    xor rdx, rdx
    mov rdx, rax                 ; count
    mov rax, 1                   ; write(2)
    syscall

    ret

;======================================================

; https://tuttlem.github.io/2013/01/08/strlen-implementation-in-nasm.html

cstrlen:
    push  rbx                 ; save any registers that
    push  rcx                 ; we will trash in here

    mov   rbx, rdi            ; rbx = rdi

    xor   al, al              ; the byte that the scan will
                                ; compare to is zero

    mov   rcx, 0xffffffff     ; the maximum number of bytes

    repne scasb               ; while [rdi] != al, keep scanning

    sub   rdi, rbx            ; length = dist2 - dist1
    mov   rax, rdi            ; rax now holds our length

    pop   rcx                 ; restore the saved registers
    pop   rbx

    ret                       ; all done!

;======================================================

; https://stackoverflow.com/a/54381686/12015230

ft_itoa:
    xor     rcx, rcx                    ;initialize counter
    xor     r9, r9                      ;set neg flag to 0
    mov     eax, edi                    ;move number in RAX for DIV instruction
    push    rbx                         ;save RBX
    mov     ebx, 10

.check_negative:
    and     edi, 0x80000000
    mov     rdi, buffer
    jz      .divide                     ;number is positive, proceed to main loop
    not     eax                         ;else
    inc     eax                         ;compute absolute value with binary complement
    inc     r9                          ;set neg flag

.divide:
    xor     edx, edx
    div     ebx
    add     edx, 48                     ;convert int to char
    push    rdx
    inc     rcx
    cmp     eax, 0
    jnz     .divide

.check_neg_flag:
    cmp     r9, 1
    jne     .buff_string
    mov     byte[rdi], '-'

.buff_string:
    pop     rdx
    mov     byte[rdi + r9], dl
    dec     rcx
    inc     r9
    cmp     rcx, 0
    jnz     .buff_string
    mov     byte[rdi + r9], 10
    mov     byte[rdi + r9 + 1], 0
    pop     rbx                         ;restore RBX
    ret