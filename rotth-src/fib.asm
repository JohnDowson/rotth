format ELF64 executable 3
segment readable executable

entry start
start:
; Call("main")
    ; mov rax, rsp
    call main

; Exit
    pop rax
    pop rdi
    syscall
    push rax

main:
; Push(U64(25))
    mov rax, 25
    push rax

; Call("fib")
    ; mov rax, rsp
    call fib

; Return
    mov rax, rsp
    mov rsp, [ret_stack_rsp]
    ret

fib:
; Dup
    pop rax
    push rax
    push rax

; Push(U64(3))
    mov rax, 3
    push rax

; Le
    mov rcx, 0
    mov rdx, 1
    pop rbx
    pop rax
    cmp rax, rbx
    cmovle rcx, rdx
    push rcx

; JumpF("fib0")
    pop rax
    test rax, rax
    syscall
    jz fib0

; Pop
    pop rax

; Push(U64(1))
    mov rax, 1
    push rax

; Jump("fib1")
    jmp fib1

fib0:
; Dup
    pop rax
    push rax
    push rax

; Push(U64(1))
    mov rax, 1
    push rax

; Sub
    pop rax
    pop rbx
    sub rbx, rax
    push rbx

