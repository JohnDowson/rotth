use crate::lir::Op;
use indoc::indoc;
use std::io::{BufWriter, Write};

pub struct Compiler {}

impl Compiler {
    pub fn new() -> Self {
        Self {}
    }

    pub fn compile<S: Write>(self, ops: Vec<Op>, mut sink: BufWriter<S>) -> std::io::Result<()> {
        use Op::*;
        write!(
            sink,
            indoc! {"
            BITS 64
            section .text
            global _start

            __print__:
                mov     r9, -368934881474191032
                sub     rsp, 40
                mov     BYTE [rsp+31], 10
                lea     rcx, [rsp+30]
            .L2:
                mov     rax, rdi
                lea     r8, [rsp+32]
                mul     r9
                mov     rax, rdi
                sub     r8, rcx
                shr     rdx, 3
                lea     rsi, [rdx+rdx*4]
                add     rsi, rsi
                sub     rax, rsi
                add     eax, 48
                mov     BYTE [rcx], al
                mov     rax, rdi
                mov     rdi, rdx
                mov     rdx, rcx
                sub     rcx, 1
                cmp     rax, 9
                ja      .L2
                lea     rax, [rsp+32]
                mov     edi, 1
                sub     rdx, rax
                xor     eax, eax
                lea     rsi, [rsp+32+rdx]
                mov     rdx, r8
                mov     rax, 1
                syscall
                add     rsp, 40
                ret

            _start:
        "},
        )?;
        for op in ops {
            match &op {
                Push(c) => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        mov rax, {:?}
                        push rax

                    "},
                    op,
                    c.bytes()
                )?,
                Dup => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        pop rax
                        push rax
                        push rax

                    "},
                    op
                )?,
                Swap => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        pop rax
                        pop rbx
                        push rax
                        push rbx

                    "},
                    op
                )?,
                Over => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        pop rax
                        pop rbx
                        push rbx
                        push rax
                        push rbx

                    "},
                    op
                )?,
                Pop => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        pop rax

                    "},
                    op
                )?,

                Print => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        pop rdi
                        call __print__

                    "},
                    op
                )?,

                Sub => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        pop rax
                        pop rbx
                        sub rbx, rax
                        push rbx

                    "},
                    op
                )?,
                Add => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        pop rax
                        pop rbx
                        add rbx, rax
                        push rbx

                    "},
                    op
                )?,

                Le => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        mov rcx, 0
                        mov rdx, 1
                        pop rbx
                        pop rax
                        cmp rax, rbx
                        cmovle rcx, rdx
                        push rcx

                    "},
                    op
                )?,
                Gt => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        mov rcx, 0
                        mov rdx, 1
                        pop rbx
                        pop rax
                        cmp rax, rbx
                        cmovg rcx, rdx
                        push rcx

                    "},
                    op
                )?,

                Return => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        ret

                    "},
                    op
                )?,
                Call(p) => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        call {}
                    "},
                    op, p
                )?,
                Exit => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        pop rdi
                        mov rax, 60
                        syscall

                    "},
                    op
                )?,
                Proc(l) => write!(
                    sink,
                    indoc! {"
                    {}:
                    ; save return address
                        pop rdi
                        inc QWORD [ret_stack_rsp]
                        mov QWORD [ret_stack_rsp], rdi

                    "},
                    l
                )?,
                ProcEnd => write!(
                    sink,
                    indoc! {"
                    ; load return adderss
                        mov QWORD rdi, [ret_stack_rsp]
                        dec QWORD [ret_stack_rsp]
                        push rdi
                    "},
                )?,
                Label(l) => write!(
                    sink,
                    indoc! {"
                    {}:
                    "},
                    l
                )?,
                JumpF(l) => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        pop rax
                        test rax, rax
                        syscall
                        jz {}

                    "},
                    op, l
                )?,
                Jump(l) => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        jmp {}

                    "},
                    op, l
                )?,
                Dump => {}
                op => todo!("{:?}", op),
            }
        }
        write!(
            sink,
            indoc! {"
            section .bss
                ret_stack_rsp: resq 1
                ret_stack: resb 65536
                ret_stack_end:
        "},
        )
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}
