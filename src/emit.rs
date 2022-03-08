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
            extern print

            _start:
                mov QWORD [ret_stack_rsp], ret_stack
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
                Drop => write!(
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
                        call print
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
                Divmod => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        xor rdx, rdx
                        pop rbx
                        pop rax
                        div rbx
                        push rax
                        push rdx
                    "},
                    op
                )?,
                Mul => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        pop rax
                        pop rbx
                        mul rbx
                        push rax
                    "},
                    op
                )?,

                Ne => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        mov rcx, 0
                        mov rdx, 1
                        pop rbx
                        pop rax
                        cmp rax, rbx
                        cmovne rcx, rdx
                        push rcx
                    "},
                    op
                )?,
                Lt => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        mov rcx, 0
                        mov rdx, 1
                        pop rbx
                        pop rax
                        cmp rax, rbx
                        cmovl rcx, rdx
                        push rcx
                    "},
                    op
                )?,
                Ge => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        mov rcx, 0
                        mov rdx, 1
                        pop rbx
                        pop rax
                        cmp rax, rbx
                        cmovge rcx, rdx
                        push rcx
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
                Eq => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        mov rcx, 0
                        mov rdx, 1
                        pop rbx
                        pop rax
                        cmp rax, rbx
                        cmove rcx, rdx
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
                        mov rax, 8
                        add [ret_stack_rsp], rax
                        mov QWORD rax, [ret_stack_rsp]
                        mov QWORD [rax], rdi
                    "},
                    l
                )?,
                ProcEnd => write!(
                    sink,
                    indoc! {"
                    ; load return adderss
                        mov QWORD rax, [ret_stack_rsp]
                        mov QWORD rdi, [rax]
                        mov rax, 8
                        sub [ret_stack_rsp], rax
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
                JumpT(_) => todo!(), // op => todo!("{:?}", op),
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
