use crate::lir::Op;
use indoc::indoc;
use somok::Somok;
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
            format ELF64 executable 3
            segment readable executable

            entry start
            start:
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
                Pop => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        pop rax

                    "},
                    op
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

                Return => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        mov rax, rsp
                        mov rsp, [ret_stack_rsp]
                        ret

                    "},
                    op
                )?,
                Call(p) => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        ; mov rax, rsp
                        call {}

                    "},
                    op, p
                )?,
                Exit => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        pop rax
                        pop rdi
                        syscall
                        push rax

                    "},
                    op
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
        ().okay()
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}
