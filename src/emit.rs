use crate::{hir::IConst, lir::Op};
use indoc::indoc;
use somok::Somok;
use std::{
    collections::HashMap,
    io::{BufWriter, Write},
};

pub struct Compiler {}

impl Compiler {
    pub fn new() -> Self {
        Self {}
    }

    pub fn compile<S: Write>(
        self,
        ops: Vec<Op>,
        strings: &[String],
        mems: &HashMap<String, usize>,
        mut sink: BufWriter<S>,
    ) -> std::io::Result<()> {
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
                ; set up args
                pop rax
                mov [argc], rax
                mov [argv], rsp

        "},
        )?;
        for op in ops {
            match &op {
                PushMem(nm) => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        push mem_{}
                    "},
                    op, nm
                )?,
                PushStr(i) => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                    ;   mov rax, len
                        push {}
                        push str_{}
                    "},
                    op,
                    strings[*i].len(),
                    i
                )?,
                Push(c) => match c {
                    IConst::Bool(b) => write!(
                        sink,
                        indoc! {"
                        ; {:?}
                            mov rax, {}
                            push rax
                        "},
                        op, *b as u64
                    )?,
                    IConst::Char(c) => write!(
                        sink,
                        indoc! {"
                        ; {:?}
                            mov rax, {}
                            push rax
                        "},
                        op, *c as u64
                    )?,
                    IConst::U64(u) => write!(
                        sink,
                        indoc! {"
                        ; {:?}
                            mov rax, {}
                            push rax
                        "},
                        op, u
                    )?,
                    IConst::I64(i) => write!(
                        sink,
                        indoc! {"
                        ; {:?}
                            mov rax, {}
                            push rax
                        "},
                        op, i
                    )?,
                    IConst::Ptr(p) => write!(
                        sink,
                        indoc! {"
                        ; {:?}
                            mov rax, {}
                            push rax
                        "},
                        op, p
                    )?,
                    IConst::Str(_s) => unreachable!(),
                },
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

                Bind => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        pop rbx
                        mov rax, 8
                        add [ret_stack_rsp], rax
                        mov QWORD rax, [ret_stack_rsp]
                        mov QWORD [rax], rbx
                    "},
                    op
                )?,
                UseBinding(offset) => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        mov rax, 8 * {}
                        mov QWORD rbx, [ret_stack_rsp]
                        sub rbx, rax
                        mov QWORD rax, [rbx]
                        push rax
                    "},
                    op, offset
                )?,
                Unbind => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        mov rax, 8
                        sub [ret_stack_rsp], rax
                    "},
                    op
                )?,

                ReadU64 => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        pop rax
                        mov rbx, [rax]
                        push rbx
                    "},
                    op
                )?,
                ReadU8 => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        pop rax
                        xor rbx, rbx
                        mov bl, [rax]
                        push rbx
                    "},
                    op
                )?,
                WriteU64 => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        pop rax
                        pop rbx
                        mov [rax], rbx
                    "},
                    op
                )?,
                WriteU8 => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        pop rax
                        pop rbx
                        mov [rax], bl
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

                Syscall0 => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        pop rax
                        syscall
                        push rax
                    "},
                    op
                )?,
                Syscall1 => write!(
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
                Syscall2 => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        pop rax
                        pop rdi
                        pop rsi
                        syscall
                        push rax
                    "},
                    op
                )?,
                Syscall3 => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        pop rax
                        pop rdi
                        pop rsi
                        pop rdx
                        syscall
                        push rax
                    "},
                    op
                )?,
                Syscall4 => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        pop rax
                        pop rdi
                        pop rsi
                        pop rdx
                        pop r10
                        syscall
                        push rax
                    "},
                    op
                )?,
                Syscall5 => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        pop rax
                        pop rdi
                        pop rsi
                        pop rdx
                        pop r10
                        pop r8
                        syscall
                        push rax
                    "},
                    op
                )?,
                Syscall6 => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        pop rax
                        pop rdi
                        pop rsi
                        pop rdx
                        pop r10
                        pop r8
                        pop r9
                        syscall
                        push rax
                    "},
                    op
                )?,

                Argc => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                        mov rax, [argc]
                        push rax
                    "},
                    op
                )?,
                Argv => write!(
                    sink,
                    indoc! {"
                    ; {:?}
                    mov rax, [argv]
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
                    ; load return adderss
                        mov QWORD rax, [ret_stack_rsp]
                        mov QWORD rdi, [rax]
                        mov rax, 8
                        sub [ret_stack_rsp], rax
                        push rdi
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
                JumpT(_) => todo!("Jump if true"),
            }
        }
        write!(
            sink,
            indoc! {"
            section .data
        "}
        )?;
        for (i, str) in strings.iter().enumerate() {
            write!(
                sink,
                indoc! {"
                str_{}:
                    db {}
                "},
                i,
                {
                    str.bytes()
                        .map(|b| b.to_string())
                        .intersperse(",".to_string())
                        .collect::<String>()
                }
            )?;
        }
        write!(
            sink,
            indoc! {"
            section .bss
                ret_stack_rsp: resq 1
                ret_stack: resb 65536
                ret_stack_end:
                argc: resq 1
                argv: resq 1
        "},
        )?;
        for (name, size) in mems {
            write!(
                sink,
                indoc! {"
            mem_{}:
                resb {}
        "},
                name, size
            )?;
        }
        ().okay()
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}
