use crate::{hir::IConst, lir::Op};
use somok::{Either, Somok};
use std::collections::HashMap;

pub fn eval(ops: Vec<Op>, strings: &[String]) -> Result<Either<u64, Vec<u64>>, String> {
    let labels = ops
        .iter()
        .enumerate()
        .filter_map(|(i, op)| {
            if let Op::Label(l) | Op::Proc(l) = op {
                (l.clone(), i).some()
            } else {
                None
            }
        })
        .collect::<HashMap<String, usize>>();

    let mut call_stack = Vec::new();
    let mut stack = Vec::new();
    let mut i = 0;

    while let Some(op) = ops.get(i) {
        #[cfg(debug_assertions)]
        println!("{}:\t{:?}", i, op);
        match op {
            Op::PushMem(_i) => {
                todo!("Support memories in eval")
            }
            Op::PushStr(i) => {
                let len = strings[*i].len() as u64;
                stack.push(len);
                stack.push(strings[*i].as_ptr() as u64);
            }
            Op::Push(c) => match c {
                IConst::Bool(b) => stack.push(*b as u64),
                IConst::U64(u) => stack.push(*u),
                IConst::I64(i) => stack.push(*i as u64),
                IConst::Ptr(p) => stack.push(*p),
                IConst::Char(c) => stack.push(*c as u64),
                IConst::Str(_s) => unreachable!(),
            },
            Op::Drop => {
                stack.pop();
            }
            Op::Dup => {
                let v = stack.last().copied().unwrap();
                stack.push(v);
            }
            Op::Swap => {
                let (a, b) = (stack.pop().unwrap(), stack.pop().unwrap());
                stack.push(a);
                stack.push(b);
            }
            Op::Over => {
                let v = stack[stack.len() - 2];
                stack.push(v);
            }

            Op::Bind => call_stack.push(stack.pop().unwrap()),
            Op::UseBinding(offset) => stack.push(call_stack[(call_stack.len() - 1) - offset]),
            Op::Unbind => {
                call_stack.pop();
            }

            Op::ReadU8 => {
                let ptr = stack.pop().unwrap();
                let read = unsafe { (ptr as *const u8).read() as u64 };
                stack.push(read);
            }
            Op::WriteU8 => {
                let byte = stack.pop().unwrap() as u8;
                let ptr = stack.pop().unwrap();
                unsafe { (ptr as *mut u8).write(byte) };
            }

            Op::Dump => println!("{:?}", stack),
            Op::Print => println!("{:?}", stack.pop().unwrap()),
            Op::Syscall0
            | Op::Syscall1
            | Op::Syscall2
            | Op::Syscall3
            | Op::Syscall4
            | Op::Syscall5
            | Op::Syscall6 => todo!("Syscalls not supported in eval"),

            Op::Add => {
                let (b, a) = (stack.pop().unwrap(), stack.pop().unwrap());
                stack.push(a + b);
            }
            Op::Sub => {
                let (b, a) = (stack.pop().unwrap(), stack.pop().unwrap());
                stack.push(a - b);
            }
            Op::Divmod => {
                let (b, a) = (stack.pop().unwrap(), stack.pop().unwrap());
                stack.push(a / b);
                stack.push(a % b);
            }
            Op::Mul => {
                let (b, a) = (stack.pop().unwrap(), stack.pop().unwrap());
                stack.push(a * b);
            }

            Op::Eq => {
                let (b, a) = (stack.pop().unwrap(), stack.pop().unwrap());
                stack.push((a == b) as u64);
            }
            Op::Ne => {
                let (b, a) = (stack.pop().unwrap(), stack.pop().unwrap());
                stack.push((a != b) as u64);
            }
            Op::Lt => {
                let (b, a) = (stack.pop().unwrap(), stack.pop().unwrap());
                stack.push((a < b) as u64);
            }
            Op::Le => {
                let (b, a) = (stack.pop().unwrap(), stack.pop().unwrap());
                stack.push((a <= b) as u64);
            }
            Op::Gt => {
                let (b, a) = (stack.pop().unwrap(), stack.pop().unwrap());
                stack.push((a > b) as u64);
            }
            Op::Ge => {
                let (b, a) = (stack.pop().unwrap(), stack.pop().unwrap());
                stack.push((a >= b) as u64);
            }

            Op::Proc(_) => (),
            Op::Label(_) => (),
            Op::Jump(l) => i = labels[l],
            Op::JumpF(l) => {
                if stack.pop() == Some(0) {
                    i = labels[l]
                }
            }
            Op::JumpT(l) => {
                if stack.pop() == Some(1) {
                    i = labels[l]
                }
            }
            Op::Call(l) => {
                call_stack.push(i as u64);
                i = labels.get(l).copied().ok_or_else(|| l.clone())?
            }
            Op::Return => i = call_stack.pop().unwrap() as usize,
            Op::Exit => return stack.pop().unwrap().left().okay(),
        }
        i += 1;
    }
    stack.right().okay()
}
