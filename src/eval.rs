use std::collections::HashMap;

use somok::Somok;

use crate::{hir::IConst, lir::Op};

pub fn eval(ops: Vec<Op>, strings: &[String]) -> Result<u64, String> {
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
    let rawstrs: Vec<(usize, *mut u8)> = strings
        .iter()
        .map(|s| {
            let (ptr, _, _) = s.clone().into_raw_parts();
            let len = s.len();
            (len, ptr)
        })
        .collect();

    let mut call_stack = Vec::new();
    let mut stack = Vec::new();
    let mut i = 0;

    while let Some(op) = ops.get(i) {
        #[cfg(debug_assertions)]
        println!("{}:\t{:?}", i, op);
        match op {
            Op::PushStr(i) => {
                let len = rawstrs[*i].0 as u64;
                stack.push(len);
                stack.push(rawstrs[*i].1 as u64);
            }
            Op::Push(c) => match c {
                IConst::Bool(b) => stack.push(*b as u64),
                IConst::U64(u) => stack.push(*u),
                IConst::I64(i) => stack.push(*i as u64),
                IConst::Ptr(p) => stack.push(*p),
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

            Op::ReadU8 => {
                let ptr = stack.pop().unwrap();
                let read = unsafe { (ptr as *const u8).read() as u64 };
                stack.push(read);
            }
            Op::WriteU8 => todo!(),

            Op::Dump => println!("{:?}", stack),
            Op::Print => println!("{:?}", stack.pop().unwrap()),
            Op::Syscall3 => todo!("Syscalls not supported in eval"),

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
                call_stack.push(i);
                i = labels.get(l).copied().ok_or_else(|| l.clone())?
            }
            Op::Return => i = call_stack.pop().unwrap(),
            Op::Exit => return stack.pop().unwrap().okay(),
        }
        i += 1;
    }
    666.okay()
}
