use std::collections::HashMap;

use somok::Somok;

use crate::lir::Op;

pub fn eval(ops: Vec<Op>) -> u64 {
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
            Op::Push(c) => stack.push(c.bytes()),
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

            Op::Dump => println!("{:?}", stack),
            Op::Print => println!("{:?}", stack.pop().unwrap()),

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
            Op::ProcEnd => (),
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
                i = labels[l]
            }
            Op::Return => i = call_stack.pop().unwrap(),
            Op::Exit => return stack.pop().unwrap(),
        }
        i += 1;
    }
    666
}
