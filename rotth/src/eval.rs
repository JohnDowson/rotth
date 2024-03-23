use crate::lir2::{cfg::Block, Op, Value};

pub fn eval(blocks: &[Block], debug: bool) -> Option<Value> {
    let mut block = &blocks[0];

    let mut bind_stack = Vec::new();
    let mut stack = Vec::new();
    let mut i = 0;

    while let Some(op) = block.ops.get(i) {
        if debug {
            println!("{}:\t{:?}", i, op);
        }
        match op {
            Op::PushMem(_i) => {
                todo!("Support memories in eval")
            }
            Op::Push(v) => stack.push(v.clone()),
            Op::Drop => {
                stack.pop();
            }
            Op::Dup => {
                let v = stack.last().cloned().unwrap();
                stack.push(v);
            }
            Op::Swap => {
                let (a, b) = (stack.pop().unwrap(), stack.pop().unwrap());
                stack.push(a);
                stack.push(b);
            }
            Op::Over => {
                let v = stack[stack.len() - 2].clone();
                stack.push(v);
            }

            Op::Bind => bind_stack.push(stack.pop().unwrap()),
            Op::UseBinding(offset) => {
                stack.push(bind_stack[(bind_stack.len() - 1) - offset].clone())
            }
            Op::Unbind => {
                bind_stack.pop();
            }

            Op::Read(_) | Op::Write(_) => {
                panic!("Pointer operations are not supported in const eval")
            }

            Op::Syscall(_) => todo!("Syscalls not supported in eval"),

            Op::Add => {
                let v = match (stack.pop().unwrap(), stack.pop().unwrap()) {
                    (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
                    (Value::UInt(a), Value::UInt(b)) => Value::UInt(a + b),
                    _ => unreachable!(),
                };
                stack.push(v);
            }
            Op::Sub => {
                let v = match (stack.pop().unwrap(), stack.pop().unwrap()) {
                    (Value::Int(a), Value::Int(b)) => Value::Int(a - b),
                    (Value::UInt(a), Value::UInt(b)) => Value::UInt(a - b),
                    _ => unreachable!(),
                };
                stack.push(v);
            }
            Op::Divmod => {
                let (d, m) = match (stack.pop().unwrap(), stack.pop().unwrap()) {
                    (Value::Int(a), Value::Int(b)) => (Value::Int(a / b), Value::Int(a % b)),
                    (Value::UInt(a), Value::UInt(b)) => (Value::UInt(a / b), Value::UInt(a % b)),
                    _ => unreachable!(),
                };
                stack.push(d);
                stack.push(m);
            }
            Op::Mul => {
                let v = match (stack.pop().unwrap(), stack.pop().unwrap()) {
                    (Value::Int(a), Value::Int(b)) => Value::Int(a * b),
                    (Value::UInt(a), Value::UInt(b)) => Value::UInt(a * b),
                    _ => unreachable!(),
                };
                stack.push(v);
            }

            Op::Eq => {
                let (a, b) = (stack.pop().unwrap(), stack.pop().unwrap());
                stack.push(Value::Bool(a == b));
            }
            Op::Ne => {
                let (b, a) = (stack.pop().unwrap(), stack.pop().unwrap());
                stack.push(Value::Bool(a != b));
            }
            Op::Lt => {
                let (b, a) = (stack.pop().unwrap(), stack.pop().unwrap());
                stack.push(Value::Bool(a < b));
            }
            Op::Le => {
                let (b, a) = (stack.pop().unwrap(), stack.pop().unwrap());
                stack.push(Value::Bool(a <= b));
            }
            Op::Gt => {
                let (b, a) = (stack.pop().unwrap(), stack.pop().unwrap());
                stack.push(Value::Bool(a > b));
            }
            Op::Ge => {
                let (b, a) = (stack.pop().unwrap(), stack.pop().unwrap());
                stack.push(Value::Bool(a >= b));
            }

            Op::Jump(t) => block = &blocks[t.0],
            Op::Branch(t, l) => {
                if stack.pop() == Some(Value::Bool(true)) {
                    block = &blocks[t.0];
                } else {
                    block = &blocks[l.0];
                }
                i = 0;
                continue;
            }
            Op::Call(_) => todo!("Call"),
            Op::Return => return stack.pop(),
            Op::PushLvar(_) => todo!(),
            Op::ReserveLocals(_) => todo!(),
            Op::FreeLocals(_) => todo!(),
        }
        i += 1;
    }
    None
}
