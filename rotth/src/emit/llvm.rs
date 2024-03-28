use std::collections::BTreeMap;

use crate::lir2::{cfg::BlockId, CompiledProc, Op, Value};
use inkwell::{
    basic_block::BasicBlock,
    builder::{Builder, BuilderError},
    context::Context,
    module::Module,
    passes::PassManager,
    values::{FunctionValue, InstructionOpcode, IntValue, PointerValue},
    AddressSpace,
};
use rotth_parser::ast::Literal;
use smol_str::SmolStr;

struct ProcBlock<'ctx> {
    low: usize,
    high: usize,
    block: BasicBlock<'ctx>,
    stack_state: Box<[IntValue<'ctx>]>,
}

impl<'ctx> ProcBlock<'ctx> {
    fn new(loc: usize, block: BasicBlock<'ctx>) -> Self {
        Self {
            block,
            low: loc,
            high: loc,

            stack_state: vec![].into_boxed_slice(),
        }
    }
}

pub struct Compiler<'a, 'ctx> {
    pub ctx: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub fpm: &'a PassManager<FunctionValue<'ctx>>,
    pub module: &'a Module<'ctx>,
    pub proc: CompiledProc,

    bindings: Vec<IntValue<'ctx>>,
    operand_stack: Vec<IntValue<'ctx>>,
    ungenerated: Vec<ProcBlock<'ctx>>,
    current_block: Option<ProcBlock<'ctx>>,
    blocks: BTreeMap<BlockId, BasicBlock<'ctx>>,
    end_of_block: bool,

    proc_value: Option<FunctionValue<'ctx>>,
    ret_slot: Option<PointerValue<'ctx>>,

    vid: usize,
    bid: usize,
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn compile(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        pass_manager: &'a PassManager<FunctionValue<'ctx>>,
        module: &'a Module<'ctx>,
        proc: CompiledProc,
    ) -> Result<FunctionValue<'ctx>, BuilderError> {
        let mut this = Self {
            ctx: context,
            builder,
            fpm: pass_manager,
            module,
            proc,

            bindings: Vec::new(),
            operand_stack: Vec::new(),
            ungenerated: Default::default(),
            current_block: Default::default(),
            blocks: Default::default(),
            end_of_block: false,

            proc_value: None,
            ret_slot: None,

            vid: 0,
            bid: 0,
        };
        this.compile_fn()
    }

    fn vid(&mut self) -> SmolStr {
        let vid = self.vid;
        self.vid += 1;
        SmolStr::new_inline(&vid.to_string())
    }
    fn bid(&mut self) -> SmolStr {
        let bid = self.bid;
        self.bid += 1;
        SmolStr::new_inline(&bid.to_string())
    }

    fn ret_slot(&self) -> PointerValue<'ctx> {
        self.ret_slot.unwrap()
    }

    fn compile_fn(&mut self) -> Result<FunctionValue<'ctx>, BuilderError> {
        let proc_name = &*self.proc.name;

        let params: Vec<_> = (0..self.proc.ins.len() as u32)
            .map(|_| self.ctx.i64_type().into())
            .collect();

        let rets = self.proc.outs.len() as u32;
        let ret_ty = self
            .ctx
            .i64_type()
            .array_type(rets)
            .ptr_type(AddressSpace::default());

        let proc_type = ret_ty.fn_type(&params, false);
        let proc = self.module.add_function(proc_name, proc_type, None);

        for param in proc.get_param_iter() {
            self.operand_stack.push(param.into_int_value())
        }

        let entry = self.ctx.append_basic_block(proc, "entry");
        self.builder.position_at_end(entry);
        let ret_slot = self.builder.build_array_alloca(
            self.ctx.i64_type(),
            self.ctx.i64_type().const_int(rets as _, false),
            "outs",
        )?;
        self.ret_slot = Some(ret_slot);

        self.blocks.insert(0, entry);
        let block = ProcBlock::new(0, entry);
        self.current_block = Some(block);

        self.proc_value = Some(proc);

        self.compile_block(self.proc.entry)?;

        while let Some(block) = self.ungenerated.pop() {
            self.operand_stack.clear();
            self.operand_stack.extend_from_slice(&*block.stack_state);
            self.current_block = Some(block);

            self.compile_block(self)?;
        }

        Ok(proc)
    }

    fn block(&self) -> &ProcBlock<'ctx> {
        self.current_block.as_ref().unwrap()
    }
    fn block_mut(&mut self) -> &mut ProcBlock<'ctx> {
        self.current_block.as_mut().unwrap()
    }
    fn func(&self) -> FunctionValue<'ctx> {
        self.proc_value.unwrap()
    }

    fn get_or_create_block(&mut self, id: BlockId, name: Option<&'static str>) -> BasicBlock<'ctx> {
        self.end_of_block = true;
        #[allow(clippy::map_entry)]
        if self.blocks.contains_key(&id) {
            *self.blocks.get(&id).unwrap()
        } else {
            let bid = self.bid();
            let bid = if let Some(name) = name { name } else { &*bid };
            let block = self.ctx.append_basic_block(self.func(), bid);
            self.blocks.insert(id, block);

            let mut pb = ProcBlock::new(id as _, block);
            pb.stack_state = self.operand_stack.clone().into_boxed_slice();
            self.ungenerated.push(pb);
            block
        }
    }

    fn compile_block(&mut self, index: BlockId) -> Result<(), BuilderError> {
        self.builder.position_at_end(self.block().block);
        for op in self.proc.blocks[index.0].ops {
            match op {
                Op::Push(lit) => {
                    let v = match lit {
                        Value::Bool(_) => todo!(),
                        Value::Int(i) => todo!(),
                        Value::UInt(u) => self.ctx.i64_type().const_int(u, false),
                        Value::String(_) => todo!(),
                        Value::Char(_) => todo!(),
                    };
                    self.operand_stack.push(v);
                }
                Op::PushMem(_) => todo!(),
                Op::Drop => {
                    self.operand_stack.pop();
                }
                Op::Dup => {
                    let v = self.operand_stack.pop().unwrap();
                    self.operand_stack.push(v);
                    self.operand_stack.push(v);
                }
                Op::Swap => {
                    let a = self.operand_stack.pop().unwrap();
                    let b = self.operand_stack.pop().unwrap();
                    self.operand_stack.push(a);
                    self.operand_stack.push(b);
                }
                Op::Over => {
                    let a = self.operand_stack.pop().unwrap();
                    let b = self.operand_stack.pop().unwrap();
                    self.operand_stack.push(b);
                    self.operand_stack.push(a);
                    self.operand_stack.push(b);
                }
                Op::Bind => {
                    let v = self.operand_stack.pop().unwrap();
                    self.bindings.push(v);
                }
                Op::UseBinding(i) => {
                    let v = self.bindings[i];
                    self.operand_stack.push(v)
                }
                Op::Unbind => {
                    self.bindings.pop();
                }
                Op::Read(n) => todo!(),
                Op::Write(n) => todo!(),
                Op::ReserveLocals(_) => todo!(),
                Op::FreeLocals(_) => todo!(),
                Op::PushLvar(_) => todo!(),
                Op::Syscall(n) => todo!(),
                Op::Add => {
                    let a = self.operand_stack.pop().unwrap();
                    let b = self.operand_stack.pop().unwrap();
                    let r = self.builder.build_int_add(a, b, &*self.vid())?;
                    self.operand_stack.push(r)
                }
                Op::Sub => {
                    let a = self.operand_stack.pop().unwrap();
                    let b = self.operand_stack.pop().unwrap();
                    let r = self.builder.build_int_sub(a, b, &*self.vid())?;
                    self.operand_stack.push(r)
                }
                Op::Divmod => todo!(),
                Op::Mul => {
                    let a = self.operand_stack.pop().unwrap();
                    let b = self.operand_stack.pop().unwrap();
                    let r = self.builder.build_int_mul(a, b, &*self.vid())?;
                    self.operand_stack.push(r)
                }
                Op::Eq => todo!(),
                Op::Ne => todo!(),
                Op::Lt => todo!(),
                Op::Le => todo!(),
                Op::Gt => todo!(),
                Op::Ge => todo!(),
                Op::Label(_) => {}
                Op::Jump(target) => {
                    let target = self.labels[target];
                    let target = self.get_or_create_block(target, None);
                    self.builder.build_unconditional_branch(target);
                }
                Op::Branch(target) => {
                    let target = self.labels[target];

                    let x = self.operand_stack.pop().unwrap();
                    let x = self
                        .builder
                        .build_cast(InstructionOpcode::Trunc, x, self.ctx.bool_type(), "cond")?
                        .into_int_value();
                    let target = self.get_or_create_block(target, Some("True"));
                    let else_block = self.get_or_create_block(index, Some("Else"));

                    self.builder.build_conditional_branch(x, target, else_block);
                }
                Op::Call(_) => todo!(),
                Op::Return => {
                    let ret = self.ret_slot();
                    for (i, _) in self.proc.outs.iter().enumerate() {
                        let value = self.operand_stack.pop().unwrap();
                        let slot = unsafe {
                            self.builder.build_gep(
                                todo!(),
                                ret,
                                &[self.ctx.i64_type().const_int(i as _, false)],
                                "build_ret",
                            )?
                        };
                        self.builder.build_store(slot, value);
                    }

                    let ret = self
                        .builder
                        .build_bitcast(
                            ret,
                            self.ctx
                                .i64_type()
                                .array_type(2)
                                .ptr_type(AddressSpace::default()),
                            "cast_ret",
                        )?
                        .into_pointer_value();

                    self.builder.build_return(Some(&ret));
                    self.end_of_block = true;
                }
            }
        }
        Ok(())
    }
}
