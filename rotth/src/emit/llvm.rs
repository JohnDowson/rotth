use std::collections::BTreeMap;

use crate::lir::{CompiledProc, Op, SpannedOp};
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    passes::PassManager,
    values::{AnyValue, BasicValue, FunctionValue, IntValue},
};
use rotth_analysis::inference::ReifiedType;
use rotth_parser::{ast::Literal, types::Primitive};
use smol_str::SmolStr;

pub struct ProcBlock<'ctx> {
    pub low: usize,
    pub high: usize,
    pub max: usize,

    pub block: BasicBlock<'ctx>,
    pub stack_state: Box<[IntValue<'ctx>]>,
    flags: u8,
}

impl<'ctx> ProcBlock<'ctx> {
    const STACK_KNOWN: u8 = 0x01;
    const LOCAL_KNOWN: u8 = 0x02;
    const SELF_REGEN: u8 = 0x04;
    const GENERATED: u8 = 0x08;
    const COLOR: u8 = 0x10;
    const IN_CODE_ORDER: u8 = 0x20;

    pub fn set_stack_known(&mut self) {
        self.flags |= Self::STACK_KNOWN;
    }

    pub fn clear_stack_known(&mut self) {
        self.flags &= !Self::STACK_KNOWN;
    }

    pub fn is_stack_known(&self) -> bool {
        (self.flags & Self::STACK_KNOWN) != 0
    }

    pub fn set_local_known(&mut self) {
        self.flags |= Self::LOCAL_KNOWN;
    }

    pub fn clear_local_known(&mut self) {
        self.flags &= !Self::LOCAL_KNOWN;
    }

    pub fn is_local_known(&self) -> bool {
        (self.flags & Self::LOCAL_KNOWN) != 0
    }

    pub fn set_self_regen(&mut self) {
        self.flags |= Self::SELF_REGEN;
    }

    pub fn clear_self_regen(&mut self) {
        self.flags &= !Self::SELF_REGEN;
    }

    pub fn is_self_regen(&self) -> bool {
        (self.flags & Self::SELF_REGEN) != 0
    }

    pub fn set_generated(&mut self) {
        self.flags |= Self::GENERATED;
    }

    pub fn clear_generated(&mut self) {
        self.flags &= !Self::GENERATED;
    }

    pub fn is_generated(&self) -> bool {
        (self.flags & Self::GENERATED) != 0
    }

    pub fn set_black(&mut self) {
        self.flags |= Self::COLOR;
    }

    pub fn set_red(&mut self) {
        self.flags &= !Self::COLOR;
    }

    pub fn is_red(&self) -> bool {
        (self.flags & Self::COLOR) == 0
    }
    pub fn is_black(&self) -> bool {
        (self.flags & Self::COLOR) == 0
    }

    pub fn set_in_code_order(&mut self) {
        self.flags |= Self::IN_CODE_ORDER;
    }

    pub fn clear_in_code_order(&mut self) {
        self.flags &= !Self::IN_CODE_ORDER;
    }

    pub fn is_in_code_order(&self) -> bool {
        (self.flags & Self::IN_CODE_ORDER) != 0
    }

    pub fn new(loc: usize, block: BasicBlock<'ctx>) -> Self {
        Self {
            block,
            low: loc,
            high: loc,
            max: usize::MAX,

            stack_state: vec![].into_boxed_slice(),
            flags: 0,
        }
    }
}

pub struct Compiler<'a, 'ctx> {
    pub ctx: &'ctx Context,
    pub builder: &'a Builder<'ctx>,
    pub fpm: &'a PassManager<FunctionValue<'ctx>>,
    pub module: &'a Module<'ctx>,
    pub proc: &'a CompiledProc,

    bindings: Vec<IntValue<'ctx>>,
    operand_stack: Vec<IntValue<'ctx>>,
    instr_index: usize,
    ungenerated: Vec<ProcBlock<'ctx>>,
    current_block: Option<ProcBlock<'ctx>>,
    blocks: BTreeMap<u32, BasicBlock<'ctx>>,
    end_of_basic_block: bool,
    fallthrough: bool,
    runoff: usize,

    proc_value: Option<FunctionValue<'ctx>>,

    vid: usize,
    bid: usize,
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn compile(
        context: &'ctx Context,
        builder: &'a Builder<'ctx>,
        pass_manager: &'a PassManager<FunctionValue<'ctx>>,
        module: &'a Module<'ctx>,
        proc: &'a CompiledProc,
    ) -> Result<FunctionValue<'ctx>, &'static str> {
        let mut this = Self {
            ctx: context,
            builder,
            fpm: pass_manager,
            module,
            proc,

            bindings: Vec::new(),
            operand_stack: Vec::new(),
            instr_index: 0,
            ungenerated: Default::default(),
            current_block: Default::default(),
            blocks: Default::default(),
            end_of_basic_block: false,
            fallthrough: false,
            runoff: usize::MAX,

            proc_value: None,

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

    fn compile_fn(&mut self) -> Result<FunctionValue<'ctx>, &'static str> {
        let proc_name = &*self.proc.name;

        let params: Vec<_> = (0..self.proc.ins.len() as u32)
            .map(|_| self.ctx.i64_type().into())
            .collect();

        let rets = self.proc.outs.len() as u32;
        let ret_ty = self.ctx.i64_type().array_type(rets);

        let proc_type = ret_ty.fn_type(&params, false);
        let proc = self.module.add_function(proc_name, proc_type, None);

        for param in proc.get_param_iter() {
            self.operand_stack.push(param.into_int_value())
        }

        let entry = self.ctx.append_basic_block(proc, "entry");
        self.builder.position_at_end(entry);

        self.blocks.insert(0, entry);
        let block = ProcBlock::new(0, entry);
        self.current_block = Some(block);

        self.proc_value = Some(proc);

        self.compile_from(0)?;

        while let Some(block) = self.ungenerated.pop() {
            println!(
                "generate {:?} (stack {})",
                block.block,
                block.stack_state.len()
            );
            self.operand_stack.clear();
            self.current_block = Some(block);

            self.compile_from(self.block().low)?;
        }

        Ok(proc)
    }

    fn block(&self) -> &ProcBlock<'ctx> {
        self.current_block.as_ref().unwrap()
    }
    fn block_mut(&mut self) -> &mut ProcBlock<'ctx> {
        self.current_block.as_mut().unwrap()
    }
    fn func(&self) -> FunctionValue {
        self.proc_value.unwrap()
    }
    fn code(&self) -> &[SpannedOp] {
        &self.proc.code
    }

    fn get_or_create_block(&mut self, offset: u32) -> BasicBlock<'ctx> {
        self.end_of_basic_block = true;
        // *self.blocks.entry(offset).or_insert_with(|| {
        //     let bid = &*self.bid();
        //     let block = self.ctx.append_basic_block(self.func(), bid);
        //     let mut pb = ProcBlock::new(offset as _, block);
        //     pb.stack_state = self.operand_stack.clone().into_boxed_slice();
        //     self.ungenerated.push(pb);
        //     block
        // })
        // Because commented out version triggers borrowchecker
        #[allow(clippy::map_entry)]
        if self.blocks.contains_key(&offset) {
            *self.blocks.get(&offset).unwrap()
        } else {
            let bid = &*self.bid();
            let block = self.ctx.append_basic_block(self.func(), bid);
            self.blocks.insert(offset, block);

            let mut pb = ProcBlock::new(offset as _, block);
            pb.stack_state = self.operand_stack.clone().into_boxed_slice();
            self.ungenerated.push(pb);
            block
        }
    }

    fn compile_from(&mut self, mut index: usize) -> Result<(), &'static str> {
        self.builder.position_at_end(self.block().block);
        self.end_of_basic_block = false;
        self.fallthrough = false;
        self.block_mut().high = index;
        loop {
            self.instr_index = index;
            let SpannedOp { op, span: _ } = &self.code()[index];
            index += 1;

            match op {
                Op::Push(lit) => {
                    let v = match lit {
                        Literal::Bool(_) => todo!(),
                        Literal::Num(u) => self.ctx.i64_type().const_int(*u, false),
                        Literal::String(_) => todo!(),
                        Literal::Char(_) => todo!(),
                    };
                    self.operand_stack.push(v);
                }
                Op::PushStr(_) => todo!(),
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
                    let v = self.bindings[*i];
                    self.operand_stack.push(v)
                }
                Op::Unbind => {
                    self.bindings.pop();
                }
                Op::ReadU64 => todo!(),
                Op::ReadU8 => todo!(),
                Op::WriteU64 => todo!(),
                Op::WriteU8 => todo!(),
                Op::ReserveLocals(_) => todo!(),
                Op::FreeLocals(_) => todo!(),
                Op::PushLvar(_) => todo!(),
                Op::Dump => todo!(),
                Op::Print => todo!(),
                Op::Syscall0 => todo!(),
                Op::Syscall1 => todo!(),
                Op::Syscall2 => todo!(),
                Op::Syscall3 => todo!(),
                Op::Syscall4 => todo!(),
                Op::Syscall5 => todo!(),
                Op::Syscall6 => todo!(),
                Op::Argc => todo!(),
                Op::Argv => todo!(),
                Op::Add => {
                    let a = self.operand_stack.pop().unwrap();
                    let b = self.operand_stack.pop().unwrap();
                    let r = self.builder.build_int_add(a, b, &*self.vid());
                    self.operand_stack.push(r)
                }
                Op::Sub => {
                    let a = self.operand_stack.pop().unwrap();
                    let b = self.operand_stack.pop().unwrap();
                    let r = self.builder.build_int_sub(a, b, &*self.vid());
                    self.operand_stack.push(r)
                }
                Op::Divmod => todo!(),
                Op::Mul => {
                    let a = self.operand_stack.pop().unwrap();
                    let b = self.operand_stack.pop().unwrap();
                    let r = self.builder.build_int_mul(a, b, &*self.vid());
                    self.operand_stack.push(r)
                }
                Op::Eq => todo!(),
                Op::Ne => todo!(),
                Op::Lt => todo!(),
                Op::Le => todo!(),
                Op::Gt => todo!(),
                Op::Ge => todo!(),
                Op::Label(_) => todo!(),
                Op::Jump(_) => todo!(),
                Op::JumpF(_) => todo!(),
                Op::JumpT(_) => todo!(),
                Op::Call(_) => todo!(),
                Op::Return => {
                    let ret = self.builder.build_array_alloca(
                        self.ctx.i64_type(),
                        self.ctx.i64_type().const_int(4, false),
                        "outs",
                    );
                    self.builder.build_return(Some(&ret));
                }
                Op::Exit => todo!(),
            }

            // This isn't in the original code, but without it the index overflows array bounds
            if index == self.code().len() {
                return Ok(());
            }
            if !self.end_of_basic_block && index == self.runoff {
                self.end_of_basic_block = true;
                self.fallthrough = true;
            }
            if self.end_of_basic_block {
                if self.fallthrough {
                    let block = self.get_or_create_block(index as _);
                    self.builder.build_unconditional_branch(block);
                }
                return Ok(());
            }
        }
    }
}

#[test]
fn test() {
    use crate::lir::spanify;
    use Op::*;
    let u64 = ReifiedType::Primitive(Primitive::U64);
    let proc = CompiledProc {
        ins: vec![u64.clone(), u64.clone()],
        outs: vec![u64],
        name: "add".into(),
        code: spanify([Add, Push(Literal::Num(69)), Add, Return]),
    };
    let context = Context::create();
    let module = context.create_module("temp");
    let builder = context.create_builder();
    let fpm = PassManager::create(&module);
    fpm.initialize();

    let res = Compiler::compile(&context, &builder, &fpm, &module, &proc).unwrap();
    res.print_to_stderr();
}
