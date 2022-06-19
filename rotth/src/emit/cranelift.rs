use capstone::arch::BuildsCapstoneSyntax;
use codegen::isa;
use cranelift::codegen::ir::StackSlot;
use cranelift::prelude::StackSlotData;
use cranelift::prelude::{codegen::isa::*, Signature};
use cranelift::{
    codegen::ir,
    frontend::Variable,
    prelude::{
        codegen::settings::{self, Configurable},
        types, AbiParam,
    },
};
use cranelift::{
    codegen::ir::{FuncRef, InstBuilder},
    frontend::FunctionBuilder,
    prelude::{IntCC, JumpTableData},
};
use cranelift::{
    codegen::{self},
    frontend::FunctionBuilderContext,
};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};
use rotth_parser::ast::Literal;
use std::cell::{Ref, RefCell, RefMut};
use std::collections::BTreeMap;
use std::ops::Deref;
use std::ptr::null_mut;
use std::rc::Rc;
use std::sync::atomic::AtomicU64;
use target_lexicon::Triple;

use crate::lir::Op;

fn get_isa() -> Box<dyn TargetIsa + 'static> {
    let mut flags_builder = codegen::settings::builder();
    flags_builder.set("opt_level", "speed").unwrap();
    flags_builder.set("is_pic", "false").unwrap();
    codegen::isa::lookup(Triple::host())
        .unwrap()
        .finish(settings::Flags::new(flags_builder))
        .unwrap()
}

const TRIPLE: Triple = Triple::host();

pub fn calling_convention() -> isa::CallConv {
    isa::CallConv::triple_default(&TRIPLE)
}

pub struct BasicBlock {
    pub low: usize,
    pub high: usize,
    pub max: usize,

    pub block: ir::Block,

    pub stack_state: Box<[ir::Value]>,
    flags: u8,
}

impl BasicBlock {
    const STACK_KNOWN: u8 = 0x01;
    const LOCAL_KNOWN: u8 = 0x02;
    const SELF_REGEN: u8 = 0x04;
    const GENERATED: u8 = 0x08;
    const COLOR: u8 = 0x10;
    const IN_CODE_ORDER: u8 = 0x20;
}

impl BasicBlock {
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

    pub fn new(loc: usize, block: ir::Block) -> Self {
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

pub struct Stack2SSA<'a> {
    builder: FunctionBuilder<'a>,
    locals: StackSlot,
    bindings: Vec<ir::Value>,
    operand_stack: Vec<ir::Value>,
    code: &'a Vec<Op>,
    instr_index: usize,
    ungenerated: Vec<BasicBlock>,
    current_block: BasicBlock,
    blocks: BTreeMap<u32, ir::Block>,
    end_of_basic_block: bool,
    fallthrough: bool,
    runoff: usize,
}

impl<'a> Stack2SSA<'a> {
    fn generate(&mut self) {
        self.generate_from(0);

        while let Some(block) = self.ungenerated.pop() {
            println!(
                "generate {:?} (stack {})",
                block.block,
                block.stack_state.len()
            );
            self.operand_stack.clear();
            let params = self.builder.block_params(block.block);
            self.operand_stack.extend_from_slice(params);
            self.current_block = block;

            self.generate_from(self.current_block.low);
        }
        println!(
            "initial IR after lowering: \n{}",
            self.builder.func.display()
        );
    }

    fn get_or_create_block(&mut self, offset: u32) -> ir::Block {
        self.end_of_basic_block = true;
        *self.blocks.entry(offset).or_insert_with(|| {
            let block = self.builder.create_block();
            for _ in 0..self.operand_stack.len() {
                self.builder.append_block_param(block, types::I32);
            }
            let mut bb = BasicBlock::new(offset as _, block);

            bb.stack_state = self.operand_stack.clone().into_boxed_slice();
            self.ungenerated.push(bb);
            block
        })
    }

    #[allow(unused_variables, unused_assignments)]
    fn generate_from(&mut self, from: usize) {
        self.builder.switch_to_block(self.current_block.block);
        let mut index = from;
        self.end_of_basic_block = false;
        self.fallthrough = false;
        loop {
            self.current_block.high = from;
            self.instr_index = index;
            let code = &self.code[index];

            index += 1;
            let mut s = None;
            match code {
                Op::Push(value) => match value {
                    Literal::Bool(value) => {
                        let x = self.builder.ins().iconst(types::I64, (*value) as i64);
                        self.operand_stack.push(x);
                    }
                    Literal::Num(value) => {
                        let x = self.builder.ins().iconst(types::I64, (*value) as i64);
                        self.operand_stack.push(x);
                    }
                    Literal::String(_) => todo!(),
                    Literal::Char(value) => {
                        let x = self.builder.ins().iconst(types::I64, (*value) as i64);
                        self.operand_stack.push(x);
                    }
                },
                Op::Add => {
                    let y = self.operand_stack.pop().unwrap();
                    let x = self.operand_stack.pop().unwrap();
                    let z = self.builder.ins().iadd(x, y);
                    self.operand_stack.push(z);
                }
                Op::Return => {
                    let v = self.operand_stack.pop().unwrap();
                    self.end_of_basic_block = true;
                    s = Some(self.builder.ins().return_(&[v]));
                }
                Op::PushStr(_) => todo!(),
                Op::PushMem(_) => todo!(),
                Op::Drop => {
                    self.operand_stack.pop().unwrap();
                }
                Op::Dup => {
                    let x = self.operand_stack.pop().unwrap();
                    self.operand_stack.push(x);
                    self.operand_stack.push(x);
                }
                Op::Swap => {
                    let x = self.operand_stack.pop().unwrap();
                    let y = self.operand_stack.pop().unwrap();
                    self.operand_stack.push(y);
                    self.operand_stack.push(x);
                }
                Op::Over => {
                    let x = self.operand_stack.pop().unwrap();
                    let y = self.operand_stack.pop().unwrap();
                    self.operand_stack.push(y);
                    self.operand_stack.push(x);
                    self.operand_stack.push(y);
                }
                Op::Bind => {
                    let x = self.operand_stack.pop().unwrap();
                    self.bindings.push(x)
                }
                Op::UseBinding(id) => {
                    let x = self.bindings[*id];
                    self.operand_stack.push(x)
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
                Op::Sub => todo!(),
                Op::Divmod => todo!(),
                Op::Mul => todo!(),
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
                Op::Exit => todo!(),
            };

            if !self.end_of_basic_block && index == self.runoff {
                self.end_of_basic_block = true;
                self.fallthrough = true;
            }
            if self.end_of_basic_block {
                if self.fallthrough {
                    let block = self.get_or_create_block(index as _);
                    self.builder.ins().jump(block, &self.operand_stack);
                }
                return;
            }
        }
    }
}

pub struct JIT {
    builder: FunctionBuilderContext,
    ctx: codegen::Context,
    module: JITModule,
}
static ID: AtomicU64 = AtomicU64::new(0);

fn get_jit_name() -> String {
    format!(
        "jit-{}",
        ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
    )
}

impl JIT {
    pub fn new() -> Self {
        let mut builder =
            JITBuilder::with_isa(get_isa(), cranelift_module::default_libcall_names());

        builder.hotswap(false);
        let module = JITModule::new(builder);

        Self {
            ctx: module.make_context(),
            module,
            builder: FunctionBuilderContext::new(),
        }
    }

    pub fn compile(&mut self, code: &Vec<Op>, argc: usize, locals: usize) -> *mut u8 {
        self.module.clear_context(&mut self.ctx);
        for _ in 0..argc {
            self.ctx
                .func
                .signature
                .params
                .push(AbiParam::new(types::I64));
        }

        self.ctx
            .func
            .signature
            .returns
            .push(AbiParam::new(types::I64));

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder);
        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);

        let operand_stack = builder.block_params(entry_block).to_vec();
        let mut bmap = BTreeMap::new();
        bmap.insert(0, entry_block);
        let current_block = BasicBlock::new(0, entry_block);
        let locals = builder.create_stack_slot(StackSlotData::new(
            cranelift::prelude::StackSlotKind::ExplicitSlot,
            locals as u32 * 4,
        ));
        let mut compiler = Stack2SSA {
            builder,
            operand_stack,
            blocks: bmap,
            end_of_basic_block: false,
            fallthrough: false,
            code,
            current_block,
            locals,
            instr_index: 0,
            runoff: usize::MAX,
            ungenerated: vec![],
            bindings: vec![],
        };

        compiler.generate();
        compiler.builder.seal_all_blocks();
        cranelift_preopt::optimize(&mut self.ctx, &*get_isa()).unwrap();
        println!("IR: \n{}", self.ctx.func.display());
        let name = get_jit_name();
        let id = self
            .module
            .declare_function(
                &name,
                cranelift_module::Linkage::Export,
                &self.ctx.func.signature,
            )
            .unwrap();

        let info = self.ctx.compile(&*get_isa()).unwrap();
        let mut code_ = vec![0; info.total_size as usize];
        unsafe {
            self.ctx.emit_to_memory(&mut code_[0]);
        }
        self.module.define_function(id, &mut self.ctx).unwrap();
        self.module.clear_context(&mut self.ctx);
        self.module.finalize_definitions();
        let code = self.module.get_finalized_function(id);

        println!("Disassembly: ",);
        let cs = disasm();
        let insns = cs.disasm_all(&code_, code as _);
        for ins in insns.unwrap().iter() {
            println!("{}", ins);
        }

        code as *mut u8
    }
}

impl Default for JIT {
    fn default() -> Self {
        Self::new()
    }
}
fn disasm() -> capstone::Capstone {
    use capstone::arch::BuildsCapstone;
    let cs = capstone::prelude::Capstone::new();
    #[cfg(target_arch = "aarch64")]
    {
        let mut cs = cs
            .arm64()
            .mode(capstone::prelude::arch::arm64::ArchMode::Arm)
            .detail(true)
            .build()
            .unwrap();
        cs.set_skipdata(true).unwrap();
        cs
    }
    #[cfg(target_arch = "x86_64")]
    {
        cs.x86()
            .mode(capstone::prelude::arch::x86::ArchMode::Mode64)
            .syntax(capstone::prelude::arch::x86::ArchSyntax::Att)
            .detail(true)
            .build()
            .unwrap()
    }
}

#[test]
fn test() {
    let mut jit = JIT::new();

    let code = vec![
        Op::Push(Literal::Num(1)),
        Op::Bind,
        Op::UseBinding(0),
        Op::Return,
    ];

    let code = jit.compile(&code, 0, 0);
    let func = unsafe { std::mem::transmute::<_, extern "C" fn() -> u64>(code) };

    println!("{}", func());
}
