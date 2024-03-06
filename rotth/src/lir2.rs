use self::cfg::{Block, BlockId, ProcBuilder};
use fnv::FnvHashMap;
use rotth_analysis::{
    ctir::{CConst, CMem, CProc, ConcreteNode, ConcreteProgram, Intrinsic},
    inference::ReifiedType,
    tir::{If, TypedIr},
};
use rotth_parser::ast::{ItemPath, ItemPathBuf, Literal};
use smol_str::SmolStr;

mod cfg;

pub struct CompiledProc {
    pub ins: Vec<ReifiedType>,
    pub outs: Vec<ReifiedType>,
    pub blocks: Vec<Block>,
}

impl std::fmt::Debug for CompiledProc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, block) in self.blocks.iter().enumerate() {
            writeln!(f, "{i}:")?;
            block.fmt(f)?;
        }
        Ok(())
    }
}

#[derive(Clone)]
pub enum ComConst {
    Compiled(Vec<Literal>),
    NotCompiled(CConst),
}

#[derive(Clone)]
pub enum ComMem {
    Compiled(usize),
    NotCompiled(CMem),
}

#[derive(Clone)]
pub enum ComVar {
    Compiled(usize),
    NotCompiled(ReifiedType),
}

#[derive(Default)]
pub struct Compiler {
    strings: Vec<SmolStr>,
    mems: FnvHashMap<ItemPathBuf, ComMem>,
    consts: FnvHashMap<ItemPathBuf, ComConst>,
    mangle_table: FnvHashMap<ItemPathBuf, Mangled>,
    unmangle_table: FnvHashMap<Mangled, ItemPathBuf>,
    proc_id: usize,
}

impl Compiler {
    pub fn compile(
        program: ConcreteProgram,
    ) -> (
        FnvHashMap<Mangled, CompiledProc>,
        Vec<SmolStr>,
        FnvHashMap<Mangled, usize>,
    ) {
        let mut this = Self::default();

        for (name, const_) in program.consts {
            let mangled = this.mangle(&name);
            this.mangle_table.insert(name.clone(), mangled.clone());
            this.unmangle_table.insert(mangled.clone(), name.clone());
            this.consts.insert(name, ComConst::NotCompiled(const_));
        }

        for (name, mem) in program.mems {
            let mangled = this.mangle(&name);
            this.mangle_table.insert(name.clone(), mangled.clone());
            this.unmangle_table.insert(mangled.clone(), name.clone());
            this.mems.insert(name, ComMem::NotCompiled(mem));
        }

        for (name, var) in program.vars {
            let mangled = this.mangle(&name);
            this.mangle_table.insert(name.clone(), mangled.clone());
            this.unmangle_table.insert(mangled.clone(), name.clone());
            this.mems.insert(name, ComMem::Compiled(var.ty.size()));
        }

        let procs = program
            .procs
            .into_iter()
            .map(|(name, proc)| {
                this.inc_proc_id();
                let mangled = this.mangle(&name);
                this.mangle_table.insert(name.clone(), mangled.clone());
                this.unmangle_table.insert(mangled.clone(), name);
                let proc = this.compile_proc(proc);
                (mangled, proc)
            })
            .collect();

        let mems = this
            .mems
            .into_iter()
            .map(|(n, m)| match m {
                ComMem::Compiled(c) => (this.mangle_table.get(&n).unwrap().clone(), c),
                _ => todo!(),
            })
            .collect();

        (procs, this.strings, mems)
    }

    fn compile_proc(
        &mut self,
        CProc {
            generics,
            vars,
            ins,
            outs,
            body,
        }: CProc,
    ) -> CompiledProc {
        let mut proc = ProcBuilder::new();
        self.compile_body(&mut proc, body);

        let ProcBuilder {
            blocks,
            current_block,
            entry: _,
            exit: _,
            bindings,
            locals,
        } = proc;
        CompiledProc { ins, outs, blocks }
    }

    fn compile_if(&mut self, proc: &mut ProcBuilder, If { truth, lie }: If<ConcreteNode>) {
        let truth_blk = proc.new_block();
        let after = proc.new_block();
        if let Some(lie) = lie {
            let lie_blk = proc.new_block();
            proc.branch(truth_blk, lie_blk);

            proc.switch_block(lie_blk);
            self.compile_body(proc, lie);
            proc.switch_block(lie_blk);

            proc.jump(after);
        } else {
            proc.branch(truth_blk, after);
        }
        proc.switch_block(truth_blk);
        self.compile_body(proc, truth);
        proc.switch_block(truth_blk);
        proc.jump(after);
        proc.switch_block(after);
    }

    fn compile_body(&mut self, proc: &mut ProcBuilder, body: Vec<ConcreteNode>) {
        for node in body {
            let _span = node.span;
            match node.node {
                TypedIr::MemUse(_) => proc.push_mem(),
                TypedIr::GVarUse(_) => todo!(),
                TypedIr::LVarUse(lvar) => {
                    let lvar = proc.locals.get(&lvar).unwrap();
                    proc.push_lvar(*lvar);
                }
                TypedIr::BindingUse(_) => todo!(),
                TypedIr::ConstUse(_) => todo!(),
                TypedIr::Call(_) => todo!(),
                TypedIr::Intrinsic(i) => match i {
                    Intrinsic::Drop => proc.drop(),
                    Intrinsic::Dup => proc.dup(),
                    Intrinsic::Swap => proc.swap(),
                    Intrinsic::Over => proc.over(),
                    Intrinsic::Cast(_) => (), // this is a noop
                    Intrinsic::Read(ty) => proc.read(ty.size()),
                    Intrinsic::Write(ty) => proc.write(ty.size()),
                    Intrinsic::Add => proc.add(),
                    Intrinsic::Sub => proc.sub(),
                    Intrinsic::Divmod => proc.divmod(),
                    Intrinsic::Mul => proc.mul(),
                    Intrinsic::Eq => proc.eq(),
                    Intrinsic::Ne => proc.ne(),
                    Intrinsic::Lt => proc.lt(),
                    Intrinsic::Le => proc.le(),
                    Intrinsic::Gt => proc.gt(),
                    Intrinsic::Ge => proc.ge(),
                    i => todo!("{i:?}"),
                },
                TypedIr::Bind(_) => todo!(),
                TypedIr::While(_) => todo!(),
                TypedIr::If(if_) => self.compile_if(proc, if_),
                TypedIr::Cond(_) => todo!(),
                TypedIr::Literal(lit) => {
                    let v = match lit {
                        Literal::Bool(b) => Value::Bool(b),
                        Literal::Int(i) => Value::Int(i),
                        Literal::UInt(u) => Value::UInt(u),
                        Literal::String(s) => Value::String(s),
                        Literal::Char(c) => Value::Char(c),
                    };
                    proc.push(v);
                }
                TypedIr::IgnorePattern => unreachable!(),
                TypedIr::Return => {
                    let num_bindings = proc.bindings.iter().flatten().count();
                    for _ in 0..num_bindings {
                        proc.unbind()
                    }
                    proc.return_()
                }
                TypedIr::FieldAccess(_) => todo!(),
            }
        }
    }

    fn mangle(&self, name: &ItemPath) -> Mangled {
        let joiner = "__".into();
        let name = name
            .iter()
            .intersperse(&joiner)
            .map(|s| {
                s.replace(
                    |c: char| !c.is_alphanumeric() && c != '_',
                    &format!("{}", self.proc_id),
                )
            })
            .collect();
        Mangled(name)
    }

    fn inc_proc_id(&mut self) {
        self.proc_id += 1;
    }
}

#[derive(Debug)]
pub enum Value {
    Bool(bool),
    Int(i64),
    UInt(u64),
    String(SmolStr),
    Char(char),
}

#[derive(Default, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Mangled(pub SmolStr);

impl std::fmt::Display for Mangled {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug)]
pub enum Op {
    Push(Value),
    PushMem(Mangled),
    Drop,
    Dup,
    Swap,
    Over,

    Bind,
    UseBinding(usize),
    Unbind,

    Read(usize),
    Write(usize),

    ReserveLocals(usize),
    FreeLocals(usize),
    PushLvar(usize),

    Add,
    Sub,
    Divmod,
    Mul,

    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,

    Jump(BlockId),
    Branch(BlockId, BlockId),
    Call(Mangled),
    Return,
}
