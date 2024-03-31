use self::cfg::{Block, BlockId, ProcBuilder};
use fnv::FnvHashMap;
use itempath::{ItemPath, ItemPathBuf};
use rotth_analysis::{
    ctir::{CConst, CProc, ConcreteNode, ConcreteProgram, Intrinsic},
    inference::ReifiedType,
    tir::{Bind, Cond, CondBranch, FieldAccess, If, TypedIr, While},
};
use rotth_parser::{ast::Literal, hir::Binding};
use smol_str::{SmolStr, ToSmolStr};
use spanner::Spanned;

pub mod cfg;

pub struct CompiledProc {
    pub name: SmolStr,
    pub ins: Vec<ReifiedType>,
    pub outs: Vec<ReifiedType>,
    pub blocks: Vec<Block>,
    pub entry: BlockId,
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
pub struct ComMem(usize);

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

        for (name, var) in program.vars {
            let mangled = this.mangle(&name);
            this.mangle_table.insert(name.clone(), mangled.clone());
            this.unmangle_table.insert(mangled.clone(), name.clone());
            this.mems.insert(name, ComMem(var.ty.size()));
        }

        let procs: Vec<_> = program
            .procs
            .into_iter()
            .map(|(name, proc)| {
                let mangled = this.mangle(&name);
                this.mangle_table.insert(name.clone(), mangled.clone());
                this.unmangle_table.insert(mangled.clone(), name.clone());
                (name, mangled, proc)
            })
            .collect();

        let procs = procs
            .into_iter()
            .map(|(name, mangled, proc)| {
                this.inc_proc_id();
                let proc = this.compile_proc(proc, name.to_smolstr());
                (mangled, proc)
            })
            .collect();

        let mems = this
            .mems
            .into_iter()
            .map(|(n, m)| match m {
                ComMem(c) => (this.mangle_table.get(&n).unwrap().clone(), c),
            })
            .collect();

        (procs, this.strings, mems)
    }

    fn compile_proc(
        &mut self,
        CProc {
            generics: _,
            vars: _,
            ins,
            outs,
            body,
            callees: _,
        }: CProc,
        name: SmolStr,
    ) -> CompiledProc {
        let mut proc = ProcBuilder::new();
        self.compile_body(&mut proc, body);
        proc.jump(proc.exit);
        proc.switch_block(proc.exit);
        proc.return_();

        proc.dbg_graph("./proc.dot").unwrap();

        let ProcBuilder {
            blocks,
            current_block: _,
            entry,
            exit: _,
            bindings: _,
            locals: _,
        } = proc;
        CompiledProc {
            name,
            ins,
            outs,
            blocks,
            entry,
        }
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

    fn compile_cond(
        &mut self,
        proc: &mut ProcBuilder,
        Cond { branches }: Cond<ReifiedType, Intrinsic>,
    ) {
        let phi_b = proc.new_block();
        let n_branches = branches.len() - 1;
        let mut this_pat_b = proc.new_block();
        proc.jump(this_pat_b);
        for (i, CondBranch { pattern, body }) in branches.into_iter().enumerate() {
            proc.switch_block(this_pat_b);
            match pattern.node {
                TypedIr::ConstUse(_) => todo!(),
                TypedIr::Literal(l) => match l {
                    Literal::Bool(b) => proc.push(Value::Bool(b)),
                    Literal::Int(i) => proc.push(Value::Int(i)),
                    Literal::UInt(u) => proc.push(Value::UInt(u)),
                    Literal::String(s) => proc.push(Value::String(s)),
                    Literal::Char(c) => proc.push(Value::Char(c)),
                },
                TypedIr::IgnorePattern => proc.dup(),
                _ => unreachable!(),
            }
            let body_b = proc.new_block();
            proc.eq();
            if i < n_branches {
                this_pat_b = proc.new_block();
                proc.branch(body_b, this_pat_b);
            } else {
                proc.jump(body_b)
            }
            proc.switch_block(body_b);
            self.compile_body(proc, body);
            proc.jump(phi_b);
        }
        proc.switch_block(phi_b);
    }

    fn compile_body(&mut self, proc: &mut ProcBuilder, body: Vec<ConcreteNode>) {
        for node in body {
            match node.node {
                TypedIr::GVarUse(name) => {
                    let mangled = self.mangle_table.get(&name).unwrap().clone();
                    proc.push_mem(mangled);
                }
                TypedIr::LVarUse(lvar) => {
                    let lvar = proc.locals.get(&lvar).unwrap();
                    proc.push_lvar(*lvar);
                }
                TypedIr::BindingUse(name) => {
                    let offset = proc
                        .bindings
                        .iter()
                        .flatten()
                        .rev()
                        .position(|b| b == &name)
                        .unwrap();
                    proc.use_binding(offset);
                }
                TypedIr::ConstUse(_) => todo!(),
                TypedIr::Call(name) => {
                    let mangled = self.mangle_table.get(&name).unwrap();
                    proc.call(mangled.clone())
                }
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
                },
                TypedIr::Bind(Bind { bindings, body }) => {
                    proc.bindings.push(Vec::new());
                    for Spanned {
                        span: _,
                        inner: binding,
                    } in &bindings
                    {
                        match binding {
                            Binding::Ignore => (),
                            Binding::Bind { name, ty: _ } => {
                                proc.bind();
                                proc.bindings.last_mut().unwrap().push(name.clone());
                            }
                        }
                    }
                    self.compile_body(proc, body);
                    for _ in proc.bindings.pop().unwrap() {
                        proc.unbind();
                    }
                }
                TypedIr::While(While { cond, body }) => {
                    let cond_b = proc.new_block();
                    let body_b = proc.new_block();
                    let after = proc.new_block();
                    proc.switch_block(cond_b);
                    self.compile_body(proc, cond);
                    proc.branch(body_b, after);
                    proc.switch_block(body_b);
                    self.compile_body(proc, body);
                    proc.jump(cond_b);
                    proc.switch_block(after)
                }
                TypedIr::If(if_) => self.compile_if(proc, if_),
                TypedIr::Cond(cond) => self.compile_cond(proc, cond),
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
                TypedIr::FieldAccess(FieldAccess {
                    ty: ReifiedType::Custom(ty),
                    field,
                }) => {
                    let size = ty.fields.get(&field).unwrap().ty.size();
                    proc.read(size);
                }
                TypedIr::FieldAccess(_) => {
                    unreachable!()
                }
            }
        }
    }

    fn mangle(&self, name: &ItemPath) -> Mangled {
        let joiner = "__".to_string().into();
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
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

    Syscall(u8),
}
