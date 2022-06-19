use crate::eval::eval;
use fnv::FnvHashMap;
use rotth_analysis::{
    ctir::{CConst, CMem, CProc, ConcreteNode, ConcreteProgram, Intrinsic},
    inference::ReifiedType,
    tir::{self, Cond, CondBranch, FieldAccess, If, TypedIr, While},
};
use rotth_parser::{
    ast::{ItemPath, ItemPathBuf, Literal},
    hir::Binding,
    types::Primitive,
};
use somok::{Either, Somok};
use spanner::Span;
use Op::*;

#[derive(Default, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Mangled(String);

impl std::fmt::Display for Mangled {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug)]
pub enum Op {
    Push(Literal),
    PushStr(usize),
    PushMem(Mangled),
    Drop,
    Dup,
    Swap,
    Over,

    Bind,
    UseBinding(usize),
    Unbind,

    ReadU64,
    ReadU8,
    WriteU64,
    WriteU8,

    ReserveLocals(usize),
    FreeLocals(usize),
    PushLvar(usize),

    Dump,
    Print,

    Syscall0,
    Syscall1,
    Syscall2,
    Syscall3,
    Syscall4,
    Syscall5,
    Syscall6,

    Argc,
    Argv,

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

    Label(Mangled),
    Jump(Mangled),
    JumpF(Mangled),
    JumpT(Mangled),
    Call(Mangled),
    Return,
    Exit,
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
pub struct CompiledProc {
    pub code: Vec<SpannedOp>,
}

impl std::fmt::Debug for CompiledProc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, op) in self.code.iter().enumerate() {
            writeln!(f, "{}:\t{:?}", i, op)?;
        }
        Ok(())
    }
}

pub fn unspan(spanned: Vec<SpannedOp>) -> Vec<Op> {
    spanned.into_iter().map(|s| s.op).collect()
}

pub struct SpannedOp {
    op: Op,
    span: Option<Span>,
}

impl std::fmt::Debug for SpannedOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(span) = self.span {
            write!(f, "{:?}\t{:?}", &self.op, span)
        } else {
            write!(f, "{:?}", &self.op)
        }
    }
}

#[derive(Default)]
pub struct Compiler {
    label: usize,
    mangle_table: FnvHashMap<ItemPathBuf, Mangled>,
    unmangle_table: FnvHashMap<Mangled, ItemPathBuf>,
    proc_id: usize,
    current_name: Mangled,
    result: FnvHashMap<Mangled, CompiledProc>,
    consts: FnvHashMap<ItemPathBuf, ComConst>,
    strings: Vec<String>,
    bindings: Vec<Vec<ItemPathBuf>>,
    mems: FnvHashMap<ItemPathBuf, ComMem>,
    local_vars: FnvHashMap<ItemPathBuf, (usize, ReifiedType)>,
    local_vars_size: usize,
    _escaping_size: usize,
    // structs: FnvHashMap<TypeId, CStruct>,
}

impl Compiler {
    pub fn compile(
        program: ConcreteProgram,
    ) -> (
        FnvHashMap<Mangled, CompiledProc>,
        Vec<String>,
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
                (mangled, proc)
            })
            .collect::<Vec<_>>();
        for (mangled, proc) in procs {
            this.compile_proc(mangled, proc)
        }

        let mems = this
            .mems
            .into_iter()
            .map(|(n, m)| match m {
                ComMem::Compiled(c) => (this.mangle_table.get(&n).unwrap().clone(), c),
                _ => todo!(),
            })
            .collect();

        (this.result, this.strings, mems)
    }

    fn compile_proc(&mut self, name: Mangled, proc: CProc) {
        self.label = 0;
        self.current_name = name.clone();
        self.result.insert(name, Default::default());

        let mut i = 0;
        for (name, var) in proc.vars {
            let offset = var.size();
            self.local_vars.insert(name, (i, var));
            i += offset
        }
        self.local_vars_size = i;
        if i > 0 {
            self.emit_unspanned(ReserveLocals(i));
        }

        self.compile_body(proc.body);

        self.local_vars = Default::default();

        if i > 0 {
            self.emit_unspanned(FreeLocals(i));
        }
        self.emit_unspanned(Return);
    }

    fn compile_const(&mut self, name: &ItemPath) -> Vec<Literal> {
        let const_ = match self.consts.get(name) {
            Some(ComConst::Compiled(c)) => return c.clone(),
            Some(ComConst::NotCompiled(c)) => c.clone(),
            None => todo!(),
        };
        let CConst { outs, body } = const_;
        let mut com = Self::with_consts_and_strings(self.consts.clone(), self.strings.clone());
        com.compile_body(body.clone());
        self.consts = com.consts;
        self.strings = com.strings;
        let ops = unspan(com.result.remove(&Mangled::default()).unwrap().code);
        let mut const_ = Vec::new();
        match eval(ops, &self.strings, false) {
            Ok(Either::Right(bytes)) => {
                for (ty, bytes) in outs.iter().zip(bytes) {
                    match ty {
                        ReifiedType::Ptr(_) => todo!(),
                        ReifiedType::Primitive(ty) => match ty {
                            Primitive::Bool => const_.push(Literal::Bool(bytes == 1)),
                            Primitive::U64 => const_.push(Literal::Num(bytes)),
                            Primitive::Char => const_.push(Literal::Char(bytes as u8 as char)),
                            _ => todo!(),
                        },
                        ReifiedType::Custom(_) => todo!(),
                    }
                }
            }
            Err(req) => {
                let c_name = self.unmangle_table.get(&req).unwrap().clone();
                self.compile_const(&c_name);
                let mut com =
                    Self::with_consts_and_strings(self.consts.clone(), self.strings.clone());
                com.compile_body(body);
                com.emit_unspanned(Exit);
                let ops = unspan(com.result.remove(&Mangled::default()).unwrap().code);
                self.consts = com.consts;
                self.strings = com.strings;
                match eval(ops, &self.strings, false) {
                    Ok(Either::Right(bytes)) => {
                        for (ty, bytes) in outs.iter().zip(bytes) {
                            match ty {
                                ReifiedType::Ptr(_) => todo!(),
                                ReifiedType::Primitive(ty) => match ty {
                                    Primitive::Bool => const_.push(Literal::Bool(bytes == 1)),
                                    Primitive::U64 => const_.push(Literal::Num(bytes)),
                                    Primitive::Char => {
                                        const_.push(Literal::Char(bytes as u8 as char))
                                    }
                                    _ => todo!(),
                                },
                                ReifiedType::Custom(_) => todo!(),
                            }
                        }
                    }
                    _ => unreachable!(),
                }
            }
            Ok(Either::Left(_)) => unreachable!(),
        };

        self.consts
            .insert(name.to_owned(), ComConst::Compiled(const_.clone()));
        const_
    }

    fn compile_mem(&mut self, name: &ItemPath) {
        let mem = match self.mems.get(name) {
            Some(ComMem::Compiled(_)) => return,
            Some(ComMem::NotCompiled(c)) => c.clone(),
            None => unreachable!(),
        };
        let CMem { body } = mem;
        let mut com = Self::with_consts_and_strings(self.consts.clone(), self.strings.clone());
        com.compile_body(body.clone());
        self.consts = com.consts;
        self.strings = com.strings;
        let ops = unspan(com.result.remove(&Mangled::default()).unwrap().code);
        let size;
        match eval(ops, &self.strings, false) {
            Ok(Either::Right(bytes)) => size = bytes[0] as usize,
            Err(req) => {
                let m_name = self.unmangle_table.get(&req).unwrap().clone();
                self.compile_const(&m_name);
                let mut com =
                    Self::with_consts_and_strings(self.consts.clone(), self.strings.clone());
                com.compile_body(body);
                com.emit_unspanned(Exit);
                let ops = unspan(com.result.remove(&Mangled::default()).unwrap().code);
                self.consts = com.consts;
                self.strings = com.strings;
                match eval(ops, &self.strings, false) {
                    Ok(Either::Right(bytes)) => size = bytes[0] as usize,
                    _ => unreachable!(),
                }
            }
            Ok(Either::Left(_)) => unreachable!(),
        };
        self.mems.insert(name.to_owned(), ComMem::Compiled(size));
    }

    fn compile_body(&mut self, body: Vec<ConcreteNode>) {
        for node in body {
            let span = node.span;
            match node.node {
                TypedIr::Cond(cond) => self.compile_cond(cond, span),
                TypedIr::Return => {
                    let num_bindings = self.bindings.iter().flatten().count();
                    for _ in 0..num_bindings {
                        self.emit(Unbind, span)
                    }
                    let i = self.local_vars_size;
                    self.emit(FreeLocals(i), span);
                    self.emit(Return, span)
                }
                TypedIr::Literal(c) => match c {
                    Literal::String(s) => {
                        let i = self.strings.len();
                        self.strings.push(s);
                        self.emit(PushStr(i), span);
                    }
                    _ => self.emit(Push(c), span),
                },
                TypedIr::ConstUse(w) => {
                    let c = self.compile_const(&w);
                    for c in c {
                        self.emit(Push(c), span)
                    }
                }
                TypedIr::MemUse(mem_name) => {
                    self.compile_mem(&mem_name);
                    let mangled = self.mangle_table.get(&mem_name).unwrap().clone();
                    self.emit(PushMem(mangled), span)
                }
                TypedIr::GVarUse(var_name) => {
                    let mangled = self.mangle_table.get(&var_name).unwrap().clone();
                    self.emit(PushMem(mangled), span)
                }
                TypedIr::LVarUse(var_name) => {
                    let lvar = self.local_vars.get(&var_name).unwrap().0;
                    self.emit(Op::PushLvar(lvar), span)
                }
                TypedIr::BindingUse(w) => {
                    let offset = self
                        .bindings
                        .iter()
                        .flatten()
                        .rev()
                        .position(|s| s == &w)
                        .unwrap();
                    self.emit(UseBinding(offset), span)
                }
                TypedIr::Call(w) => {
                    let mangled = self.mangle_table.get(&w).unwrap().clone();
                    self.emit(Call(mangled), span)
                }
                TypedIr::Intrinsic(i) => match i {
                    Intrinsic::Drop => self.emit(Drop, span),
                    Intrinsic::Dup => self.emit(Dup, span),
                    Intrinsic::Swap => self.emit(Swap, span),
                    Intrinsic::Over => self.emit(Over, span),

                    Intrinsic::Cast(_) => (), // this is a noop

                    Intrinsic::Read(ReifiedType::Primitive(Primitive::U64)) => {
                        self.emit(ReadU64, span)
                    }
                    Intrinsic::Write(ReifiedType::Primitive(Primitive::U64)) => {
                        self.emit(WriteU64, span)
                    }
                    Intrinsic::Read(ReifiedType::Primitive(Primitive::U8)) => {
                        self.emit(ReadU8, span)
                    }
                    Intrinsic::Write(ReifiedType::Primitive(Primitive::U8)) => {
                        self.emit(WriteU8, span)
                    }
                    Intrinsic::Read(_) => todo!(),
                    Intrinsic::Write(_) => todo!(),

                    Intrinsic::Add => self.emit(Add, span),
                    Intrinsic::Sub => self.emit(Sub, span),
                    Intrinsic::Divmod => self.emit(Divmod, span),
                    Intrinsic::Mul => self.emit(Mul, span),

                    Intrinsic::Eq => self.emit(Eq, span),
                    Intrinsic::Ne => self.emit(Ne, span),
                    Intrinsic::Lt => self.emit(Lt, span),
                    Intrinsic::Le => self.emit(Le, span),
                    Intrinsic::Gt => self.emit(Gt, span),
                    Intrinsic::Ge => self.emit(Ge, span),

                    Intrinsic::Dump => self.emit(Dump, span),
                    Intrinsic::Print => self.emit(Print, span),

                    Intrinsic::Syscall0 => self.emit(Syscall0, span),
                    Intrinsic::Syscall1 => self.emit(Syscall1, span),
                    Intrinsic::Syscall2 => self.emit(Syscall2, span),
                    Intrinsic::Syscall3 => self.emit(Syscall3, span),
                    Intrinsic::Syscall4 => self.emit(Syscall4, span),
                    Intrinsic::Syscall5 => self.emit(Syscall5, span),
                    Intrinsic::Syscall6 => self.emit(Syscall6, span),

                    Intrinsic::Argc => self.emit(Argc, span),
                    Intrinsic::Argv => self.emit(Argv, span),

                    Intrinsic::CompStop => return,
                },
                TypedIr::If(cond) => self.compile_if(cond, span),
                TypedIr::While(while_) => self.compile_while(while_, span),
                TypedIr::Bind(bind) => self.compile_bind(bind, span),
                TypedIr::IgnorePattern => unreachable!(), // this is a noop
                TypedIr::FieldAccess(FieldAccess { ty, field }) => {
                    let ty = if let ReifiedType::Custom(ty) = ty {
                        ty
                    } else {
                        todo!("{ty:?}")
                    };
                    let offset = ty.fields[&field].offset;
                    self.emit(Push(Literal::Num(offset as _)), span);
                    self.emit(Add, span);
                }
            }
        }
    }

    fn compile_bind(&mut self, bind: tir::Bind<ConcreteNode>, span: Span) {
        let mut new_bindings = Vec::new();
        for binding in bind.bindings.iter().rev() {
            match &binding.inner {
                Binding::Ignore => self.emit(Drop, span),
                Binding::Bind { name, ty: _ } => {
                    new_bindings.push(name.clone());
                    self.emit(Bind, span)
                }
            }
        }
        self.bindings.push(new_bindings);
        self.compile_body(bind.body);
        for binding in bind.bindings.into_iter().rev() {
            match binding.inner {
                Binding::Ignore => (),
                Binding::Bind { name: _, ty: _ } => self.emit(Unbind, span),
            }
        }
        self.bindings.pop();
    }

    fn compile_while(&mut self, while_: While<ConcreteNode>, span: Span) {
        let cond_label = self.gen_label();
        let end_label = self.gen_label();
        self.emit(Label(cond_label.clone()), span);
        self.compile_body(while_.cond);
        self.emit(JumpF(end_label.clone()), span);
        self.compile_body(while_.body);
        self.emit(Jump(cond_label), span);
        self.emit(Label(end_label), span)
    }

    fn compile_if(&mut self, if_: If<ConcreteNode>, span: Span) {
        let lie_label = self.gen_label();
        let mut end_label = None;
        self.emit(JumpF(lie_label.clone()), span);

        self.compile_body(if_.truth);
        if if_.lie.is_some() {
            end_label = self.gen_label().some();
            self.emit(Jump(end_label.clone().unwrap()), span)
        }

        self.emit(Label(lie_label), span);

        if let Some(lie) = if_.lie {
            self.compile_body(lie);
            self.emit(Label(end_label.unwrap()), span)
        }
    }

    fn compile_cond(&mut self, cond: Cond<ReifiedType, Intrinsic>, span: Span) {
        let phi_label = self.gen_label();
        let num_branches = cond.branches.len() - 1;
        let mut this_branch_label = self.gen_label();
        let mut next_branch_label = self.gen_label();
        for (i, CondBranch { pattern, body }) in cond.branches.into_iter().enumerate() {
            if i != 0 {
                self.emit(Label(this_branch_label), span);
            }

            self.emit(Dup, span);
            match pattern.node {
                TypedIr::Literal(c) => self.emit(Push(c), span),
                TypedIr::ConstUse(w) => {
                    let c = self.compile_const(&w)[0].clone();
                    self.emit(Push(c), span)
                }
                TypedIr::IgnorePattern => self.emit(Dup, span), // todo: this is hacky
                _ => unreachable!(),
            }
            self.emit(Eq, span);
            if i < num_branches {
                self.emit(JumpF(next_branch_label.clone()), span);
            }
            this_branch_label = next_branch_label;
            next_branch_label = self.gen_label();
            self.compile_body(body);
            self.emit(Jump(phi_label.clone()), span);
        }

        self.emit(Label(phi_label), span)
    }

    fn emit_unspanned(&mut self, op: Op) {
        self.result
            .get_mut(&self.current_name)
            .unwrap()
            .code
            .push(SpannedOp { op, span: None })
    }

    fn emit(&mut self, op: Op, span: Span) {
        self.result
            .get_mut(&self.current_name)
            .unwrap()
            .code
            .push(SpannedOp {
                op,
                span: Some(span),
            })
    }

    fn gen_label(&mut self) -> Mangled {
        let res = Mangled(format!(".{}_{}", self.current_name, self.label));
        self.label += 1;
        res
    }

    fn inc_proc_id(&mut self) {
        self.proc_id += 1;
    }

    fn with_consts_and_strings(
        consts: FnvHashMap<ItemPathBuf, ComConst>,
        strings: Vec<String>,
    ) -> Self {
        let mut result: FnvHashMap<Mangled, CompiledProc> = Default::default();
        result.insert(Default::default(), Default::default());
        Self {
            consts,
            strings,
            result,
            ..Default::default()
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
            .collect::<String>();
        Mangled(name)
    }
}
