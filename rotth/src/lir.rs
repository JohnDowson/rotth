use crate::{
    eval::eval,
    hir::{
        self, Bind, Binding, Cond, CondBranch, Const, HirKind, HirNode, If, Intrinsic, Mem, Proc,
        TopLevel, While,
    },
    iconst::IConst,
    types::{self, StructIndex, Type},
};

#[derive(Debug)]
pub enum Op {
    Push(IConst),
    PushStr(usize),
    PushMem(String),
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

    ReserveEscaping(usize),
    PushEscaping(usize),

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

    Proc(String),
    Label(String),
    Jump(String),
    JumpF(String),
    JumpT(String),
    Call(String),
    Return,
    Exit,
}
use fnv::FnvHashMap;
use somok::{Either, PartitionThree, Somok, Ternary};
use Op::*;

#[derive(Clone)]
enum ComConst {
    Compiled(Vec<IConst>),
    NotCompiled(Const),
}

#[derive(Clone)]
enum ComMem {
    Compiled(usize),
    NotCompiled(Mem),
}

pub struct Compiler {
    label: usize,
    mangle_table: FnvHashMap<String, String>,
    proc_id: usize,
    current_name: String,
    result: Vec<Op>,
    consts: FnvHashMap<String, ComConst>,
    strings: Vec<String>,
    bindings: Vec<Vec<String>>,
    mems: FnvHashMap<String, ComMem>,
    vars: FnvHashMap<String, types::Type>,
    local_vars: FnvHashMap<String, (usize, hir::Var)>,
    local_vars_size: usize,
    escaping_size: usize,
    structs: StructIndex,
}

impl Compiler {
    pub fn compile(
        mut self,
        items: FnvHashMap<String, TopLevel>,
    ) -> (Vec<Op>, Vec<String>, FnvHashMap<String, usize>) {
        let (procs, consts_mems_gvars) = items
            .into_iter()
            .partition::<Vec<_>, _>(|(_, it)| matches!(it, TopLevel::Proc(_)));
        let procs = procs
            .into_iter()
            .map(|(name, proc)| {
                if let TopLevel::Proc(proc) = proc {
                    let mangled = self.mangle_name(name);
                    (mangled, proc)
                } else {
                    unreachable!()
                }
            })
            .collect::<Vec<_>>();

        let (consts, mems, vars) =
            consts_mems_gvars
                .into_iter()
                .partition_three::<Vec<_>, _>(|(_, it)| match it {
                    TopLevel::Proc(_) => unreachable!(),
                    TopLevel::Const(_) => Ternary::First,
                    TopLevel::Mem(_) => Ternary::Second,
                    TopLevel::Var(_) => Ternary::Third,
                });

        self.mems = mems
            .into_iter()
            .map(|(name, mem)| {
                if let TopLevel::Mem(mem) = mem {
                    (name, ComMem::NotCompiled(mem))
                } else {
                    unreachable!()
                }
            })
            .collect::<FnvHashMap<_, _>>();

        self.vars = vars
            .into_iter()
            .map(|(name, mem)| {
                if let TopLevel::Var(var) = mem {
                    (name, var.ty)
                } else {
                    unreachable!()
                }
            })
            .collect::<FnvHashMap<_, _>>();

        self.consts = consts
            .into_iter()
            .map(|(name, const_)| {
                if let TopLevel::Const(const_) = const_ {
                    (name, ComConst::NotCompiled(const_))
                } else {
                    unreachable!()
                }
            })
            .collect::<FnvHashMap<_, _>>();

        self.emit(Call("main".to_string()));

        self.emit(Exit);
        for (name, proc) in procs {
            self.compile_proc(name, proc)
        }

        let vars = self
            .vars
            .into_iter()
            .map(|(nm, ty)| (nm, ty.size(&self.structs)));
        (
            self.result,
            self.strings,
            self.mems
                .into_iter()
                .map(|(nm, sz)| {
                    (nm, {
                        match sz {
                            ComMem::Compiled(sz) => sz,
                            ComMem::NotCompiled(_) => unreachable!(),
                        }
                    })
                })
                .chain(vars)
                .collect(),
        )
    }

    fn compile_proc(&mut self, name: String, proc: Proc) {
        self.label = 0;
        self.current_name = name.clone();
        let label = name;
        self.emit(Proc(label));

        let mut i = 0;
        let (local, escaping) = proc
            .vars
            .into_iter()
            .partition::<Vec<_>, _>(|(_, v)| v.escaping);
        for (name, var) in local {
            let offset = var.ty.size(&self.structs);
            self.local_vars.insert(name, (i, var));
            i += offset
        }
        self.local_vars_size = i;
        self.emit(ReserveLocals(i));

        for (name, var) in escaping {
            let offset = var.ty.size(&self.structs);
            self.local_vars.insert(name, (i, var));
            self.escaping_size += offset
        }
        self.emit(ReserveEscaping(i));

        self.compile_body(proc.body);

        self.local_vars = Default::default();

        self.emit(FreeLocals(i));
        self.emit(Return);
    }

    fn compile_const(&mut self, name: String) -> Vec<IConst> {
        let const_ = match self.consts.get(&name) {
            Some(ComConst::Compiled(i)) => return i.clone(),
            Some(ComConst::NotCompiled(c)) => c.clone(),
            None => unreachable!(),
        };
        let Const {
            outs,
            body,
            span: _,
        } = const_;
        let mut com = Self::with_consts_and_strings(self.consts.clone(), self.strings.clone());
        com.compile_body(body.clone());
        self.consts = com.consts;
        self.strings = com.strings;
        let ops = com.result;
        let mut const_ = Vec::new();
        match eval(ops, &self.strings) {
            Ok(Either::Right(bytes)) => {
                for (&ty, bytes) in outs.iter().zip(bytes) {
                    match ty {
                        Type::BOOL => const_.push(IConst::Bool(bytes == 1)),
                        Type::U64 => const_.push(IConst::U64(bytes)),
                        Type::I64 => const_.push(IConst::I64(bytes as i64)),
                        Type::CHAR => const_.push(IConst::Char(bytes as u8 as char)),
                        ty => unreachable!("{:?}", ty),
                    }
                }
            }
            Err(req) => {
                self.compile_const(req);
                let mut com =
                    Self::with_consts_and_strings(self.consts.clone(), self.strings.clone());
                com.compile_body(body);
                com.emit(Exit);
                let ops = com.result;
                self.consts = com.consts;
                self.strings = com.strings;
                match eval(ops, &self.strings) {
                    Ok(Either::Right(bytes)) => {
                        for (&ty, bytes) in outs.iter().zip(bytes) {
                            match ty {
                                Type::BOOL => const_.push(IConst::Bool(bytes == 1)),
                                Type::U64 => const_.push(IConst::U64(bytes)),
                                Type::I64 => const_.push(IConst::I64(bytes as i64)),
                                Type::CHAR => const_.push(IConst::Char(bytes as u8 as char)),
                                ty => unreachable!("{:?}", ty),
                            }
                        }
                    }
                    _ => unreachable!(),
                }
            }
            Ok(Either::Left(_)) => unreachable!(),
        };

        self.consts.insert(name, ComConst::Compiled(const_.clone()));
        const_
    }

    fn compile_mem(&mut self, name: &String) {
        let mem = match self.mems.get(name) {
            Some(ComMem::Compiled(_)) => return,
            Some(ComMem::NotCompiled(c)) => c.clone(),
            None => unreachable!(),
        };
        let Mem { body, span: _ } = mem;
        let mut com = Self::with_consts_and_strings(self.consts.clone(), self.strings.clone());
        com.compile_body(body.clone());
        self.consts = com.consts;
        self.strings = com.strings;
        let ops = com.result;
        let size;
        match eval(ops, &self.strings) {
            Ok(Either::Right(bytes)) => size = bytes[0] as usize,
            Err(req) => {
                self.compile_const(req);
                let mut com =
                    Self::with_consts_and_strings(self.consts.clone(), self.strings.clone());
                com.compile_body(body);
                com.emit(Exit);
                let ops = com.result;
                self.consts = com.consts;
                self.strings = com.strings;
                match eval(ops, &self.strings) {
                    Ok(Either::Right(bytes)) => size = bytes[0] as usize,
                    _ => unreachable!(),
                }
            }
            Ok(Either::Left(_)) => unreachable!(),
        };
        self.mems.insert(name.clone(), ComMem::Compiled(size));
    }

    fn compile_body(&mut self, body: Vec<HirNode>) {
        for node in body {
            match node.hir {
                HirKind::Cond(cond) => self.compile_cond(cond),
                HirKind::Return => {
                    let num_bindings = self.bindings.iter().flatten().count();
                    for _ in 0..num_bindings {
                        self.emit(Unbind)
                    }
                    let i = self.local_vars_size;
                    self.emit(FreeLocals(i));
                    self.emit(Return)
                }
                HirKind::Literal(c) => match c {
                    IConst::Str(s) => {
                        let i = self.strings.len();
                        self.strings.push(s);
                        self.emit(PushStr(i));
                    }
                    _ => self.emit(Push(c)),
                },
                HirKind::Word(w) if self.is_const(&w) => {
                    let c = self.compile_const(w);
                    for c in c {
                        self.emit(Push(c))
                    }
                }
                HirKind::Word(w) if self.is_mem(&w) => {
                    self.compile_mem(&w);
                    self.emit(PushMem(w))
                }
                HirKind::Word(w) if self.is_binding(&w) => {
                    let offset = self
                        .bindings
                        .iter()
                        .flatten()
                        .rev()
                        .position(|s| s == &w)
                        .unwrap();
                    self.emit(UseBinding(offset))
                }
                HirKind::Word(w) if self.is_lvar(&w) => {
                    let &(offset, ref var) = &self.local_vars[&w];
                    if var.escaping {
                        self.emit(PushEscaping(offset))
                    } else {
                        self.emit(PushLvar(offset))
                    }
                }
                HirKind::Word(w) if self.is_gvar(&w) => self.emit(PushMem(w)),
                HirKind::Word(w) => {
                    let mangled = self.mangle_table.get(&w).unwrap().clone();
                    self.emit(Call(mangled))
                }
                HirKind::Intrinsic(i) => match i {
                    Intrinsic::Drop => self.emit(Drop),
                    Intrinsic::Dup => self.emit(Dup),
                    Intrinsic::Swap => self.emit(Swap),
                    Intrinsic::Over => self.emit(Over),

                    Intrinsic::Cast(_) => (), // this is a noop

                    Intrinsic::ReadU64 => self.emit(ReadU64),
                    Intrinsic::ReadU8 => self.emit(ReadU8),
                    Intrinsic::WriteU64 => self.emit(WriteU64),
                    Intrinsic::WriteU8 => self.emit(WriteU8),

                    Intrinsic::Add => self.emit(Add),
                    Intrinsic::Sub => self.emit(Sub),
                    Intrinsic::Divmod => self.emit(Divmod),
                    Intrinsic::Mul => self.emit(Mul),

                    Intrinsic::Eq => self.emit(Eq),
                    Intrinsic::Ne => self.emit(Ne),
                    Intrinsic::Lt => self.emit(Lt),
                    Intrinsic::Le => self.emit(Le),
                    Intrinsic::Gt => self.emit(Gt),
                    Intrinsic::Ge => self.emit(Ge),

                    Intrinsic::Dump => self.emit(Dump),
                    Intrinsic::Print => self.emit(Print),

                    Intrinsic::Syscall0 => self.emit(Syscall0),
                    Intrinsic::Syscall1 => self.emit(Syscall1),
                    Intrinsic::Syscall2 => self.emit(Syscall2),
                    Intrinsic::Syscall3 => self.emit(Syscall3),
                    Intrinsic::Syscall4 => self.emit(Syscall4),
                    Intrinsic::Syscall5 => self.emit(Syscall5),
                    Intrinsic::Syscall6 => self.emit(Syscall6),

                    Intrinsic::Argc => self.emit(Argc),
                    Intrinsic::Argv => self.emit(Argv),

                    Intrinsic::CompStop => return,
                },
                HirKind::If(cond) => self.compile_if(cond),
                HirKind::While(while_) => self.compile_while(while_),
                HirKind::Bind(bind) => self.compile_bind(bind),
                HirKind::IgnorePattern => unreachable!(), // this is a noop
                HirKind::FieldAccess(f) => {
                    let struct_ = &self.structs[f.ty.unwrap()];
                    let offset = struct_.fields[&f.field].offset;
                    self.emit(Push(IConst::U64(offset as _)));
                    self.emit(Add);
                }
            }
        }
    }

    fn compile_bind(&mut self, bind: Bind) {
        let mut new_bindings = Vec::new();
        for binding in bind.bindings.iter().rev() {
            match binding {
                Binding::Ignore => self.emit(Drop),
                Binding::Bind { name, ty: _ } => {
                    new_bindings.push(name.clone());
                    self.emit(Bind)
                }
            }
        }
        self.bindings.push(new_bindings);
        self.compile_body(bind.body);
        for binding in bind.bindings.into_iter().rev() {
            match binding {
                Binding::Ignore => (),
                Binding::Bind { name: _, ty: _ } => self.emit(Unbind),
            }
        }
        self.bindings.pop();
    }

    fn compile_while(&mut self, while_: While) {
        let cond_label = self.gen_label();
        let end_label = self.gen_label();
        self.emit(Label(cond_label.clone()));
        self.compile_body(while_.cond);
        self.emit(JumpF(end_label.clone()));
        self.compile_body(while_.body);
        self.emit(Jump(cond_label));
        self.emit(Label(end_label))
    }

    fn compile_if(&mut self, if_: If) {
        let lie_label = self.gen_label();
        let mut end_label = None;
        self.emit(JumpF(lie_label.clone()));

        self.compile_body(if_.truth);
        if if_.lie.is_some() {
            end_label = self.gen_label().some();
            self.emit(Jump(end_label.clone().unwrap()))
        }

        self.emit(Label(lie_label));

        if let Some(lie) = if_.lie {
            self.compile_body(lie);
            self.emit(Label(end_label.unwrap()))
        }
    }

    fn compile_cond(&mut self, cond: Cond) {
        let phi_label = self.gen_label();
        let num_branches = cond.branches.len() - 1;
        let mut this_branch_label = self.gen_label();
        let mut next_branch_label = self.gen_label();
        for (i, CondBranch { pattern, body }) in cond.branches.into_iter().enumerate() {
            if i != 0 {
                self.emit(Label(this_branch_label));
            }

            self.emit(Dup);
            match pattern.hir {
                HirKind::Literal(c) => self.emit(Push(c)),
                HirKind::Word(w) if self.is_const(&w) => {
                    let c = self.compile_const(w)[0].clone();
                    self.emit(Push(c))
                }
                HirKind::Word(w) => unreachable!("Impossible non-constant: {}", w),
                HirKind::IgnorePattern => self.emit(Dup), // todo: this is hacky
                _ => unreachable!(),
            }
            self.emit(Eq);
            if i < num_branches {
                self.emit(JumpF(next_branch_label.clone()));
            }
            this_branch_label = next_branch_label;
            next_branch_label = self.gen_label();
            self.compile_body(body);
            self.emit(Jump(phi_label.clone()));
        }

        self.emit(Label(phi_label))
    }

    fn emit(&mut self, op: Op) {
        self.result.push(op)
    }

    fn gen_label(&mut self) -> String {
        let res = format!(".{}_{}", self.current_name, self.label);
        self.label += 1;
        res
    }

    pub fn new(structs: StructIndex) -> Self {
        Self {
            label: 0,
            mangle_table: Default::default(),
            proc_id: 0,
            current_name: "".to_string(),
            result: Default::default(),
            consts: Default::default(),
            strings: Default::default(),
            bindings: Default::default(),
            mems: Default::default(),
            vars: Default::default(),
            local_vars: Default::default(),
            local_vars_size: Default::default(),
            escaping_size: Default::default(),
            structs,
        }
    }
    fn with_consts_and_strings(consts: FnvHashMap<String, ComConst>, strings: Vec<String>) -> Self {
        Self {
            label: 0,
            mangle_table: Default::default(),
            proc_id: 0,
            current_name: "".to_string(),
            result: Default::default(),
            consts,
            strings,
            bindings: Default::default(),
            mems: Default::default(),
            vars: Default::default(),
            local_vars: Default::default(),
            local_vars_size: Default::default(),
            escaping_size: Default::default(),
            structs: Default::default(),
        }
    }

    fn mangle_name(&mut self, name: String) -> String {
        let name_mangled = if name != "main" {
            format!(
                "proc{}_{}",
                self.proc_id,
                name.replace(|p: char| !p.is_ascii_alphabetic(), "")
            )
        } else {
            name.clone()
        };
        self.mangle_table.insert(name, name_mangled.clone());
        self.proc_id += 1;
        name_mangled
    }

    fn is_const(&self, w: &str) -> bool {
        self.consts.contains_key(w)
    }
    fn is_binding(&self, w: &str) -> bool {
        self.bindings.iter().flatten().any(|n| n == w)
    }
    fn is_mem(&self, w: &str) -> bool {
        self.mems.contains_key(w)
    }
    fn is_gvar(&self, w: &str) -> bool {
        self.vars.contains_key(w)
    }
    fn is_lvar(&self, w: &str) -> bool {
        self.local_vars.contains_key(w)
    }
}
