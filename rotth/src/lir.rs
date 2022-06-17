use fnv::FnvHashMap;
use rotth_parser::{
    ast::{ItemPath, ItemPathBuf, Literal},
    hir::{Binding, Intrinsic},
    types::Primitive,
};
use somok::{Either, Somok};
use Op::*;

use crate::{
    ctir::{CProc, ConcreteProgram},
    eval::eval,
    inference::ReifiedType,
    tir::{self, Cond, CondBranch, FieldAccess, If, KConst, KMem, Type, TypedIr, TypedNode, While},
};

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

    Proc(Mangled),
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
    NotCompiled(KConst<ReifiedType>),
}

#[derive(Clone)]
pub enum ComMem {
    Compiled(usize),
    NotCompiled(KMem<ReifiedType>),
}

#[derive(Clone)]
pub enum ComVar {
    Compiled(usize),
    NotCompiled(ReifiedType),
}

#[derive(Default)]
pub struct Compiler {
    label: usize,
    mangle_table: FnvHashMap<ItemPathBuf, Mangled>,
    unmangle_table: FnvHashMap<Mangled, ItemPathBuf>,
    proc_id: usize,
    current_name: Mangled,
    result: Vec<Op>,
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
    pub fn compile(program: ConcreteProgram) -> (Vec<Op>, Vec<String>, FnvHashMap<Mangled, usize>) {
        let mut this = Self::default();

        this.emit(Call(Mangled("main".into())));
        this.emit(Exit);

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
        let label = name;
        self.emit(Proc(label));

        let mut i = 0;
        for (name, var) in proc.vars {
            let offset = var.size();
            self.local_vars.insert(name, (i, var));
            i += offset
        }
        self.local_vars_size = i;
        if i > 0 {
            self.emit(ReserveLocals(i));
        }

        self.compile_body(proc.body);

        self.local_vars = Default::default();

        if i > 0 {
            self.emit(FreeLocals(i));
        }
        self.emit(Return);
    }

    fn compile_const(&mut self, name: &ItemPath) -> Vec<Literal> {
        let const_ = match self.consts.get(name) {
            Some(ComConst::Compiled(c)) => return c.clone(),
            Some(ComConst::NotCompiled(c)) => c.clone(),
            None => todo!(),
        };
        let KConst {
            outs,
            span: _,
            body,
        } = const_;
        let mut com = Self::with_consts_and_strings(self.consts.clone(), self.strings.clone());
        com.compile_body(body.clone());
        self.consts = com.consts;
        self.strings = com.strings;
        let ops = com.result;
        let mut const_ = Vec::new();
        match eval(ops, &self.strings, false) {
            Ok(Either::Right(bytes)) => {
                for (ty, bytes) in outs.iter().zip(bytes) {
                    match ty {
                        Type::Generic(_) => unreachable!(),
                        Type::Concrete(ty) => match ty {
                            tir::ConcreteType::Ptr(_) => todo!(),
                            tir::ConcreteType::Primitive(ty) => match ty {
                                Primitive::Bool => const_.push(Literal::Bool(bytes == 1)),
                                Primitive::U64 => const_.push(Literal::Num(bytes)),
                                Primitive::Char => const_.push(Literal::Char(bytes as u8 as char)),
                                _ => todo!(),
                            },
                            tir::ConcreteType::Custom(_) => todo!(),
                        },
                    }
                }
            }
            Err(req) => {
                let c_name = self.unmangle_table.get(&req).unwrap().clone();
                self.compile_const(&c_name);
                let mut com =
                    Self::with_consts_and_strings(self.consts.clone(), self.strings.clone());
                com.compile_body(body);
                com.emit(Exit);
                let ops = com.result;
                self.consts = com.consts;
                self.strings = com.strings;
                match eval(ops, &self.strings, false) {
                    Ok(Either::Right(bytes)) => {
                        for (ty, bytes) in outs.iter().zip(bytes) {
                            match ty {
                                Type::Generic(_) => unreachable!(),
                                Type::Concrete(ty) => match ty {
                                    tir::ConcreteType::Ptr(_) => todo!(),
                                    tir::ConcreteType::Primitive(ty) => match ty {
                                        Primitive::Bool => const_.push(Literal::Bool(bytes == 1)),
                                        Primitive::U64 => const_.push(Literal::Num(bytes)),
                                        Primitive::Char => {
                                            const_.push(Literal::Char(bytes as u8 as char))
                                        }
                                        _ => todo!(),
                                    },
                                    tir::ConcreteType::Custom(_) => todo!(),
                                },
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
        let KMem { body, span: _ } = mem;
        let mut com = Self::with_consts_and_strings(self.consts.clone(), self.strings.clone());
        com.compile_body(body.clone());
        self.consts = com.consts;
        self.strings = com.strings;
        let ops = com.result;
        let size;
        match eval(ops, &self.strings, false) {
            Ok(Either::Right(bytes)) => size = bytes[0] as usize,
            Err(req) => {
                let m_name = self.unmangle_table.get(&req).unwrap().clone();
                self.compile_const(&m_name);
                let mut com =
                    Self::with_consts_and_strings(self.consts.clone(), self.strings.clone());
                com.compile_body(body);
                com.emit(Exit);
                let ops = com.result;
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

    fn compile_body(&mut self, body: Vec<TypedNode<ReifiedType>>) {
        for node in body {
            match node.node {
                TypedIr::Cond(cond) => self.compile_cond(cond),
                TypedIr::Return => {
                    let num_bindings = self.bindings.iter().flatten().count();
                    for _ in 0..num_bindings {
                        self.emit(Unbind)
                    }
                    let i = self.local_vars_size;
                    self.emit(FreeLocals(i));
                    self.emit(Return)
                }
                TypedIr::Literal(c) => match c {
                    Literal::String(s) => {
                        let i = self.strings.len();
                        self.strings.push(s);
                        self.emit(PushStr(i));
                    }
                    _ => self.emit(Push(c)),
                },
                TypedIr::ConstUse(w) => {
                    let c = self.compile_const(&w);
                    for c in c {
                        self.emit(Push(c))
                    }
                }
                TypedIr::MemUse(mem_name) => {
                    self.compile_mem(&mem_name);
                    let mangled = self.mangle_table.get(&mem_name).unwrap().clone();
                    self.emit(PushMem(mangled))
                }
                TypedIr::GVarUse(var_name) => {
                    let mangled = self.mangle_table.get(&var_name).unwrap().clone();
                    self.emit(PushMem(mangled))
                }
                TypedIr::LVarUse(var_name) => {
                    let lvar = self.local_vars.get(&var_name).unwrap().0;
                    self.emit(Op::PushLvar(lvar))
                }
                TypedIr::BindingUse(w) => {
                    let offset = self
                        .bindings
                        .iter()
                        .flatten()
                        .rev()
                        .position(|s| s == &w)
                        .unwrap();
                    self.emit(UseBinding(offset))
                }
                TypedIr::Call(w) => {
                    let mangled = self.mangle_table.get(&w).unwrap().clone();
                    self.emit(Call(mangled))
                }
                TypedIr::Intrinsic(i) => match i {
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
                TypedIr::If(cond) => self.compile_if(cond),
                TypedIr::While(while_) => self.compile_while(while_),
                TypedIr::Bind(bind) => self.compile_bind(bind),
                TypedIr::IgnorePattern => unreachable!(), // this is a noop
                TypedIr::FieldAccess(FieldAccess { ty, field }) => {
                    let ty = if let ReifiedType::Custom(ty) = ty {
                        ty
                    } else {
                        todo!("{ty:?}")
                    };
                    let offset = ty.fields[&field].offset;
                    self.emit(Push(Literal::Num(offset as _)));
                    self.emit(Add);
                }
            }
        }
    }

    fn compile_bind(&mut self, bind: tir::Bind<ReifiedType>) {
        let mut new_bindings = Vec::new();
        for binding in bind.bindings.iter().rev() {
            match &binding.inner {
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
            match binding.inner {
                Binding::Ignore => (),
                Binding::Bind { name: _, ty: _ } => self.emit(Unbind),
            }
        }
        self.bindings.pop();
    }

    fn compile_while(&mut self, while_: While<ReifiedType>) {
        let cond_label = self.gen_label();
        let end_label = self.gen_label();
        self.emit(Label(cond_label.clone()));
        self.compile_body(while_.cond);
        self.emit(JumpF(end_label.clone()));
        self.compile_body(while_.body);
        self.emit(Jump(cond_label));
        self.emit(Label(end_label))
    }

    fn compile_if(&mut self, if_: If<ReifiedType>) {
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

    fn compile_cond(&mut self, cond: Cond<ReifiedType>) {
        let phi_label = self.gen_label();
        let num_branches = cond.branches.len() - 1;
        let mut this_branch_label = self.gen_label();
        let mut next_branch_label = self.gen_label();
        for (i, CondBranch { pattern, body }) in cond.branches.into_iter().enumerate() {
            if i != 0 {
                self.emit(Label(this_branch_label));
            }

            self.emit(Dup);
            match pattern.node {
                TypedIr::Literal(c) => self.emit(Push(c)),
                TypedIr::ConstUse(w) => {
                    let c = self.compile_const(&w)[0].clone();
                    self.emit(Push(c))
                }
                TypedIr::IgnorePattern => self.emit(Dup), // todo: this is hacky
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
        Self {
            label: 0,
            mangle_table: Default::default(),
            unmangle_table: Default::default(),
            proc_id: 0,
            current_name: Default::default(),
            result: Default::default(),
            consts,
            strings,
            bindings: Default::default(),
            mems: Default::default(),
            local_vars: Default::default(),
            local_vars_size: Default::default(),
            _escaping_size: Default::default(),
            // structs: Default::default(),
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
