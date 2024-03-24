use fnv::FnvHashMap;
use rotth_parser::{hir, types::Primitive};
use smol_str::SmolStr;
use std::fmt::Write;

use crate::{
    concrete_error, error,
    inference::{ReifiedType, TermId},
    tir::{
        Bind, Cond, CondBranch, FieldAccess, GenId, If, TirNode, Type, TypecheckedProgram, TypedIr,
        TypedNode, Var, While,
    },
    Error, ErrorKind,
};
use itempath::{path, ItemPathBuf};

#[derive(Debug)]
pub enum ConcreteError {
    MainWithInputs,
    IncorrectMainOutputs,
    GenericMain,
    NoEntry,
    IncompleteProcedure(ItemPathBuf),
    IncompleteConst(ItemPathBuf),
    IncompleteMem(ItemPathBuf),
    IncompleteVar(ItemPathBuf),
}

#[derive(Debug, Clone)]
pub enum Intrinsic {
    Drop,
    Dup,
    Swap,
    Over,

    Cast(ReifiedType),

    Read(ReifiedType),
    Write(ReifiedType),

    CompStop,
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
}

#[derive(Debug, Clone)]
pub struct CConst {
    pub outs: Vec<ReifiedType>,
    pub body: Vec<ConcreteNode>,
}
#[derive(Debug, Clone)]
pub struct CMem {
    pub body: Vec<ConcreteNode>,
}

#[derive(Debug)]
pub struct ConcreteProgram {
    pub procs: FnvHashMap<ItemPathBuf, CProc>,
    pub consts: FnvHashMap<ItemPathBuf, CConst>,
    pub mems: FnvHashMap<ItemPathBuf, CMem>,
    pub vars: FnvHashMap<ItemPathBuf, Var<ReifiedType>>,
    // pub structs: FnvHashMap<TypeId, CStruct>,
}

fn substitutions(subs: &mut FnvHashMap<GenId, ReifiedType>, g: &Type, c: &ReifiedType) {
    match (g, c) {
        (Type::Generic(g), c) => {
            subs.insert(*g, c.clone());
        }
        (Type::Concrete(_), _) => {}
        (Type::Ptr(box g), _) => substitutions(subs, g, c),
    }
}

#[derive(Debug, Clone)]
pub struct CStruct {
    pub fields: FnvHashMap<SmolStr, Field>,
}

impl CStruct {
    pub fn size(&self) -> usize {
        self.fields.values().map(|f| f.ty.size()).sum()
    }
}

#[derive(Debug, Clone)]
pub struct Field {
    pub ty: ReifiedType,
    pub offset: usize,
}
pub type ConcreteNode = TypedNode<ReifiedType, Intrinsic>;
#[derive(Debug, Clone)]
pub struct CProc {
    pub generics: FnvHashMap<SmolStr, GenId>,
    pub vars: FnvHashMap<ItemPathBuf, ReifiedType>,
    pub ins: Vec<ReifiedType>,
    pub outs: Vec<ReifiedType>,
    pub body: Vec<ConcreteNode>,
}

#[derive(Default)]
pub struct Walker {
    procs: FnvHashMap<ItemPathBuf, Option<CProc>>,
    // structs: FnvHashMap<TypeId, CStruct>,
    consts: FnvHashMap<ItemPathBuf, Option<CConst>>,
    mems: FnvHashMap<ItemPathBuf, Option<CMem>>,
    vars: FnvHashMap<ItemPathBuf, Option<Var<ReifiedType>>>,
}

impl Walker {
    pub fn walk(program: TypecheckedProgram) -> Result<ConcreteProgram, Error> {
        if let Some(main) = program.procs.get(&path!(main)) {
            if !main.generics.is_empty() {
                return error(
                    main.span,
                    ErrorKind::Concrete(ConcreteError::GenericMain),
                    "Main cannot be a generic proc",
                );
            }
            if !main.ins.is_empty() {
                return error(
                    main.span,
                    ErrorKind::Concrete(ConcreteError::MainWithInputs),
                    "Main cannot have any inputs",
                );
            }
            if main.outs.len() != 1
                && matches!(
                    program.engine.reify(&Default::default(), main.outs[0]),
                    Ok(ReifiedType::Primitive(Primitive::U64))
                )
            {
                return error(
                    main.span,
                    ErrorKind::Concrete(ConcreteError::IncorrectMainOutputs),
                    "Main must be have one output of type u64",
                );
            }
            let mut this = Self::default();

            let main = this.walk_proc(&program, &path!(main), &Default::default())?;
            this.procs.insert(path!(main), Some(main));
            let procs = this
                .procs
                .into_iter()
                .map(|(p, i)| {
                    if let Some(i) = i {
                        Ok((p, i))
                    } else {
                        concrete_error(
                            None,
                            ConcreteError::IncompleteProcedure(p),
                            "Main can not be a generic proc",
                        )
                    }
                })
                .collect::<Result<_, _>>()?;
            let consts = this
                .consts
                .into_iter()
                .map(|(p, i)| {
                    if let Some(i) = i {
                        Ok((p, i))
                    } else {
                        concrete_error(
                            None,
                            ConcreteError::IncompleteConst(p),
                            "Main can not be a generic proc",
                        )
                    }
                })
                .collect::<Result<_, _>>()?;

            let mems = this
                .mems
                .into_iter()
                .map(|(p, i)| {
                    if let Some(i) = i {
                        Ok((p, i))
                    } else {
                        concrete_error(
                            None,
                            ConcreteError::IncompleteMem(p),
                            "Main can not be a generic proc",
                        )
                    }
                })
                .collect::<Result<_, _>>()?;

            let vars = this
                .vars
                .into_iter()
                .map(|(p, i)| {
                    if let Some(i) = i {
                        Ok((p, i))
                    } else {
                        concrete_error(
                            None,
                            ConcreteError::IncompleteVar(p),
                            "Main can not be a generic proc",
                        )
                    }
                })
                .collect::<Result<_, _>>()?;

            Ok(ConcreteProgram {
                procs,
                consts,
                mems,
                vars,
                // structs: this.structs,
            })
        } else {
            concrete_error(None, ConcreteError::NoEntry, "Couldn't find an entrypoint")
        }
    }

    fn walk_proc(
        &mut self,
        program: &TypecheckedProgram,
        path: &ItemPathBuf,
        gensubs: &FnvHashMap<GenId, ReifiedType>,
    ) -> Result<CProc, Error> {
        let proc = program.procs.get(path).unwrap();
        let body = self.walk_body(program, &proc.body, &proc.vars, gensubs)?;

        let vars = proc
            .vars
            .iter()
            .map(|(p, t)| Ok((p.clone(), program.engine.reify(gensubs, t.ty)?)))
            .collect::<Result<_, String>>();
        let vars = match vars {
            Ok(vars) => vars,
            Err(message) => {
                return error(
                    proc.span,
                    ErrorKind::UnificationError(message),
                    "Couldn't reify var types for this proc",
                )
            }
        };

        let ins = proc
            .ins
            .iter()
            .map(|t| program.engine.reify(gensubs, *t))
            .collect::<Result<_, String>>();
        let ins = match ins {
            Ok(ins) => ins,
            Err(message) => {
                return error(
                    proc.span,
                    ErrorKind::UnificationError(message),
                    "Couldn't reify ins types for this proc",
                )
            }
        };
        let outs = proc
            .outs
            .iter()
            .map(|t| program.engine.reify(gensubs, *t))
            .collect::<Result<_, String>>();
        let outs = match outs {
            Ok(outs) => outs,
            Err(message) => {
                return error(
                    proc.span,
                    ErrorKind::UnificationError(message),
                    "Couldn't reify outs types for this proc",
                )
            }
        };

        Ok(CProc {
            generics: proc.generics.clone(),
            vars,
            ins,
            outs,
            body,
        })
    }

    fn walk_body(
        &mut self,
        program: &TypecheckedProgram,
        body: &[TirNode],
        local_vars: &FnvHashMap<ItemPathBuf, Var<TermId>>,
        gensubs: &FnvHashMap<GenId, ReifiedType>,
    ) -> Result<Vec<ConcreteNode>, Error> {
        let mut concrete_body = Vec::new();
        for node in body {
            let node = self.walk_node(program, node, local_vars, gensubs)?;
            concrete_body.push(node)
        }
        Ok(concrete_body)
    }

    fn walk_node(
        &mut self,
        program: &TypecheckedProgram,
        node: &TirNode,
        local_vars: &FnvHashMap<ItemPathBuf, Var<TermId>>,
        gensubs: &FnvHashMap<GenId, ReifiedType>,
    ) -> Result<ConcreteNode, Error> {
        match &node.node {
            TypedIr::GVarUse(var_name) => {
                if let Some(_var) = program.vars.get(var_name) {
                    let ins = node
                        .ins
                        .iter()
                        .map(|t| program.engine.reify(gensubs, *t))
                        .collect::<Result<Vec<_>, _>>()
                        .unwrap();
                    let outs = node
                        .outs
                        .iter()
                        .map(|t| program.engine.reify(gensubs, *t))
                        .collect::<Result<Vec<_>, _>>()
                        .unwrap();

                    if !self.vars.contains_key(var_name) {
                        self.vars.insert(
                            var_name.clone(),
                            Some(Var {
                                ty: outs[0].clone(),
                            }),
                        );
                    }

                    Ok(TypedNode {
                        span: node.span,
                        node: TypedIr::GVarUse(var_name.clone()),
                        ins,
                        outs,
                    })
                } else {
                    unreachable!("")
                }
            }
            TypedIr::LVarUse(var_name) => {
                if let Some(_var) = local_vars.get(var_name) {
                    let ins = node
                        .ins
                        .iter()
                        .map(|t| program.engine.reify(gensubs, *t))
                        .collect::<Result<Vec<_>, _>>()
                        .unwrap();
                    let outs = node
                        .outs
                        .iter()
                        .map(|t| program.engine.reify(gensubs, *t))
                        .collect::<Result<Vec<_>, _>>()
                        .unwrap();

                    Ok(TypedNode {
                        span: node.span,
                        node: TypedIr::LVarUse(var_name.clone()),
                        ins,
                        outs,
                    })
                } else {
                    unreachable!("{:?}", var_name)
                }
            }
            TypedIr::MemUse(mem_name) => {
                let mem = program.mems.get(mem_name).unwrap();
                let ins = node
                    .ins
                    .iter()
                    .map(|t| program.engine.reify(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();
                let outs = node
                    .outs
                    .iter()
                    .map(|t| program.engine.reify(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();

                if !self.mems.contains_key(mem_name) {
                    self.mems.insert(mem_name.clone(), None);
                    let body: Vec<ConcreteNode> =
                        self.walk_body(program, &mem.body, local_vars, &Default::default())?;
                    let mem = CMem { body };
                    self.mems.insert(mem_name.clone(), Some(mem));
                }

                Ok(TypedNode {
                    span: node.span,
                    node: TypedIr::MemUse(mem_name.clone()),
                    ins,
                    outs,
                })
            }
            TypedIr::BindingUse(p) => {
                let ins = node
                    .ins
                    .iter()
                    .map(|t| program.engine.reify(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();
                let outs = node
                    .outs
                    .iter()
                    .map(|t| program.engine.reify(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();

                Ok(TypedNode {
                    span: node.span,
                    node: TypedIr::BindingUse(p.clone()),
                    ins,
                    outs,
                })
            }
            TypedIr::ConstUse(const_name) => {
                let const_ = program.consts.get(const_name).unwrap();

                let ins = node
                    .ins
                    .iter()
                    .map(|t| program.engine.reify(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();

                let outs = node
                    .outs
                    .iter()
                    .map(|t| program.engine.reify(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();

                if !self.consts.contains_key(const_name) {
                    self.consts.insert(const_name.clone(), None);
                    let body: Vec<ConcreteNode> =
                        self.walk_body(program, &const_.body, local_vars, &Default::default())?;
                    let const_ = CConst {
                        outs: outs.clone(),
                        body,
                    };
                    self.consts.insert(const_name.clone(), Some(const_));
                }

                Ok(TypedNode {
                    span: node.span,
                    node: TypedIr::ConstUse(const_name.clone()),
                    ins,
                    outs,
                })
            }
            TypedIr::Call(p) => {
                let callee = program.procs.get(p).unwrap();

                let ins = node
                    .ins
                    .iter()
                    .map(|t| match program.engine.reify(gensubs, *t) {
                        Ok(t) => Ok(t),
                        Err(e) => error(
                            node.span,
                            ErrorKind::UnificationError(e),
                            format!("Failure resolving term {t:?}"),
                        ),
                    });

                let outs = node
                    .outs
                    .iter()
                    .map(|t| match program.engine.reify(gensubs, *t) {
                        Ok(t) => Ok(t),
                        Err(e) => error(
                            node.span,
                            ErrorKind::UnificationError(e),
                            format!("Failure resolving term {t:?}"),
                        ),
                    });

                let mut gensubs = FnvHashMap::default();
                let mut ins2 = Vec::new();
                let mut outs2 = Vec::new();
                for (generic, instantiated) in callee.ins.iter().zip(ins) {
                    let instantiated = instantiated?;
                    let generic = program.engine.reconstruct_lossy(*generic);
                    ins2.push(instantiated.clone());
                    substitutions(&mut gensubs, &generic, &instantiated)
                }
                for (generic, instantiated) in callee.outs.iter().zip(outs) {
                    let instantiated = instantiated?;
                    let generic = program.engine.reconstruct_lossy(*generic);
                    outs2.push(instantiated.clone());
                    substitutions(&mut gensubs, &generic, &instantiated)
                }

                let mut substr = String::new();
                let mut subs = gensubs.iter().collect::<Vec<_>>();
                subs.sort_by(|(i1, _), (i2, _)| i1.cmp(i2));
                for (_, sub) in subs {
                    write!(substr, "{sub:?}").unwrap();
                }
                let callee_name = if substr.is_empty() {
                    p.clone()
                } else {
                    p.child(substr)
                };

                if !self.procs.contains_key(&callee_name) {
                    self.procs.insert(callee_name.clone(), None);
                    let callee = self.walk_proc(program, p, &gensubs)?;
                    self.procs.insert(callee_name.clone(), Some(callee));
                }

                Ok(TypedNode {
                    span: node.span,
                    node: TypedIr::Call(callee_name),
                    ins: ins2,
                    outs: outs2,
                })
            }
            TypedIr::Intrinsic(i) => {
                let i = match i {
                    hir::Intrinsic::Cast(t) => {
                        let t = program.engine.reify(gensubs, t.inner).unwrap();
                        Intrinsic::Cast(t)
                    }
                    hir::Intrinsic::Read(t) => {
                        let t = program.engine.reify(gensubs, t.inner).unwrap();
                        Intrinsic::Read(t)
                    }
                    hir::Intrinsic::Write(t) => {
                        let t = program.engine.reify(gensubs, t.inner).unwrap();
                        Intrinsic::Write(t)
                    }
                    hir::Intrinsic::Drop => Intrinsic::Drop,
                    hir::Intrinsic::Dup => Intrinsic::Dup,
                    hir::Intrinsic::Swap => Intrinsic::Swap,
                    hir::Intrinsic::Over => Intrinsic::Over,
                    hir::Intrinsic::CompStop => Intrinsic::CompStop,
                    hir::Intrinsic::Dump => Intrinsic::Dump,
                    hir::Intrinsic::Print => Intrinsic::Print,
                    hir::Intrinsic::Syscall0 => Intrinsic::Syscall0,
                    hir::Intrinsic::Syscall1 => Intrinsic::Syscall1,
                    hir::Intrinsic::Syscall2 => Intrinsic::Syscall2,
                    hir::Intrinsic::Syscall3 => Intrinsic::Syscall3,
                    hir::Intrinsic::Syscall4 => Intrinsic::Syscall4,
                    hir::Intrinsic::Syscall5 => Intrinsic::Syscall5,
                    hir::Intrinsic::Syscall6 => Intrinsic::Syscall6,
                    hir::Intrinsic::Argc => Intrinsic::Argc,
                    hir::Intrinsic::Argv => Intrinsic::Argv,
                    hir::Intrinsic::Add => Intrinsic::Add,
                    hir::Intrinsic::Sub => Intrinsic::Sub,
                    hir::Intrinsic::Divmod => Intrinsic::Divmod,
                    hir::Intrinsic::Mul => Intrinsic::Mul,
                    hir::Intrinsic::Eq => Intrinsic::Eq,
                    hir::Intrinsic::Ne => Intrinsic::Ne,
                    hir::Intrinsic::Lt => Intrinsic::Lt,
                    hir::Intrinsic::Le => Intrinsic::Le,
                    hir::Intrinsic::Gt => Intrinsic::Gt,
                    hir::Intrinsic::Ge => Intrinsic::Ge,
                };
                let ins = node
                    .ins
                    .iter()
                    .map(|t| program.engine.reify(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();
                let outs = node
                    .outs
                    .iter()
                    .map(|t| program.engine.reify(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();

                Ok(TypedNode {
                    span: node.span,
                    node: TypedIr::Intrinsic(i),
                    ins,
                    outs,
                })
            }
            TypedIr::Bind(Bind { bindings, body }) => {
                let ins = node
                    .ins
                    .iter()
                    .map(|t| program.engine.reify(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();
                let outs = node
                    .outs
                    .iter()
                    .map(|t| program.engine.reify(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();
                let body: Vec<ConcreteNode> = self.walk_body(program, body, local_vars, gensubs)?;

                Ok(ConcreteNode {
                    span: node.span,
                    node: TypedIr::Bind(Bind {
                        bindings: bindings.clone(),
                        body,
                    }),
                    ins,
                    outs,
                })
            }
            TypedIr::While(While { cond, body }) => {
                let ins = node
                    .ins
                    .iter()
                    .map(|t| program.engine.reify(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();
                let outs = node
                    .outs
                    .iter()
                    .map(|t| program.engine.reify(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();
                let cond = self.walk_body(program, cond, local_vars, gensubs)?;
                let body = self.walk_body(program, body, local_vars, gensubs)?;

                Ok(TypedNode {
                    span: node.span,
                    node: TypedIr::While(While { cond, body }),
                    ins,
                    outs,
                })
            }
            TypedIr::If(If { truth, lie }) => {
                let ins = node
                    .ins
                    .iter()
                    .map(|t| program.engine.reify(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();
                let outs = node
                    .outs
                    .iter()
                    .map(|t| program.engine.reify(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();
                let truth = self.walk_body(program, truth, local_vars, gensubs)?;
                let lie = if let Some(lie) = lie {
                    Some(self.walk_body(program, lie, local_vars, gensubs)?)
                } else {
                    None
                };

                Ok(TypedNode {
                    span: node.span,
                    node: TypedIr::If(If { truth, lie }),
                    ins,
                    outs,
                })
            }
            TypedIr::Cond(Cond { branches }) => {
                let ins = node
                    .ins
                    .iter()
                    .map(|t| program.engine.reify(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();
                let outs = node
                    .outs
                    .iter()
                    .map(|t| program.engine.reify(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();
                let mut concrete_branches = Vec::new();
                for CondBranch { pattern, body } in branches {
                    let pattern = self.walk_node(program, pattern, local_vars, gensubs)?;
                    let body = self.walk_body(program, body, local_vars, gensubs)?;
                    concrete_branches.push(CondBranch { pattern, body })
                }
                Ok(TypedNode {
                    span: node.span,
                    node: TypedIr::Cond(Cond {
                        branches: concrete_branches,
                    }),
                    ins,
                    outs,
                })
            }
            TypedIr::Literal(l) => {
                let ins = node
                    .ins
                    .iter()
                    .map(|t| program.engine.reify(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();
                let outs = node
                    .outs
                    .iter()
                    .map(|t| program.engine.reify(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();
                Ok(TypedNode {
                    span: node.span,
                    node: TypedIr::Literal(l.clone()),
                    ins,
                    outs,
                })
            }
            TypedIr::IgnorePattern => {
                let ins = node
                    .ins
                    .iter()
                    .map(|t| program.engine.reify(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();
                let outs = node
                    .outs
                    .iter()
                    .map(|t| program.engine.reify(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();
                Ok(TypedNode {
                    span: node.span,
                    node: TypedIr::IgnorePattern,
                    ins,
                    outs,
                })
            }
            TypedIr::Return => {
                let ins = node
                    .ins
                    .iter()
                    .map(|t| program.engine.reify(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();
                let outs = node
                    .outs
                    .iter()
                    .map(|t| program.engine.reify(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();
                Ok(TypedNode {
                    span: node.span,
                    node: TypedIr::Return,
                    ins,
                    outs,
                })
            }
            TypedIr::FieldAccess(f) => {
                let ins = node
                    .ins
                    .iter()
                    .map(|t| program.engine.reify(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();
                let outs = node
                    .outs
                    .iter()
                    .map(|t| program.engine.reify(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();

                let ty = match &ins[0] {
                    ReifiedType::Ptr(box t) => t.clone(),
                    ty => todo!("{ty:?}"),
                };
                Ok(TypedNode {
                    span: node.span,
                    node: TypedIr::FieldAccess(FieldAccess {
                        ty,
                        field: f.field.clone(),
                    }),
                    ins,
                    outs,
                })
            }
        }
    }
}
