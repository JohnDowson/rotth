use fnv::FnvHashMap;
use rotth_parser::{path, types::Primitive};
use smol_str::SmolStr;
use somok::Somok;
use std::fmt::Write;

use crate::{
    inference::TermId,
    tir::{
        Bind, ConcreteType, Cond, CondBranch, FieldAccess, GenId, If, KConst, KMem, KProc, Type,
        TypeId, TypecheckedProgram, TypedIr, TypedNode, Var, While,
    },
    typecheck, Error,
};
use rotth_parser::ast::ItemPathBuf;

#[derive(Debug)]
pub enum ConcreteError {
    MainWithInputs,
    GenericMain,
    NoEntry,
    IncompleteProcedure(ItemPathBuf),
    IncompleteConst(ItemPathBuf),
}

#[derive(Debug)]
pub struct ConcreteProgram {
    pub procs: FnvHashMap<ItemPathBuf, KProc<TypedNode<Type>>>,
    pub consts: FnvHashMap<ItemPathBuf, KConst<Type>>,
    pub mems: FnvHashMap<ItemPathBuf, KMem<Type>>,
    pub vars: FnvHashMap<ItemPathBuf, Var<Type>>,
    pub structs: FnvHashMap<TypeId, CStruct>,
}

fn substitutions(subs: &mut FnvHashMap<GenId, Type>, g: &Type, c: &Type) {
    match (g, c) {
        (Type::Generic(g), c @ Type::Concrete(_)) => {
            subs.insert(*g, c.clone());
        }
        (Type::Concrete(g), Type::Concrete(c)) => {
            if let (ConcreteType::Ptr(box g), ConcreteType::Ptr(box c)) = (g, c) {
                substitutions(subs, g, c)
            }
        }
        (_, Type::Generic(_)) => unreachable!(),
    }
}

#[derive(Debug)]
pub struct CStruct {
    pub fields: FnvHashMap<SmolStr, Field>,
}

#[derive(Debug)]
pub struct Field {
    pub size: usize,
    pub offset: usize,
}

#[derive(Default)]
pub struct Walker {
    procs: FnvHashMap<ItemPathBuf, Option<KProc<TypedNode<Type>>>>,
    consts: FnvHashMap<ItemPathBuf, Option<KConst<Type>>>,
    mems: FnvHashMap<ItemPathBuf, Option<KMem<Type>>>,
    vars: FnvHashMap<ItemPathBuf, Option<Var<Type>>>,
}

impl Walker {
    pub fn walk(program: TypecheckedProgram) -> Result<ConcreteProgram, Error> {
        if let Some(main) = program.procs.get(&path!(main)) {
            if !main.generics.is_empty() {
                return Error::from(ConcreteError::GenericMain).error();
            }
            if !main.ins.is_empty() {
                return Error::from(ConcreteError::MainWithInputs).error();
            }
            let mut this = Self::default();

            let body = this.walk_body(&program, &main.body, &main.vars, &Default::default())?;
            let main = KProc {
                generics: Default::default(),
                vars: main.vars.clone(),
                span: main.span,
                ins: Default::default(),
                outs: main.outs.clone(),
                body,
            };
            this.procs.insert(path!(main), Some(main));
            let procs = this
                .procs
                .into_iter()
                .map(|(p, i)| {
                    if let Some(i) = i {
                        Ok((p, i))
                    } else {
                        Err(ConcreteError::IncompleteProcedure(p))
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
                        Err(ConcreteError::IncompleteConst(p))
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
                        Err(ConcreteError::IncompleteConst(p))
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
                        Err(ConcreteError::IncompleteConst(p))
                    }
                })
                .collect::<Result<_, _>>()?;

            let structs = program
                .structs
                .into_iter()
                .map(|(_, i)| {
                    (i.id, {
                        let mut offset = 0;
                        let mut fields = i
                            .fields
                            .into_iter()
                            .map(|(n, t)| {
                                let f = match t {
                                    Type::Generic(_) => todo!(),
                                    Type::Concrete(c) => match c {
                                        ConcreteType::Ptr(_) => 8,
                                        ConcreteType::Primitive(p) => match p {
                                            Primitive::Void => 0,
                                            Primitive::Bool => 1,
                                            Primitive::Char => 1,
                                            Primitive::U64 => 8,
                                            Primitive::U32 => 4,
                                            Primitive::U16 => 2,
                                            Primitive::U8 => 1,
                                            Primitive::I64 => 8,
                                            Primitive::I32 => 4,
                                            Primitive::I16 => 2,
                                            Primitive::I8 => 1,
                                        },
                                        ConcreteType::Custom(_) => todo!(),
                                    },
                                };
                                (n, f)
                            })
                            .collect::<Vec<_>>();
                        fields.sort_by_key(|(_, s)| *s);
                        let fields = fields
                            .into_iter()
                            .map(|(n, s)| {
                                let f = Field { size: s, offset };
                                offset += s;

                                (n, f)
                            })
                            .collect();

                        CStruct { fields }
                    })
                })
                .collect();

            Ok(ConcreteProgram {
                procs,
                consts,
                mems,
                vars,
                structs,
            })
        } else {
            Error::from(ConcreteError::NoEntry).error()
        }
    }

    fn walk_body(
        &mut self,
        program: &TypecheckedProgram,
        body: &[TypedNode<TermId>],
        local_vars: &FnvHashMap<ItemPathBuf, Var<Type>>,
        gensubs: &FnvHashMap<GenId, Type>,
    ) -> Result<Vec<TypedNode<Type>>, Error> {
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
        node: &TypedNode<TermId>,
        local_vars: &FnvHashMap<ItemPathBuf, Var<Type>>,
        gensubs: &FnvHashMap<GenId, Type>,
    ) -> Result<TypedNode<Type>, Error> {
        match &node.node {
            TypedIr::GVarUse(var_name) => {
                if let Some(_var) = program.vars.get(var_name) {
                    let ins = node
                        .ins
                        .iter()
                        .map(|t| program.engine.reconstruct_substituting(gensubs, *t))
                        .collect::<Result<Vec<_>, _>>()
                        .unwrap();
                    let outs = node
                        .outs
                        .iter()
                        .map(|t| program.engine.reconstruct_substituting(gensubs, *t))
                        .collect::<Result<Vec<_>, _>>()
                        .unwrap();

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
                        .map(|t| program.engine.reconstruct_substituting(gensubs, *t))
                        .collect::<Result<Vec<_>, _>>()
                        .unwrap();
                    let outs = node
                        .outs
                        .iter()
                        .map(|t| program.engine.reconstruct_substituting(gensubs, *t))
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
                    .map(|t| program.engine.reconstruct_substituting(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();
                let outs = node
                    .outs
                    .iter()
                    .map(|t| program.engine.reconstruct_substituting(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();

                if !self.mems.contains_key(mem_name) {
                    self.mems.insert(mem_name.clone(), None);
                    let body: Vec<TypedNode<Type>> =
                        self.walk_body(program, &mem.body, local_vars, &Default::default())?;
                    let mem = KMem {
                        span: mem.span,
                        body,
                    };
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
                    .map(|t| program.engine.reconstruct_substituting(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();
                let outs = node
                    .outs
                    .iter()
                    .map(|t| program.engine.reconstruct_substituting(gensubs, *t))
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
                    .map(|t| program.engine.reconstruct_substituting(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();
                let outs = node
                    .outs
                    .iter()
                    .map(|t| program.engine.reconstruct_substituting(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();

                if !self.consts.contains_key(const_name) {
                    self.consts.insert(const_name.clone(), None);
                    let body: Vec<TypedNode<Type>> =
                        self.walk_body(program, &const_.body, local_vars, &Default::default())?;
                    let const_ = KConst {
                        outs: const_.outs.clone(),
                        span: const_.span,
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

                let ins = node.ins.iter().map(|t| {
                    match program.engine.reconstruct_substituting(gensubs, *t) {
                        Ok(t) => Ok(t),
                        Err(e) => typecheck::error(
                            node.span,
                            typecheck::ErrorKind::UnificationError(e),
                            format!("Failure resolving term {t:?}"),
                        ),
                    }
                });

                let outs = node.outs.iter().map(|t| {
                    match program.engine.reconstruct_substituting(gensubs, *t) {
                        Ok(t) => Ok(t),
                        Err(e) => typecheck::error(
                            node.span,
                            typecheck::ErrorKind::UnificationError(e),
                            format!("Failure resolving term {t:?}"),
                        ),
                    }
                });

                let mut gensubs = FnvHashMap::default();
                let mut ins2 = Vec::new();
                let mut outs2 = Vec::new();
                for (generic, instantiated) in callee.ins.iter().zip(ins) {
                    let instantiated = instantiated?;
                    ins2.push(instantiated.clone());
                    substitutions(&mut gensubs, generic, &instantiated)
                }
                for (generic, instantiated) in callee.outs.iter().zip(outs) {
                    let instantiated = instantiated?;
                    outs2.push(instantiated.clone());
                    substitutions(&mut gensubs, generic, &instantiated)
                }

                let mut substr = String::new();
                let mut subs = gensubs.iter().collect::<Vec<_>>();
                subs.sort_by(|(i1, _), (i2, _)| i1.cmp(i2));
                for (_, sub) in subs {
                    write!(substr, "{:?}", sub).unwrap();
                }
                let callee_name = if substr.is_empty() {
                    p.clone()
                } else {
                    p.child(substr)
                };

                if !self.procs.contains_key(&callee_name) {
                    self.procs.insert(callee_name.clone(), None);
                    let body: Vec<TypedNode<Type>> =
                        self.walk_body(program, &callee.body, &callee.vars, &gensubs)?;
                    let callee = KProc {
                        generics: Default::default(),
                        vars: callee.vars.clone(),
                        span: callee.span,
                        ins: ins2.clone(),
                        outs: outs2.clone(),
                        body,
                    };
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
                let ins = node
                    .ins
                    .iter()
                    .map(|t| program.engine.reconstruct_substituting(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();
                let outs = node
                    .outs
                    .iter()
                    .map(|t| program.engine.reconstruct_substituting(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();

                Ok(TypedNode {
                    span: node.span,
                    node: TypedIr::Intrinsic(i.clone()),
                    ins,
                    outs,
                })
            }
            TypedIr::Bind(Bind { bindings, body }) => {
                let ins = node
                    .ins
                    .iter()
                    .map(|t| program.engine.reconstruct_substituting(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();
                let outs = node
                    .outs
                    .iter()
                    .map(|t| program.engine.reconstruct_substituting(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();
                let body = self.walk_body(program, body, local_vars, gensubs)?;

                Ok(TypedNode {
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
                    .map(|t| program.engine.reconstruct_substituting(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();
                let outs = node
                    .outs
                    .iter()
                    .map(|t| program.engine.reconstruct_substituting(gensubs, *t))
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
                    .map(|t| program.engine.reconstruct_substituting(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();
                let outs = node
                    .outs
                    .iter()
                    .map(|t| program.engine.reconstruct_substituting(gensubs, *t))
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
                    .map(|t| program.engine.reconstruct_substituting(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();
                let outs = node
                    .outs
                    .iter()
                    .map(|t| program.engine.reconstruct_substituting(gensubs, *t))
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
                    .map(|t| program.engine.reconstruct_substituting(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();
                let outs = node
                    .outs
                    .iter()
                    .map(|t| program.engine.reconstruct_substituting(gensubs, *t))
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
                    .map(|t| program.engine.reconstruct_substituting(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();
                let outs = node
                    .outs
                    .iter()
                    .map(|t| program.engine.reconstruct_substituting(gensubs, *t))
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
                    .map(|t| program.engine.reconstruct_substituting(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();
                let outs = node
                    .outs
                    .iter()
                    .map(|t| program.engine.reconstruct_substituting(gensubs, *t))
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
                    .map(|t| program.engine.reconstruct_substituting(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();
                let outs = node
                    .outs
                    .iter()
                    .map(|t| program.engine.reconstruct_substituting(gensubs, *t))
                    .collect::<Result<Vec<_>, _>>()
                    .unwrap();

                let ty = match &ins[0] {
                    Type::Concrete(ConcreteType::Ptr(box t)) => t.clone(),
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
