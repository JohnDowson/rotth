use fnv::FnvHashMap;
use rotth_parser::{
    ast::{ItemPath, ItemPathBuf, Literal},
    hir::{self, FieldAccess, Hir, Intrinsic},
    types::{self, Primitive, StructIndex},
};
use smol_str::SmolStr;
use somok::Somok;
use spanner::{Span, Spanned};

use crate::{
    inference::{type_to_info, Engine, TermId, TypeInfo},
    typecheck::{self, error, ErrorKind, THeap, TypeStack},
    Error,
};

#[derive(Debug, Clone)]
pub enum TopLevel {
    Proc(Vec<TirNode>),
    Const(Vec<TirNode>),
    Mem(),
    Var(),
}
#[derive(Clone)]
pub struct TirNode {
    pub span: Span,
    pub node: Tir,
    pub ins: Vec<TermId>,
    pub outs: Vec<TermId>,
}

impl std::fmt::Debug for TirNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            write!(
                f,
                "{:#?}@{:#?} {:?}:{:?}",
                &self.node, &self.span, &self.ins, &self.outs
            )
        } else {
            write!(
                f,
                "{:?}@{:?} {:?}:{:?}",
                &self.node, &self.span, &self.ins, &self.outs
            )
        }
    }
}

#[derive(Debug, Clone)]
pub struct Bind {
    pub bindings: Vec<Spanned<hir::Binding>>,
    pub body: Vec<TirNode>,
}
#[derive(Debug, Clone)]
pub struct While {
    pub cond: Vec<TirNode>,
    pub body: Vec<TirNode>,
}
#[derive(Debug, Clone)]
pub struct If {
    pub truth: Vec<TirNode>,
    pub lie: Option<Vec<TirNode>>,
}
#[derive(Debug, Clone)]
pub struct Cond {
    pub branches: Vec<CondBranch>,
}
#[derive(Debug, Clone)]
pub struct CondBranch {
    pub pattern: TirNode,
    pub body: Vec<TirNode>,
}

#[derive(Clone)]
pub enum Tir {
    BindingUse(ItemPathBuf),
    ConstUse(ItemPathBuf),
    Call(ItemPathBuf),
    Intrinsic(hir::Intrinsic),
    Bind(Bind),
    While(While),
    If(If),
    Cond(Cond),
    Literal(Literal),
    IgnorePattern,
    Return,
    FieldAccess(FieldAccess),
}

impl std::fmt::Debug for Tir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BindingUse(arg0) => f.debug_tuple("BindingUse").field(arg0).finish(),
            Self::ConstUse(arg0) => f.debug_tuple("ConstUse").field(arg0).finish(),
            Self::Call(arg0) => f.debug_tuple("Call").field(arg0).finish(),
            Self::Intrinsic(arg0) => arg0.fmt(f),
            Self::Bind(arg0) => arg0.fmt(f),
            Self::While(arg0) => arg0.fmt(f),
            Self::If(arg0) => arg0.fmt(f),
            Self::Cond(arg0) => arg0.fmt(f),
            Self::Literal(arg0) => arg0.fmt(f),
            Self::IgnorePattern => write!(f, "_"),
            Self::Return => write!(f, "return"),
            Self::FieldAccess(arg0) => arg0.fmt(f),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Generic(GenId),
    Concrete(ConcreteType),
}

impl Type {
    pub const BOOL: Self = Self::Concrete(ConcreteType::Primitive(Primitive::Bool));
    pub const CHAR: Self = Self::Concrete(ConcreteType::Primitive(Primitive::Char));
    pub const U64: Self = Self::Concrete(ConcreteType::Primitive(Primitive::U64));
    pub const U32: Self = Self::Concrete(ConcreteType::Primitive(Primitive::U32));
    pub const U16: Self = Self::Concrete(ConcreteType::Primitive(Primitive::U16));
    pub const U8: Self = Self::Concrete(ConcreteType::Primitive(Primitive::U8));

    pub const I64: Self = Self::Concrete(ConcreteType::Primitive(Primitive::I64));
    pub const I32: Self = Self::Concrete(ConcreteType::Primitive(Primitive::I32));
    pub const I16: Self = Self::Concrete(ConcreteType::Primitive(Primitive::I16));
    pub const I8: Self = Self::Concrete(ConcreteType::Primitive(Primitive::I8));

    pub const VOID: Self = Self::Concrete(ConcreteType::Primitive(Primitive::Void));

    pub fn ptr_to(t: Self) -> Self {
        Self::Concrete(ConcreteType::Ptr(box t))
    }
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Generic(i) => write!(f, "Generic({:?})", i),
            Type::Concrete(t) => t.fmt(f),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GenId(pub usize);

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum ConcreteType {
    Ptr(Box<Type>),
    Primitive(Primitive),
    Custom(TypeId),
}

impl std::fmt::Debug for ConcreteType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConcreteType::Ptr(box ty) => {
                write!(f, "&>")?;
                ty.fmt(f)
            }
            ConcreteType::Primitive(p) => p.fmt(f),
            ConcreteType::Custom(s) => s.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct KProc<T> {
    pub generics: FnvHashMap<SmolStr, GenId>,
    pub span: Span,
    pub ins: Vec<Type>,
    pub outs: Vec<Type>,
    pub body: Vec<T>,
}
#[derive(Debug)]
pub struct KConst {
    pub outs: Vec<Type>,
    pub body: Vec<TirNode>,
}
struct KMem {}
struct KGVar {}

#[derive(Debug)]
pub struct KStruct {
    pub id: TypeId,
    pub fields: FnvHashMap<SmolStr, Type>,
}

pub struct Walker {
    known_procs: FnvHashMap<ItemPathBuf, KProc<Spanned<Hir>>>,
    known_gvars: FnvHashMap<ItemPathBuf, KGVar>,
    known_mems: FnvHashMap<ItemPathBuf, KMem>,
    known_structs: FnvHashMap<ItemPathBuf, KStruct>,
    checked_consts: FnvHashMap<ItemPathBuf, KConst>,
    checked_procs: FnvHashMap<ItemPathBuf, KProc<TirNode>>,
    ty_ctx: TyCtx,
    structs: StructIndex,
    next_type_id: usize,
    next_gen_id: usize,
}

#[derive(Debug)]
pub struct TypecheckedProgram {
    pub procs: FnvHashMap<ItemPathBuf, KProc<TirNode>>,
    pub consts: FnvHashMap<ItemPathBuf, KConst>,
    pub types: FnvHashMap<ItemPathBuf, KStruct>,
}

#[derive(Default)]
pub struct TyCtx {
    pub engine: Engine,
}

impl Walker {
    pub fn walk(
        ast: FnvHashMap<ItemPathBuf, hir::TopLevel>,
        structs: StructIndex,
    ) -> Result<TypecheckedProgram, Error> {
        let mut this = Walker {
            checked_consts: Default::default(),
            known_procs: Default::default(),
            known_gvars: Default::default(),
            known_mems: Default::default(),
            known_structs: Default::default(),
            structs,
            checked_procs: Default::default(),
            ty_ctx: Default::default(),
            next_type_id: Default::default(),
            next_gen_id: Default::default(),
        };

        for ty in this.structs.clone() {
            let mut fields = FnvHashMap::default();
            for (n, ty) in &ty.fields {
                let ty = this.abstract_to_concrete_type(ty, None)?;
                fields.insert(n.clone(), ty);
            }
            let id = this.type_id();
            this.known_structs.insert(ty.name, KStruct { id, fields });
        }

        for (path, def) in ast {
            match def {
                hir::TopLevel::Proc(proc) => this.register_proc(path, proc)?,
                hir::TopLevel::Const(const_) => this.register_const(path, const_)?,
                hir::TopLevel::Mem(_) => todo!(),
                hir::TopLevel::Var(_) => todo!(),
            }
        }

        this.typecheck()?;

        TypecheckedProgram {
            procs: this.checked_procs,
            consts: this.checked_consts,
            types: this.known_structs,
        }
        .okay()
    }

    fn register_proc(&mut self, path: ItemPathBuf, proc: hir::Proc) -> Result<(), Error> {
        let generics = proc
            .generics
            .into_iter()
            .map(|g| (g, self.gen_id()))
            .collect();
        self.gen_id_reset();
        let ins = proc
            .ins
            .into_iter()
            .map(|ty| self.abstract_to_concrete_type(&ty, Some(&generics)))
            .collect::<Result<Vec<_>, _>>()?;
        let outs = proc
            .outs
            .into_iter()
            .map(|ty| self.abstract_to_concrete_type(&ty, Some(&generics)))
            .collect::<Result<Vec<_>, _>>()?;

        let known_proc = KProc {
            generics,
            ins,
            outs,
            span: proc.span,
            body: proc.body,
        };

        self.known_procs.insert(path, known_proc);
        ().okay()
    }

    fn register_const(&mut self, path: ItemPathBuf, const_: hir::Const) -> Result<(), Error> {
        let outs = const_
            .outs
            .iter()
            .map(|ty| self.abstract_to_concrete_type(ty, None))
            .collect::<Result<Vec<_>, _>>()?;

        let mut heap = THeap::default();
        let mut ts = TypeStack::default();
        let body = self.typecheck_body(&mut ts, &mut heap, &const_.body, None, None, None, true)?;

        let known_const = KConst { outs, body };
        self.checked_consts.insert(path, known_const);
        ().okay()
    }

    fn typecheck(&mut self) -> Result<(), Error> {
        let mut heap = THeap::default();
        let known_procs = self.known_procs.keys().cloned().collect::<Vec<_>>();

        for path in known_procs {
            self.typecheck_proc(&mut heap, &path)?;
        }

        Ok(())
    }

    fn typecheck_proc(&mut self, heap: &mut THeap, path: &ItemPath) -> Result<(), Error> {
        let proc = self.known_procs.get(path).cloned().unwrap();

        let mut ins = TypeStack::from_iter(
            proc.ins.iter().map(|term| {
                let info = type_to_info(&mut self.ty_ctx.engine, term, false);
                self.ty_ctx.engine.insert(info)
            }),
            heap,
        );

        let outs = proc.outs.iter().map(|term| {
            let info = type_to_info(&mut self.ty_ctx.engine, term, false);
            self.ty_ctx.engine.insert(info)
        });
        let outs = TypeStack::from_iter(outs, heap);

        let body = self.typecheck_body(
            &mut ins,
            heap,
            &proc.body,
            None,
            Some(&proc.generics),
            Some(&outs),
            false,
        )?;

        if let Err(msg) = self.ty_ctx.engine.unify_stacks(heap, ins, outs) {
            return error(proc.span, ErrorKind::UnificationError, msg);
        }

        self.checked_procs.insert(
            path.to_owned(),
            KProc {
                generics: proc.generics,
                span: proc.span,
                ins: proc.ins,
                outs: proc.outs,
                body,
            },
        );
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn typecheck_body(
        &mut self,
        ins: &mut TypeStack,
        heap: &mut THeap,
        body: &[Spanned<Hir>],
        bindings: Option<&FnvHashMap<SmolStr, TermId>>,
        generics: Option<&FnvHashMap<SmolStr, GenId>>,
        expected_outs: Option<&TypeStack>,
        in_const: bool,
    ) -> Result<Vec<TirNode>, Error> {
        let mut checked_body = Vec::new();
        for node in body {
            let node = match &node.inner {
                Hir::Path(path)
                    if path
                        .only()
                        .and_then(|p| bindings.and_then(|bs| bs.get(p)))
                        .is_some() =>
                {
                    let ty = path
                        .only()
                        .and_then(|p| bindings.and_then(|bs| bs.get(p)))
                        .copied()
                        .unwrap();
                    ins.push(heap, ty);
                    TirNode {
                        span: node.span,
                        node: Tir::BindingUse(path.clone()),
                        ins: vec![],
                        outs: vec![ty],
                    }
                }
                Hir::Path(proc_name) if self.is_proc(proc_name) => {
                    if in_const {
                        return error(
                            node.span,
                            ErrorKind::CallInConst,
                            "Calls in constant bodies are not supported",
                        );
                    }
                    let (ins, outs) = if let Some(p) = self.known_procs.get(proc_name) {
                        let expected_ins = p
                            .ins
                            .iter()
                            .map(|ty| {
                                let info = type_to_info(&mut self.ty_ctx.engine, ty, true);
                                self.ty_ctx.engine.insert(info)
                            })
                            .collect::<Vec<_>>();
                        for expected_ty in &expected_ins {
                            if let Some(actual_t) = ins.pop(heap) {
                                if let Err(message) =
                                    self.ty_ctx.engine.unify(*expected_ty, actual_t)
                                {
                                    return error(node.span, ErrorKind::UnificationError, message);
                                }
                            } else {
                                return error(
                                    node.span,
                                    ErrorKind::NotEnoughData,
                                    format!(
                                        "Not enough data in the stack for `{:?}` proc",
                                        proc_name
                                    ),
                                );
                            }
                        }
                        let outs = p
                            .outs
                            .iter()
                            .map(|ty| {
                                let info = type_to_info(&mut self.ty_ctx.engine, ty, true);
                                self.ty_ctx.engine.insert(info)
                            })
                            .collect::<Vec<_>>();
                        for out_t in &outs {
                            ins.push(heap, *out_t);
                        }
                        (expected_ins, outs)
                    } else {
                        return error(node.span, ErrorKind::Undefined, "Undefined proc");
                    };

                    TirNode {
                        span: node.span,
                        node: Tir::Call(proc_name.clone()),
                        ins,
                        outs,
                    }
                }
                Hir::Path(const_name) if self.is_const(const_name) => {
                    let outs: Vec<TermId> = self
                        .checked_consts
                        .get(const_name)
                        .unwrap()
                        .outs
                        .iter()
                        .map(|term| {
                            let info = type_to_info(&mut self.ty_ctx.engine, term, false);
                            self.ty_ctx.engine.insert(info)
                        })
                        .collect();

                    for ty in &outs {
                        ins.push(heap, *ty);
                    }

                    TirNode {
                        span: node.span,
                        node: Tir::ConstUse(const_name.clone()),
                        ins: vec![],
                        outs,
                    }
                }
                Hir::Path(path) => {
                    return error(
                        node.span,
                        ErrorKind::Undefined,
                        format!("Undefined word `{:?}`", path),
                    )
                }
                Hir::Intrinsic(i) => {
                    let (mut w_ins, mut w_outs) = (Vec::new(), Vec::new());
                    match i {
                        Intrinsic::Drop => {
                            if let Some(ty) = ins.pop(heap) {
                                w_ins.push(ty);
                            } else {
                                return error(
                                    node.span,
                                    ErrorKind::NotEnoughData,
                                    "Not enough data in the stack for `pop` intrinsic",
                                );
                            }
                        }
                        Intrinsic::Dup => {
                            if let Some(ty) = ins.pop(heap) {
                                ins.push(heap, ty);
                                ins.push(heap, ty);
                                w_ins.push(ty);
                                w_outs.push(ty);
                                w_outs.push(ty);
                            } else {
                                return error(
                                    node.span,
                                    ErrorKind::NotEnoughData,
                                    "Not enough data in the stack for `dup` intrinsic",
                                );
                            }
                        }
                        Intrinsic::Swap => {
                            if let Some(ty1) = ins.pop(heap) {
                                if let Some(ty2) = ins.pop(heap) {
                                    ins.push(heap, ty1);
                                    ins.push(heap, ty2);
                                    w_ins.push(ty1);
                                    w_ins.push(ty2);
                                    w_outs.push(ty1);
                                    w_outs.push(ty2);
                                } else {
                                    return error(
                                        node.span,
                                        ErrorKind::NotEnoughData,
                                        "Not enough data in the stack for `swap` intrinsic",
                                    );
                                }
                            } else {
                                return error(
                                    node.span,
                                    ErrorKind::NotEnoughData,
                                    "Not enough data in the stack for `swap` intrinsic",
                                );
                            }
                        }
                        Intrinsic::Over => {
                            if let Some(ty1) = ins.pop(heap) {
                                if let Some(ty2) = ins.pop(heap) {
                                    ins.push(heap, ty2);
                                    ins.push(heap, ty1);
                                    ins.push(heap, ty2);
                                    w_ins.push(ty1);
                                    w_ins.push(ty2);
                                    w_outs.push(ty2);
                                    w_outs.push(ty1);
                                    w_outs.push(ty2);
                                } else {
                                    return error(
                                        node.span,
                                        ErrorKind::NotEnoughData,
                                        "Not enough data in the stack for `over` intrinsic",
                                    );
                                }
                            } else {
                                return error(
                                    node.span,
                                    ErrorKind::NotEnoughData,
                                    "Not enough data in the stack for `over` intrinsic",
                                );
                            }
                        }
                        Intrinsic::Cast(o_ty) => {
                            let o_type = self.abstract_to_concrete_type(o_ty, generics)?;
                            let o_type = type_to_info(&mut self.ty_ctx.engine, &o_type, false);
                            let o_type = self.ty_ctx.engine.insert(o_type);
                            if let Some(in_t) = ins.pop(heap) {
                                ins.push(heap, o_type);
                                w_ins.push(in_t);
                                w_outs.push(o_type);
                            } else {
                                return error(
                                    node.span,
                                    ErrorKind::NotEnoughData,
                                    "Not enough data in the stack for `cast` intrinsic",
                                );
                            }
                        }
                        Intrinsic::ReadU64 => {
                            if let Some(actual) = ins.pop(heap) {
                                // let vptr = type_to_info(
                                //     &mut self.ty_ctx.engine,
                                //     &Type::ptr_to(Type::VOID),
                                //     false,
                                // );
                                let unknown = self.ty_ctx.engine.insert(TypeInfo::Unknown);
                                let vptr = TypeInfo::Ptr(unknown);
                                let expect = self.ty_ctx.engine.insert(vptr);
                                if let Err(message) = self.ty_ctx.engine.unify(expect, actual) {
                                    return error(node.span, ErrorKind::UnificationError, message);
                                } else {
                                    let out_ty = self.ty_ctx.engine.insert(TypeInfo::U64);
                                    ins.push(heap, out_ty);
                                    w_ins.push(expect);
                                    w_outs.push(out_ty);
                                }
                            } else {
                                return error(
                                    node.span,
                                    ErrorKind::NotEnoughData,
                                    "Not enough data in the stack for `!u64` intrinsic",
                                );
                            }
                        }
                        Intrinsic::ReadU8 => todo!(),
                        Intrinsic::WriteU64 => {
                            if let Some(ty1) = ins.pop(heap) {
                                let vptr = type_to_info(
                                    &mut self.ty_ctx.engine,
                                    &Type::ptr_to(Type::VOID),
                                    false,
                                );
                                let expect = self.ty_ctx.engine.insert(vptr);
                                if let Err(message) = self.ty_ctx.engine.unify(expect, ty1) {
                                    return error(node.span, ErrorKind::UnificationError, message);
                                }
                                if let Some(ty2) = ins.pop(heap) {
                                    let u64 = type_to_info(
                                        &mut self.ty_ctx.engine,
                                        &Type::ptr_to(Type::VOID),
                                        false,
                                    );
                                    let expect = self.ty_ctx.engine.insert(u64);
                                    if let Err(message) = self.ty_ctx.engine.unify(expect, ty2) {
                                        return error(
                                            node.span,
                                            ErrorKind::UnificationError,
                                            message,
                                        );
                                    }
                                    w_ins.push(ty1);
                                    w_ins.push(ty2);
                                } else {
                                    return error(
                                        node.span,
                                        ErrorKind::NotEnoughData,
                                        "Not enough data in the stack for `!u64` intrinsic",
                                    );
                                }
                            } else {
                                return error(
                                    node.span,
                                    ErrorKind::NotEnoughData,
                                    "Not enough data in the stack for `!u64` intrinsic",
                                );
                            }
                        }
                        Intrinsic::WriteU8 => todo!(),
                        Intrinsic::CompStop => todo!(),
                        Intrinsic::Dump => todo!(),
                        Intrinsic::Print => todo!(),
                        Intrinsic::Syscall0 => todo!(),
                        Intrinsic::Syscall1 => todo!(),
                        Intrinsic::Syscall2 => todo!(),
                        Intrinsic::Syscall3 => todo!(),
                        Intrinsic::Syscall4 => todo!(),
                        Intrinsic::Syscall5 => todo!(),
                        Intrinsic::Syscall6 => todo!(),
                        Intrinsic::Argc => todo!(),
                        Intrinsic::Argv => todo!(),
                        Intrinsic::Add => {
                            if let Some(a) = ins.pop(heap) {
                                if let Some(b) = ins.pop(heap) {
                                    if let Err(message) = self.ty_ctx.engine.unify(a, b) {
                                        return error(
                                            node.span,
                                            ErrorKind::UnificationError,
                                            message,
                                        );
                                    }
                                    ins.push(heap, a);
                                } else {
                                    return error(
                                        node.span,
                                        ErrorKind::NotEnoughData,
                                        "Not enough data in the stack for `add` intrinsic",
                                    );
                                }
                            } else {
                                return error(
                                    node.span,
                                    ErrorKind::NotEnoughData,
                                    "Not enough data in the stack for `add` intrinsic",
                                );
                            }
                        }
                        Intrinsic::Sub => todo!(),
                        Intrinsic::Divmod => todo!(),
                        Intrinsic::Mul => todo!(),
                        Intrinsic::Eq => todo!(),
                        Intrinsic::Ne => todo!(),
                        Intrinsic::Lt => todo!(),
                        Intrinsic::Le => todo!(),
                        Intrinsic::Gt => todo!(),
                        Intrinsic::Ge => todo!(),
                    };
                    TirNode {
                        span: node.span,
                        node: Tir::Intrinsic(i.clone()),
                        ins: w_ins,
                        outs: w_outs,
                    }
                }
                Hir::Bind(hir::Bind { bindings, body }) => {
                    let mut w_ins = Vec::new();
                    let bindings_hash = bindings
                        .iter()
                        .filter_map(|Spanned { span, inner: b }| match b {
                            hir::Binding::Ignore => {
                                if let Some(ty) = ins.pop(heap) {
                                    w_ins.push(ty);
                                    None
                                } else {
                                    Some(error(
                                        *span,
                                        ErrorKind::NotEnoughData,
                                        "Not enough data to bind",
                                    ))
                                }
                            }
                            hir::Binding::Bind { name, ty: expected } => {
                                let ty = if let Some(ty) = ins.pop(heap) {
                                    if let Some(ty2) = expected {
                                        let ty2 = self
                                            .abstract_to_concrete_type(
                                                &Spanned {
                                                    span: node.span,
                                                    inner: ty2.clone(),
                                                },
                                                generics,
                                            )
                                            .map(|ty| {
                                                let info = type_to_info(
                                                    &mut self.ty_ctx.engine,
                                                    &ty,
                                                    false,
                                                );
                                                self.ty_ctx.engine.insert(info)
                                            });

                                        if let Ok(ty2) = ty2 {
                                            if let Err(message) = self.ty_ctx.engine.unify(ty, ty2)
                                            {
                                                return Some(error(
                                                    *span,
                                                    ErrorKind::UnificationError,
                                                    message,
                                                ));
                                            }
                                        }
                                    }
                                    ty
                                } else {
                                    return Some(error(
                                        *span,
                                        ErrorKind::NotEnoughData,
                                        "Not enough data to bind",
                                    ));
                                };
                                w_ins.push(ty);
                                Some(Ok((name.clone(), ty)))
                            }
                        })
                        .collect::<Result<FnvHashMap<_, _>, _>>()?;
                    let b_ins = ins.clone().into_vec(heap);
                    let body = self.typecheck_body(
                        ins,
                        heap,
                        body,
                        Some(&bindings_hash),
                        generics,
                        expected_outs,
                        in_const,
                    )?;
                    let mut a_ins = ins.clone().into_vec(heap).into_iter();
                    for _ in b_ins {
                        a_ins.next();
                    }
                    let w_outs = a_ins.collect();
                    TirNode {
                        span: node.span,
                        node: Tir::Bind(Bind {
                            bindings: bindings.clone(),
                            body,
                        }),
                        ins: w_ins,
                        outs: w_outs,
                    }
                }
                Hir::While(hir::While { cond, body }) => {
                    let mut ins_c = ins.clone();
                    let cond = self.typecheck_body(
                        &mut ins_c,
                        heap,
                        cond,
                        bindings,
                        generics,
                        expected_outs,
                        in_const,
                    )?;
                    if let Some(ty) = ins_c.pop(heap) {
                        let bool = self.ty_ctx.engine.insert(TypeInfo::Bool);
                        if let Err(message) = self.ty_ctx.engine.unify(ty, bool) {
                            return error(node.span, ErrorKind::UnificationError, message);
                        }
                        if let Err(message) =
                            self.ty_ctx.engine.unify_stacks(heap, ins.clone(), ins_c)
                        {
                            return error(node.span, ErrorKind::UnificationError, message);
                        };
                    } else {
                        return error(
                            node.span,
                            ErrorKind::NotEnoughData,
                            "Not enough data on the stack for `while` condition",
                        );
                    };
                    let mut ins_b = ins.clone();
                    let body = self.typecheck_body(
                        &mut ins_b,
                        heap,
                        body,
                        bindings,
                        generics,
                        expected_outs,
                        in_const,
                    )?;
                    if let Err(message) = self.ty_ctx.engine.unify_stacks(heap, ins.clone(), ins_b)
                    {
                        return error(node.span, ErrorKind::UnificationError, message);
                    };

                    TirNode {
                        span: node.span,
                        node: Tir::While(While { cond, body }),
                        ins: vec![],
                        outs: vec![],
                    }
                }
                Hir::If(hir::If { truth, lie }) => {
                    let bool = self.ty_ctx.engine.insert(TypeInfo::Bool);
                    let b = ins.pop(heap).map(|b| self.ty_ctx.engine.unify(b, bool));
                    if let Some(Err(message)) = b {
                        return error(node.span, ErrorKind::UnificationError, message);
                    } else if b.is_none() {
                        return error(
                            node.span,
                            ErrorKind::NotEnoughData,
                            "Not enough data on the stack for `if`",
                        );
                    };

                    let mut ins_t = ins.clone();
                    let truth = self.typecheck_body(
                        &mut ins_t,
                        heap,
                        truth,
                        bindings,
                        generics,
                        expected_outs,
                        in_const,
                    )?;
                    let lie = if let Some(lie) = lie {
                        let mut ins_l = ins.clone();
                        let lie = self.typecheck_body(
                            &mut ins_l,
                            heap,
                            lie,
                            bindings,
                            generics,
                            expected_outs,
                            in_const,
                        )?;

                        if let Err(message) = self.ty_ctx.engine.unify_stacks(heap, ins_t, ins_l) {
                            return error(node.span, ErrorKind::UnificationError, message);
                        }
                        Some(lie)
                    } else {
                        None
                    };

                    TirNode {
                        span: node.span,
                        node: Tir::If(If { truth, lie }),
                        ins: vec![bool],
                        outs: vec![],
                    }
                }
                Hir::Cond(hir::Cond { branches }) => {
                    let ty = if let Some(ty) = ins.pop(heap) {
                        ty
                    } else {
                        return error(
                            node.span,
                            ErrorKind::NotEnoughData,
                            "Not enough data for `cond`",
                        );
                    };
                    let mut bodies = Vec::new();
                    let mut tir_branches = Vec::new();
                    for hir::CondBranch { pattern, body } in branches {
                        let (pat_ty, tir_pat) = match &pattern.inner {
                            Hir::Literal(l) => match l {
                                Literal::Bool(_) => (
                                    self.ty_ctx.engine.insert(TypeInfo::Bool),
                                    Tir::Literal(l.clone()),
                                ),
                                Literal::Num(_) => (
                                    self.ty_ctx.engine.insert(TypeInfo::U64),
                                    Tir::Literal(l.clone()),
                                ),
                                Literal::String(_) => todo!(),
                                Literal::Char(_) => (
                                    self.ty_ctx.engine.insert(TypeInfo::Char),
                                    Tir::Literal(l.clone()),
                                ),
                            },
                            Hir::IgnorePattern => (
                                self.ty_ctx.engine.insert(TypeInfo::Unknown),
                                Tir::IgnorePattern,
                            ),
                            _ => todo!(),
                        };
                        let tir_pattern = TirNode {
                            span: pattern.span,
                            node: tir_pat,
                            ins: vec![],
                            outs: vec![],
                        };
                        if let Err(message) = self.ty_ctx.engine.unify(ty, pat_ty) {
                            return error(pattern.span, ErrorKind::UnificationError, message);
                        }
                        let mut ins_b = ins.clone();
                        let body = self.typecheck_body(
                            &mut ins_b,
                            heap,
                            body,
                            bindings,
                            generics,
                            expected_outs,
                            in_const,
                        )?;
                        let tir_branch = CondBranch {
                            pattern: tir_pattern,
                            body,
                        };
                        tir_branches.push(tir_branch);
                        bodies.push(ins_b);
                    }
                    if bodies.array_windows::<2>().any(|[a, b]| {
                        self.ty_ctx
                            .engine
                            .unify_stacks(heap, a.clone(), b.clone())
                            .is_err()
                    }) {
                        return error(node.span, ErrorKind::UnificationError, "Uhh");
                    }
                    *ins = bodies.pop().unwrap();

                    TirNode {
                        span: node.span,
                        node: Tir::Cond(Cond {
                            branches: tir_branches,
                        }),
                        ins: vec![],
                        outs: vec![],
                    }
                }
                Hir::Literal(l) => {
                    let mut w_outs = Vec::new();
                    match l {
                        Literal::Bool(_) => {
                            let bool = self.ty_ctx.engine.insert(TypeInfo::Bool);
                            ins.push(heap, bool);
                            w_outs.push(bool)
                        }
                        Literal::Num(_) => {
                            let u64 = self.ty_ctx.engine.insert(TypeInfo::U64);
                            ins.push(heap, u64);
                            w_outs.push(u64)
                        }
                        Literal::String(_) => {
                            let cptr = type_to_info(
                                &mut self.ty_ctx.engine,
                                &Type::ptr_to(Type::CHAR),
                                false,
                            );
                            let cptr = self.ty_ctx.engine.insert(cptr);
                            let u64 = self.ty_ctx.engine.insert(TypeInfo::U64);
                            ins.push(heap, u64);
                            ins.push(heap, cptr);
                            w_outs.push(u64);
                            w_outs.push(cptr)
                        }
                        Literal::Char(_) => {
                            let char = self.ty_ctx.engine.insert(TypeInfo::Char);
                            ins.push(heap, char);
                            w_outs.push(char)
                        }
                    };
                    TirNode {
                        span: node.span,
                        node: Tir::Literal(l.clone()),
                        ins: vec![],
                        outs: w_outs,
                    }
                }
                Hir::IgnorePattern => todo!(),
                Hir::Return => {
                    if let Some(expected) = expected_outs {
                        if let Err(message) =
                            self.ty_ctx
                                .engine
                                .unify_stacks(heap, ins.clone(), expected.clone())
                        {
                            return error(node.span, ErrorKind::UnificationError, message);
                        }
                    }
                    TirNode {
                        span: node.span,
                        node: Tir::Return,
                        ins: ins.clone().into_vec(heap),
                        outs: vec![],
                    }
                }
                Hir::FieldAccess(hir::FieldAccess { field }) => {
                    let (in_ty, out_ty) = if let Some(ty) = ins.pop(heap) {
                        let out_ty = match self.ty_ctx.engine.reconstruct(ty) {
                            Ok(ty) => match ty {
                                Type::Generic(_) => todo!(),
                                Type::Concrete(ty) => match ty {
                                    ConcreteType::Ptr(box ty) => match ty {
                                        Type::Generic(_) => todo!(),
                                        Type::Concrete(ty) => match ty {
                                            ConcreteType::Ptr(_) => todo!(),
                                            ConcreteType::Primitive(_) => todo!(),
                                            ConcreteType::Custom(id) => {
                                                if let Some((_, KStruct { id: _, fields })) = self
                                                    .known_structs
                                                    .iter()
                                                    .find(|(_, v)| v.id == id)
                                                {
                                                    if let Some(ty) = fields.get(field) {
                                                        let info = type_to_info(
                                                            &mut self.ty_ctx.engine,
                                                            ty,
                                                            true,
                                                        );
                                                        let ty = self.ty_ctx.engine.insert(info);
                                                        self.ty_ctx.engine.insert(TypeInfo::Ptr(ty))
                                                    } else {
                                                        return error(
                                                            node.span,
                                                            ErrorKind::Undefined,
                                                            "No suchfield on this type",
                                                        );
                                                    }
                                                } else {
                                                    return error(
                                                        node.span,
                                                        ErrorKind::Undefined,
                                                        "Undefined type",
                                                    );
                                                }
                                            }
                                        },
                                    },
                                    ConcreteType::Primitive(_) => todo!(),
                                    ConcreteType::Custom(_) => todo!(),
                                },
                            },
                            Err(_) => todo!(),
                        };
                        ins.push(heap, out_ty);
                        (ty, out_ty)
                    } else {
                        return error(
                            node.span,
                            ErrorKind::NotEnoughData,
                            "There is no struct pointer on the stack",
                        );
                    };

                    TirNode {
                        span: node.span,
                        node: Tir::FieldAccess(FieldAccess {
                            field: field.clone(),
                        }),
                        ins: vec![in_ty],
                        outs: vec![out_ty],
                    }
                }
            };
            checked_body.push(node);
        }

        Ok(checked_body)
    }

    fn type_id(&mut self) -> TypeId {
        let id = TypeId(self.next_type_id);
        self.next_type_id += 1;
        id
    }

    fn gen_id(&mut self) -> GenId {
        let id = GenId(self.next_type_id);
        self.next_gen_id += 1;
        id
    }

    fn gen_id_reset(&mut self) {
        self.next_gen_id = 0;
    }

    fn abstract_to_concrete_type(
        &mut self,
        ty: &Spanned<types::Type>,
        generics: Option<&FnvHashMap<SmolStr, GenId>>,
    ) -> Result<Type, Error> {
        let span = ty.span;
        let kind = match &ty.inner {
            types::Type::Ptr(box t) => Type::ptr_to(self.abstract_to_concrete_type(
                &Spanned {
                    span,
                    inner: t.clone(),
                },
                generics,
            )?),
            types::Type::Primitive(ty) => Type::Concrete(ConcreteType::Primitive(*ty)),
            types::Type::Custom(ty) => {
                if let Some(ty) = self.known_structs.get(ty) {
                    Type::Concrete(ConcreteType::Custom(ty.id))
                } else if let Some(ty) = self.structs.get(ty).cloned() {
                    let mut fields = FnvHashMap::default();
                    for (n, ty) in ty.fields {
                        let ty = self.abstract_to_concrete_type(&ty, None)?;
                        fields.insert(n, ty);
                    }
                    let id = self.type_id();
                    self.known_structs.insert(ty.name, KStruct { id, fields });
                    Type::Concrete(ConcreteType::Custom(id))
                } else if let Some(g) = ty.only() {
                    if let Some(id) = generics.and_then(|gs| gs.get(g)) {
                        Type::Generic(*id)
                    } else {
                        return typecheck::error(
                            span,
                            typecheck::ErrorKind::Undefined,
                            "This type name is not found in the current scope",
                        );
                    }
                } else {
                    return typecheck::error(
                        span,
                        typecheck::ErrorKind::Undefined,
                        "This type name is not found in the current scope",
                    );
                }
            }
        };

        kind.okay()
    }

    fn is_proc(&self, name: &ItemPath) -> bool {
        self.known_procs.contains_key(name)
    }

    fn is_const(&self, name: &ItemPath) -> bool {
        self.checked_consts.contains_key(name)
    }
}
