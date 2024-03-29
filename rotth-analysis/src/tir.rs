use fnv::FnvHashMap;
use itempath::{ItemPath, ItemPathBuf};
use rotth_parser::{
    ast::Literal,
    hir::{self, Hir, Intrinsic},
    types::{self, StructIndex},
};
use smol_str::SmolStr;
use somok::{PartitionThree, Somok, Ternary};
use spanner::{Span, Spanned};
use std::rc::Rc;

use crate::{
    error,
    inference::{Engine, Insert, ReifiedType, TermId, TypeInfo},
    typecheck::{self, NodeRepr, THeap, TypeRepr, TypeStack},
    Error, ErrorKind,
};

impl TypeRepr for Type {}
impl<T: TypeRepr, I> NodeRepr for TypedNode<T, I> {}

#[derive(Clone)]
pub struct TypedNode<T: TypeRepr, I> {
    pub span: Span,
    pub node: TypedIr<T, I>,
    pub ins: Vec<T>,
    pub outs: Vec<T>,
}

pub type TirNode = TypedNode<TermId, Intrinsic<TermId>>;

impl<T: TypeRepr + std::fmt::Debug, I: std::fmt::Debug> std::fmt::Debug for TypedNode<T, I> {
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
pub struct Bind<T: NodeRepr> {
    pub bindings: Vec<Spanned<hir::Binding>>,
    pub body: Vec<T>,
}
#[derive(Debug, Clone)]
pub struct While<T: NodeRepr> {
    pub cond: Vec<T>,
    pub body: Vec<T>,
}
#[derive(Clone)]
pub struct If<T: NodeRepr> {
    pub truth: Vec<T>,
    pub lie: Option<Vec<T>>,
}

impl<T: NodeRepr + std::fmt::Debug> std::fmt::Debug for If<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut d = f.debug_struct("If");
        d.field("truth", &self.truth);
        if let Some(lie) = &self.lie {
            d.field("lie", lie);
        }
        d.finish()
    }
}

#[derive(Debug, Clone)]
pub struct Cond<T: TypeRepr, I> {
    pub branches: Vec<CondBranch<T, I>>,
}
#[derive(Debug, Clone)]
pub struct CondBranch<T: TypeRepr, I> {
    pub pattern: TypedNode<T, I>,
    pub body: Vec<TypedNode<T, I>>,
}

#[derive(Debug, Clone)]
pub struct FieldAccess<T> {
    pub ty: T,
    pub field: SmolStr,
}

#[derive(Clone)]
pub enum TypedIr<T: TypeRepr, I> {
    MemUse(ItemPathBuf),
    GVarUse(ItemPathBuf),
    LVarUse(ItemPathBuf),
    BindingUse(ItemPathBuf),
    ConstUse(ItemPathBuf),
    Call(ItemPathBuf),
    Intrinsic(I),
    Bind(Bind<TypedNode<T, I>>),
    While(While<TypedNode<T, I>>),
    If(If<TypedNode<T, I>>),
    Cond(Cond<T, I>),
    Literal(Literal),
    IgnorePattern,
    Return,
    FieldAccess(FieldAccess<T>),
}

impl<T: TypeRepr + std::fmt::Debug, I: std::fmt::Debug> std::fmt::Debug for TypedIr<T, I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::GVarUse(arg0) => f.debug_tuple("GVarUse").field(arg0).finish(),
            Self::LVarUse(arg0) => f.debug_tuple("LVarUse").field(arg0).finish(),
            Self::MemUse(arg0) => f.debug_tuple("MemUse").field(arg0).finish(),
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
    Concrete(TermId),
    Ptr(Box<Self>),
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Generic(i) => write!(f, "Generic({i:?})"),
            Type::Concrete(t) => t.fmt(f),
            Type::Ptr(box t) => write!(f, "&>{t:?}"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct GenId(pub usize);

#[derive(Debug, Clone)]
pub struct KProc<T> {
    pub generics: FnvHashMap<SmolStr, GenId>,
    pub vars: FnvHashMap<ItemPathBuf, Var<TermId>>,
    pub span: Span,
    pub ins: Vec<TermId>,
    pub outs: Vec<TermId>,
    pub body: Vec<T>,
}
#[derive(Debug, Clone)]
pub struct KConst<T> {
    pub outs: Vec<Type>,
    pub span: Span,
    pub body: Vec<T>,
}
#[derive(Debug, Clone)]
pub struct KMem<T> {
    pub span: Span,
    pub body: Vec<T>,
}
#[derive(Debug, Clone)]

pub struct Var<T> {
    pub ty: T,
}

#[derive(Debug, Clone)]
pub struct KStruct {
    pub typename: ItemPathBuf,
    pub generics: Vec<GenId>,
    pub fields: FnvHashMap<SmolStr, Type>,
}

#[derive(Debug, Clone)]
pub enum Ref {
    Proc(ItemPathBuf),
    Mem(ItemPathBuf),
    Const(ItemPathBuf),
    Struct(ItemPathBuf),
    Var,
}

#[derive(Debug)]
pub struct Walker {
    known_procs: FnvHashMap<ItemPathBuf, Rc<KProc<Spanned<Hir>>>>,
    known_gvars: FnvHashMap<ItemPathBuf, TermId>,
    known_mems: FnvHashMap<ItemPathBuf, Rc<KMem<TirNode>>>,
    known_structs: FnvHashMap<ItemPathBuf, Rc<KStruct>>,
    checked_consts: FnvHashMap<ItemPathBuf, Rc<KConst<TirNode>>>,
    checked_procs: FnvHashMap<ItemPathBuf, KProc<TypedNode<TermId, Intrinsic<TermId>>>>,
    item_refs: FnvHashMap<ItemPathBuf, Ref>,
    unresloved_item_refs: FnvHashMap<ItemPathBuf, ItemPathBuf>,
    engine: Engine,
    structs: StructIndex,
    next_gen_id: usize,
}

#[derive(Debug)]
pub struct TypecheckedProgram {
    pub procs: FnvHashMap<ItemPathBuf, KProc<TypedNode<TermId, Intrinsic<TermId>>>>,
    pub consts: FnvHashMap<ItemPathBuf, KConst<TirNode>>,
    pub mems: FnvHashMap<ItemPathBuf, KMem<TirNode>>,
    pub vars: FnvHashMap<ItemPathBuf, ReifiedType>,
    pub structs: FnvHashMap<ItemPathBuf, KStruct>,
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
            item_refs: Default::default(),
            unresloved_item_refs: Default::default(),
            checked_procs: Default::default(),
            engine: Engine::default(),
            next_gen_id: Default::default(),
        };

        for struct_ in this.structs.clone() {
            let mut generics_hash: FnvHashMap<SmolStr, GenId> = Default::default();
            let mut generics: Vec<GenId> = Default::default();
            for (name, gid) in struct_
                .generics
                .into_iter()
                .map(|ty| (ty.inner, this.gen_id()))
            {
                generics.push(gid);
                generics_hash.insert(name, gid);
            }
            let mut fields = FnvHashMap::default();
            for (n, ty) in &struct_.fields {
                let ty = this.abstract_to_concrete_type(ty, Some(&generics_hash))?;
                fields.insert(n.clone(), ty);
            }
            this.known_structs.insert(
                struct_.name.clone(),
                Rc::new(KStruct {
                    generics,
                    fields,
                    typename: struct_.name,
                }),
            );
        }

        let (consts, mems_vars, items) =
            ast.into_iter()
                .partition_three::<Vec<_>, _>(|(_, i)| match i {
                    hir::TopLevel::Ref(_) => Ternary::Third,
                    hir::TopLevel::Proc(_) => Ternary::Third,
                    hir::TopLevel::Const(_) => Ternary::First,
                    hir::TopLevel::Mem(_) => Ternary::Second,
                    hir::TopLevel::Var(_) => Ternary::Second,
                });
        for (path, def) in consts {
            match def {
                hir::TopLevel::Const(const_) => this.register_const(path, const_)?,
                _ => unreachable!(),
            }
        }
        for (path, def) in mems_vars {
            match def {
                hir::TopLevel::Mem(mem) => this.register_mem(path, mem)?,
                hir::TopLevel::Var(var) => this.register_var(path, var)?,
                _ => unreachable!(),
            }
        }
        for (path, def) in items {
            match def {
                hir::TopLevel::Ref(referee) => {
                    this.unresloved_item_refs.insert(path, referee);
                }
                hir::TopLevel::Proc(proc) => this.register_proc(path, proc)?,
                _ => unreachable!(),
            }
        }
        this.resolve_refs()?;

        this.typecheck()?;

        let Walker {
            known_structs,
            checked_consts,
            checked_procs,
            engine,
            known_mems,
            known_gvars,
            ..
        } = this;

        TypecheckedProgram {
            procs: checked_procs,
            consts: checked_consts
                .into_iter()
                .map(|(a, b)| (a, Rc::try_unwrap(b).unwrap()))
                .collect(),
            mems: known_mems
                .into_iter()
                .map(|(a, b)| (a, Rc::try_unwrap(b).unwrap()))
                .collect(),
            structs: known_structs
                .into_iter()
                .map(|(a, b)| (a, Rc::try_unwrap(b).unwrap()))
                .collect(),
            vars: known_gvars
                .into_iter()
                .map(|(p, t)| (p, engine.reify(&Default::default(), t).unwrap()))
                .collect(),
            engine,
        }
        .okay()
    }
    fn resolve(&self, referee: ItemPathBuf) -> Ref {
        if self.known_procs.get(&referee).cloned().is_some() {
            Ref::Proc(referee)
        } else if self.checked_consts.get(&referee).cloned().is_some() {
            Ref::Const(referee)
        } else if self.known_mems.get(&referee).cloned().is_some() {
            Ref::Mem(referee)
        } else if self.known_structs.get(&referee).cloned().is_some() {
            Ref::Struct(referee)
        } else if let Some(reference) = self.unresloved_item_refs.get(&referee).cloned() {
            self.resolve(reference)
        } else {
            unreachable!("Can't resolve reference: {referee:?}");
        }
    }
    fn resolve_refs(&mut self) -> Result<(), Error> {
        for (path, referee) in &self.unresloved_item_refs {
            let item_ref = self.resolve(referee.clone());
            self.item_refs.insert(path.clone(), item_ref);
        }
        Ok(())
    }

    fn register_proc(&mut self, path: ItemPathBuf, proc: hir::Proc) -> Result<(), Error> {
        if self.known_procs.contains_key(&path) {
            return Ok(());
        }
        let generics = proc
            .generics
            .into_iter()
            .map(|g| (g, self.gen_id()))
            .collect();
        let ins = proc
            .ins
            .into_iter()
            .map(|ty| {
                let ty = self.abstract_to_concrete_type(&ty, Some(&generics))?;
                let ty = self.engine.insert(&ty);

                Ok(ty)
            })
            .collect::<Result<Vec<_>, Error>>()?;
        let outs = proc
            .outs
            .into_iter()
            .map(|ty| {
                let ty = self.abstract_to_concrete_type(&ty, Some(&generics))?;
                let ty = self.engine.insert(&ty);
                Ok(ty)
            })
            .collect::<Result<Vec<_>, Error>>()?;
        let vars = proc
            .vars
            .into_iter()
            .map(|(k, hir::Var { ty, span: _ })| {
                Ok((k, {
                    let ty = self.abstract_to_concrete_type(&ty, Some(&generics))?;
                    let ty = self.engine.insert_unknown_generics(&ty);
                    Var { ty }
                }))
            })
            .collect::<Result<_, Error>>()?;

        let known_proc = KProc {
            vars,
            generics,
            ins,
            outs,
            span: proc.span,
            body: proc.body,
        };

        self.known_procs.insert(path, Rc::new(known_proc));
        ().okay()
    }

    fn register_mem(&mut self, path: ItemPathBuf, mem: hir::Mem) -> Result<(), Error> {
        if self.known_mems.contains_key(&path) {
            return Ok(());
        }
        let hir::Mem { body, span } = mem;

        let mut heap = THeap::default();
        let mut ts = TypeStack::default();
        let mut expected_outs = TypeStack::default();
        let u64 = self.engine.insert(TypeInfo::U64);
        expected_outs.push(&mut heap, u64);
        let body = self.typecheck_body(
            &mut ts,
            &mut heap,
            &body,
            &Default::default(),
            Default::default(),
            Default::default(),
            Some(&expected_outs),
            true,
        )?;

        self.known_mems.insert(path, Rc::new(KMem { span, body }));
        Ok(())
    }

    fn typecheck_syscall(
        &mut self,
        i: usize,
        span: Span,
        ins: &mut TypeStack,
        heap: &mut THeap,
        w_ins: &mut Vec<TermId>,
        w_outs: &mut Vec<TermId>,
    ) -> Result<(), Error> {
        if let Some(ty) = ins.pop(heap) {
            let u64 = self.engine.insert(TypeInfo::U64);
            if let Err(msg) = self.engine.unify(u64, ty) {
                return error(
                    span,
                    ErrorKind::UnificationError(msg),
                    "`syscall1` intrinsic expects a u64 as it's input",
                );
            }
            w_ins.push(ty);
            for _ in 0..i {
                if let Some(ty) = ins.pop(heap) {
                    w_ins.push(ty);
                } else {
                    return error(
                        span,
                        ErrorKind::NotEnoughData,
                        "Not enough data in the stack for `syscall1` intrinsic",
                    );
                }
            }
            w_outs.push(u64);
            ins.push(heap, u64);
            Ok(())
        } else {
            error(
                span,
                ErrorKind::NotEnoughData,
                "Not enough data in the stack for `print` intrinsic",
            )
        }
    }

    fn register_var(&mut self, path: ItemPathBuf, var: hir::Var) -> Result<(), Error> {
        if self.known_gvars.contains_key(&path) {
            return Ok(());
        }
        let hir::Var { ty, span } = var;
        let ty = self.abstract_to_concrete_type(&ty, None)?;
        let ty = self.engine.insert_unknown_generics(&ty);

        if self.engine.is_real(ty) {
            self.known_gvars.insert(path, ty);

            Ok(())
        } else {
            error(
                span,
                ErrorKind::UnificationError("This `var` has incomplete type".to_string()),
                "Generic types in global variables must be parametrised",
            )
        }
    }

    fn register_const(&mut self, path: ItemPathBuf, const_: hir::Const) -> Result<(), Error> {
        if self.checked_consts.contains_key(&path) {
            return Ok(());
        }

        let outs = const_
            .outs
            .iter()
            .map(|ty| self.abstract_to_concrete_type(ty, None))
            .collect::<Result<Vec<_>, _>>()?;

        let mut heap = THeap::default();
        let mut ts = TypeStack::default();
        let body = self.typecheck_body(
            &mut ts,
            &mut heap,
            &const_.body,
            &Default::default(),
            None,
            None,
            None,
            true,
        )?;

        let known_const = KConst {
            outs,
            body,
            span: const_.span,
        };
        self.checked_consts.insert(path, Rc::new(known_const));
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
        let proc = self.known_procs.get(path).map(|p| (**p).clone()).unwrap();

        let mut ins = TypeStack::from_iter(proc.ins.iter().copied(), heap);
        let outs = TypeStack::from_iter(proc.outs.iter().copied(), heap);

        let body = self.typecheck_body(
            &mut ins,
            heap,
            &proc.body,
            &proc.vars,
            None,
            Some(&proc.generics),
            Some(&outs),
            false,
        )?;

        if let Err(msg) = self.engine.unify_stacks(heap, outs, ins) {
            return error(
                proc.span,
                ErrorKind::UnificationError(dbg! {msg}),
                "Expected proc outputs do not match actual outputs",
            );
        }

        self.checked_procs.insert(
            path.to_owned(),
            KProc {
                generics: proc.generics,
                vars: proc.vars,
                span: proc.span,
                ins: proc.ins,
                outs: proc.outs,
                body,
            },
        );
        Ok(())
    }

    fn typecheck_call(
        &mut self,
        ins: &mut TypeStack,
        heap: &mut simplearena::Heap<typecheck::TypeFrame, 0>,
        proc: Rc<KProc<Spanned<Hir>>>,
        proc_name: &ItemPath,
        span: Span,
        in_const: bool,
    ) -> Result<TypedNode<TermId, Intrinsic<TermId>>, Error> {
        if in_const {
            return error(
                span,
                ErrorKind::CallInConst,
                "Calls in constant bodies are not supported",
            );
        }
        let (ins, outs) = {
            let generics = proc
                .generics
                .values()
                .map(|g| (*g, self.engine.insert(TypeInfo::Unknown)))
                .collect();
            let expected_ins = proc
                .ins
                .iter()
                .map(|term| self.engine.anonymize_generics(*term, &generics))
                .rev()
                .collect::<Vec<_>>();
            let snap = ins.clone();
            let estack = TypeStack::from_iter(expected_ins.iter().copied(), heap);
            for expected_ty in &expected_ins {
                if let Some(actual_t) = ins.pop(heap) {
                    if let Err(msg) = self.engine.unify(*expected_ty, actual_t) {
                        return error(
                            span,
                            ErrorKind::UnificationError(msg),
                            format!(
                                "Expected proc invocation inputs do not match actual stack:\nExpected: {:?}\nActual: {:?}",
                                self.engine.debug_stack(estack, heap),
                                self.engine.debug_stack(snap, heap)
                            ),
                        );
                    }
                } else {
                    return error(
                        span,
                        ErrorKind::NotEnoughData,
                        format!("Not enough data in the stack for `{proc_name:?}` proc"),
                    );
                }
            }
            let outs = proc
                .outs
                .iter()
                .map(|term| self.engine.anonymize_generics(*term, &generics))
                .collect::<Vec<_>>();
            for out_t in &outs {
                ins.push(heap, *out_t);
            }
            (expected_ins, outs)
        };

        Ok(TypedNode {
            span,
            node: TypedIr::Call(proc_name.to_owned()),
            ins,
            outs,
        })
    }

    fn typecheck_const_use(
        &mut self,
        ins: &mut TypeStack,
        heap: &mut simplearena::Heap<typecheck::TypeFrame, 0>,
        const_: Rc<KConst<TirNode>>,
        const_name: &ItemPath,
        span: Span,
    ) -> TypedNode<TermId, Intrinsic<TermId>> {
        let outs: Vec<TermId> = const_
            .outs
            .iter()
            .map(|term| self.engine.insert(term))
            .collect();

        // dbg! {const_name};
        // dbg! {&const_.outs};

        for ty in &outs {
            ins.push(heap, *ty);
        }

        // dbg! {&outs};
        // eprintln!();
        // eprintln!();
        TypedNode {
            span,
            node: TypedIr::ConstUse(const_name.to_owned()),
            ins: vec![],
            outs,
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn typecheck_body(
        &mut self,
        ins: &mut TypeStack,
        heap: &mut THeap,
        body: &[Spanned<Hir>],
        vars: &FnvHashMap<ItemPathBuf, Var<TermId>>,
        bindings_in_scope: Option<&FnvHashMap<ItemPathBuf, TermId>>,
        generics: Option<&FnvHashMap<SmolStr, GenId>>,
        expected_outs: Option<&TypeStack>,
        in_const: bool,
    ) -> Result<Vec<TirNode>, Error> {
        let mut checked_body = Vec::new();
        for node in body {
            let node = match &node.inner {
                Hir::Path(path) if bindings_in_scope.and_then(|bs| bs.get(path)).is_some() => {
                    let ty = bindings_in_scope
                        .and_then(|bs| bs.get(path))
                        .copied()
                        .unwrap();
                    ins.push(heap, ty);
                    TypedNode {
                        span: node.span,
                        node: TypedIr::BindingUse(path.clone()),
                        ins: vec![],
                        outs: vec![ty],
                    }
                }
                Hir::Path(path) if vars.contains_key(path) => {
                    let var = vars.get(path).unwrap();
                    let ty = self.engine.insert(TypeInfo::Ptr(var.ty));
                    ins.push(heap, ty);
                    TypedNode {
                        span: node.span,
                        node: TypedIr::LVarUse(path.clone()),
                        ins: vec![],
                        outs: vec![ty],
                    }
                }
                Hir::Path(path) if self.is_mem(path) => {
                    let ty = self.engine.insert(TypeInfo::U8);
                    let ty = self.engine.insert(TypeInfo::Ptr(ty));
                    ins.push(heap, ty);
                    TypedNode {
                        span: node.span,
                        node: TypedIr::MemUse(path.clone()),
                        ins: vec![],
                        outs: vec![ty],
                    }
                }
                Hir::Path(path) if self.is_var(path) => {
                    let var = self.known_gvars.get(path).unwrap();
                    let ty = self.engine.insert(TypeInfo::Ptr(*var));
                    ins.push(heap, ty);
                    TypedNode {
                        span: node.span,
                        node: TypedIr::GVarUse(path.clone()),
                        ins: vec![],
                        outs: vec![ty],
                    }
                }
                Hir::Path(path) if self.item_refs.contains_key(path) => {
                    match self.item_refs.get(path).cloned().unwrap() {
                        Ref::Proc(proc_name) => {
                            let proc = self.known_procs.get(&proc_name).cloned().unwrap();
                            self.typecheck_call(ins, heap, proc, &proc_name, node.span, in_const)?
                        }
                        Ref::Mem(_) => {
                            let ty = self.engine.insert(TypeInfo::U8);
                            let ty = self.engine.insert(TypeInfo::Ptr(ty));
                            ins.push(heap, ty);
                            TypedNode {
                                span: node.span,
                                node: TypedIr::MemUse(path.clone()),
                                ins: vec![],
                                outs: vec![ty],
                            }
                        }
                        Ref::Const(const_name) => {
                            let const_ = self.checked_consts.get(&const_name).cloned().unwrap();
                            self.typecheck_const_use(ins, heap, const_, &const_name, node.span)
                        }
                        Ref::Struct(_) => todo!(),
                        Ref::Var => todo!(),
                    }
                }
                Hir::Path(proc_name) if self.is_proc(proc_name) => {
                    let proc = self.known_procs.get(proc_name).cloned().unwrap();
                    self.typecheck_call(ins, heap, proc, proc_name, node.span, in_const)?
                }
                Hir::Path(const_name) if self.is_const(const_name) => {
                    let const_ = self.checked_consts.get(const_name).cloned().unwrap();
                    self.typecheck_const_use(ins, heap, const_, const_name, node.span)
                }
                Hir::Path(path) => {
                    return error(
                        node.span,
                        ErrorKind::Undefined,
                        format!("Undefined word `{path:?}`"),
                    )
                }
                Hir::Intrinsic(i) => {
                    let (mut w_ins, mut w_outs) = (Vec::new(), Vec::new());
                    let i = match i {
                        Intrinsic::Drop => {
                            if let Some(ty) = ins.pop(heap) {
                                w_ins.push(ty);
                                Intrinsic::Drop
                            } else {
                                return error(
                                    node.span,
                                    ErrorKind::NotEnoughData,
                                    "Not enough data in the stack for `drop` intrinsic",
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
                                Intrinsic::Dup
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
                                    Intrinsic::Swap
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
                                    Intrinsic::Over
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
                            let o_type = self.engine.insert(&o_type);
                            if let Some(in_t) = ins.pop(heap) {
                                ins.push(heap, o_type);
                                w_ins.push(in_t);
                                w_outs.push(o_type);
                                Intrinsic::Cast(o_ty.map_ref(|_| o_type))
                            } else {
                                return error(
                                    node.span,
                                    ErrorKind::NotEnoughData,
                                    "Not enough data in the stack for `cast` intrinsic",
                                );
                            }
                        }
                        Intrinsic::Read(ty) => {
                            if let Some(actual) = ins.pop(heap) {
                                let ety = self.abstract_to_concrete_type(ty, generics)?;
                                let ety = self.engine.insert_unknown_generics(&ety);

                                let eptr = self.engine.insert(TypeInfo::Ptr(ety));
                                if let Err(msg) = self.engine.unify(eptr, actual) {
                                    return error(
                                        node.span,
                                        ErrorKind::UnificationError(msg),
                                        format!(
                                            "Read intrinsic expects a pointer to {:?}",
                                            &ty.inner
                                        ),
                                    );
                                } else {
                                    ins.push(heap, ety);
                                    w_ins.push(eptr);
                                    w_outs.push(ety);
                                    Intrinsic::Read(ty.map_ref(|_| ety))
                                }
                            } else {
                                return error(
                                    node.span,
                                    ErrorKind::NotEnoughData,
                                    "Not enough data in the stack for `!u64` intrinsic",
                                );
                            }
                        }
                        Intrinsic::Write(ty) => {
                            if let Some(ty1) = ins.pop(heap) {
                                let ety = self.abstract_to_concrete_type(ty, generics)?;
                                let ety = self.engine.insert_unknown_generics(&ety);
                                let eptr = self.engine.insert(TypeInfo::Ptr(ety));
                                if let Err(msg) = self.engine.unify(eptr, ty1) {
                                    return error(
                                        node.span,
                                        ErrorKind::UnificationError(msg),
                                        format!(
                                            "Write intrinsic expects a pointer to {:?}",
                                            &ty.inner
                                        ),
                                    );
                                }
                                if let Some(ty2) = ins.pop(heap) {
                                    if let Err(msg) = self.engine.unify(ety, ty2) {
                                        return error(
                                            node.span,
                                            ErrorKind::UnificationError(msg),
                                            "Write intrinsic expects a u64",
                                        );
                                    }
                                    w_ins.push(ty1);
                                    w_ins.push(ty2);
                                    Intrinsic::Write(ty.map_ref(|_| ety))
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
                        Intrinsic::CompStop => {
                            dbg! {&self.engine};
                            return error(
                                node.span,
                                ErrorKind::CompStop,
                                format!(
                                    "Stack at this point:\n{:?}",
                                    ins.clone()
                                        .into_vec(heap)
                                        .into_iter()
                                        .map(|t| self.engine.term_to_string(t))
                                        .collect::<Vec<_>>()
                                ),
                            );
                        }
                        Intrinsic::Dump => todo!(),
                        Intrinsic::Print => {
                            if let Some(ty) = ins.pop(heap) {
                                w_ins.push(ty);
                                Intrinsic::Print
                            } else {
                                return error(
                                    node.span,
                                    ErrorKind::NotEnoughData,
                                    "Not enough data in the stack for `print` intrinsic",
                                );
                            }
                        }
                        Intrinsic::Syscall0 => {
                            self.typecheck_syscall(
                                0,
                                node.span,
                                ins,
                                heap,
                                &mut w_ins,
                                &mut w_outs,
                            )?;
                            Intrinsic::Syscall0
                        }
                        Intrinsic::Syscall1 => {
                            self.typecheck_syscall(
                                1,
                                node.span,
                                ins,
                                heap,
                                &mut w_ins,
                                &mut w_outs,
                            )?;
                            Intrinsic::Syscall1
                        }
                        Intrinsic::Syscall2 => {
                            self.typecheck_syscall(
                                2,
                                node.span,
                                ins,
                                heap,
                                &mut w_ins,
                                &mut w_outs,
                            )?;
                            Intrinsic::Syscall2
                        }
                        Intrinsic::Syscall3 => {
                            self.typecheck_syscall(
                                3,
                                node.span,
                                ins,
                                heap,
                                &mut w_ins,
                                &mut w_outs,
                            )?;
                            Intrinsic::Syscall3
                        }
                        Intrinsic::Syscall4 => {
                            self.typecheck_syscall(
                                4,
                                node.span,
                                ins,
                                heap,
                                &mut w_ins,
                                &mut w_outs,
                            )?;
                            Intrinsic::Syscall4
                        }
                        Intrinsic::Syscall5 => {
                            self.typecheck_syscall(
                                5,
                                node.span,
                                ins,
                                heap,
                                &mut w_ins,
                                &mut w_outs,
                            )?;
                            Intrinsic::Syscall5
                        }
                        Intrinsic::Syscall6 => {
                            self.typecheck_syscall(
                                6,
                                node.span,
                                ins,
                                heap,
                                &mut w_ins,
                                &mut w_outs,
                            )?;
                            Intrinsic::Syscall6
                        }
                        Intrinsic::Argc => {
                            let ty = self.engine.insert(TypeInfo::U64);
                            ins.push(heap, ty);
                            w_outs.push(ty);
                            Intrinsic::Argc
                        }
                        Intrinsic::Argv => {
                            let char = self.engine.insert(TypeInfo::Char);
                            let charp = self.engine.insert(TypeInfo::Ptr(char));
                            let charpp = self.engine.insert(TypeInfo::Ptr(charp));
                            ins.push(heap, charpp);
                            w_outs.push(charpp);
                            Intrinsic::Argv
                        }
                        Intrinsic::Add => {
                            if let Some(a) = ins.pop(heap) {
                                if let Some(b) = ins.pop(heap) {
                                    if let Err(msg) = self.engine.unify(a, b) {
                                        return error(
                                            node.span,
                                            ErrorKind::UnificationError(msg),
                                            "Add intrinsic expects it's inputs to be of the same type",
                                        );
                                    }
                                    w_ins.push(a);
                                    w_ins.push(a);
                                    w_outs.push(a);
                                    ins.push(heap, a);
                                    Intrinsic::Add
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
                        Intrinsic::Sub => {
                            if let Some(a) = ins.pop(heap) {
                                if let Some(b) = ins.pop(heap) {
                                    if let Err(msg) = self.engine.unify(a, b) {
                                        return error(
                                            node.span,
                                            ErrorKind::UnificationError(msg),
                                            "Sub intrinsic expects it's inputs to be of the same type",
                                        );
                                    }
                                    w_ins.push(a);
                                    w_ins.push(a);
                                    w_outs.push(a);
                                    ins.push(heap, a);
                                    Intrinsic::Sub
                                } else {
                                    return error(
                                        node.span,
                                        ErrorKind::NotEnoughData,
                                        "Not enough data in the stack for `sub` intrinsic",
                                    );
                                }
                            } else {
                                return error(
                                    node.span,
                                    ErrorKind::NotEnoughData,
                                    "Not enough data in the stack for `sub` intrinsic",
                                );
                            }
                        }
                        Intrinsic::Divmod => {
                            if let Some(a) = ins.pop(heap) {
                                if let Some(b) = ins.pop(heap) {
                                    if let Err(msg) = self.engine.unify(a, b) {
                                        return error(
                                            node.span,
                                            ErrorKind::UnificationError(msg),
                                            "`divmod` intrinsic expects it's inputs to be of the same type",
                                        );
                                    }
                                    w_ins.push(a);
                                    w_ins.push(a);
                                    w_outs.push(a);
                                    w_outs.push(a);
                                    ins.push(heap, a);
                                    ins.push(heap, a);
                                    Intrinsic::Divmod
                                } else {
                                    return error(
                                        node.span,
                                        ErrorKind::NotEnoughData,
                                        "Not enough data in the stack for `divmod` intrinsic",
                                    );
                                }
                            } else {
                                return error(
                                    node.span,
                                    ErrorKind::NotEnoughData,
                                    "Not enough data in the stack for `divmod` intrinsic",
                                );
                            }
                        }
                        Intrinsic::Mul => {
                            if let Some(a) = ins.pop(heap) {
                                if let Some(b) = ins.pop(heap) {
                                    if let Err(msg) = self.engine.unify(a, b) {
                                        return error(
                                            node.span,
                                            ErrorKind::UnificationError(msg),
                                            "`mul` intrinsic expects it's inputs to be of the same type",
                                        );
                                    }
                                    w_ins.push(a);
                                    w_ins.push(a);
                                    w_outs.push(a);
                                    ins.push(heap, a);
                                    Intrinsic::Mul
                                } else {
                                    return error(
                                        node.span,
                                        ErrorKind::NotEnoughData,
                                        "Not enough data in the stack for `mul` intrinsic",
                                    );
                                }
                            } else {
                                return error(
                                    node.span,
                                    ErrorKind::NotEnoughData,
                                    "Not enough data in the stack for `mul` intrinsic",
                                );
                            }
                        }
                        Intrinsic::Eq => {
                            if let Some(a) = ins.pop(heap) {
                                if let Some(b) = ins.pop(heap) {
                                    if let Err(msg) = self.engine.unify(a, b) {
                                        return error(
                                            node.span,
                                            ErrorKind::UnificationError(msg),
                                            "Eq intrinsic expects it's inputs to be of the same type",
                                        );
                                    }
                                    let bool = self.engine.insert(TypeInfo::Bool);
                                    ins.push(heap, bool);
                                    Intrinsic::Eq
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
                        Intrinsic::Ne => {
                            if let Some(a) = ins.pop(heap) {
                                if let Some(b) = ins.pop(heap) {
                                    if let Err(msg) = self.engine.unify(a, b) {
                                        return error(
                                            node.span,
                                            ErrorKind::UnificationError(msg),
                                            "Eq intrinsic expects it's inputs to be of the same type",
                                        );
                                    }
                                    let bool = self.engine.insert(TypeInfo::Bool);
                                    ins.push(heap, bool);
                                    Intrinsic::Ne
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
                        Intrinsic::Lt => {
                            if let Some(a) = ins.pop(heap) {
                                if let Some(b) = ins.pop(heap) {
                                    if let Err(msg) = self.engine.unify(a, b) {
                                        return error(
                                        node.span,
                                        ErrorKind::UnificationError(msg),
                                        "`lt` intrinsic expects it's inputs to be of the same type",
                                    );
                                    }
                                    let bool = self.engine.insert(TypeInfo::Bool);
                                    ins.push(heap, bool);
                                    Intrinsic::Lt
                                } else {
                                    return error(
                                        node.span,
                                        ErrorKind::NotEnoughData,
                                        "Not enough data in the stack for `lt` intrinsic",
                                    );
                                }
                            } else {
                                return error(
                                    node.span,
                                    ErrorKind::NotEnoughData,
                                    "Not enough data in the stack for `lt` intrinsic",
                                );
                            }
                        }
                        Intrinsic::Le => {
                            if let Some(a) = ins.pop(heap) {
                                if let Some(b) = ins.pop(heap) {
                                    if let Err(msg) = self.engine.unify(a, b) {
                                        return error(
                                        node.span,
                                        ErrorKind::UnificationError(msg),
                                        "`le` intrinsic expects it's inputs to be of the same type",
                                    );
                                    }
                                    let bool = self.engine.insert(TypeInfo::Bool);
                                    ins.push(heap, bool);
                                    Intrinsic::Le
                                } else {
                                    return error(
                                        node.span,
                                        ErrorKind::NotEnoughData,
                                        "Not enough data in the stack for `le` intrinsic",
                                    );
                                }
                            } else {
                                return error(
                                    node.span,
                                    ErrorKind::NotEnoughData,
                                    "Not enough data in the stack for `le` intrinsic",
                                );
                            }
                        }
                        Intrinsic::Gt => {
                            if let Some(a) = ins.pop(heap) {
                                if let Some(b) = ins.pop(heap) {
                                    if let Err(msg) = self.engine.unify(a, b) {
                                        return error(
                                        node.span,
                                        ErrorKind::UnificationError(msg),
                                        "`gt` intrinsic expects it's inputs to be of the same type",
                                    );
                                    }
                                    let bool = self.engine.insert(TypeInfo::Bool);
                                    ins.push(heap, bool);
                                    Intrinsic::Gt
                                } else {
                                    return error(
                                        node.span,
                                        ErrorKind::NotEnoughData,
                                        "Not enough data in the stack for `gt` intrinsic",
                                    );
                                }
                            } else {
                                return error(
                                    node.span,
                                    ErrorKind::NotEnoughData,
                                    "Not enough data in the stack for `gt` intrinsic",
                                );
                            }
                        }
                        Intrinsic::Ge => {
                            if let Some(a) = ins.pop(heap) {
                                if let Some(b) = ins.pop(heap) {
                                    if let Err(msg) = self.engine.unify(a, b) {
                                        return error(
                                        node.span,
                                        ErrorKind::UnificationError(msg),
                                        "`ge` intrinsic expects it's inputs to be of the same type",
                                    );
                                    }
                                    let bool = self.engine.insert(TypeInfo::Bool);
                                    ins.push(heap, bool);
                                    Intrinsic::Ge
                                } else {
                                    return error(
                                        node.span,
                                        ErrorKind::NotEnoughData,
                                        "Not enough data in the stack for `ge` intrinsic",
                                    );
                                }
                            } else {
                                return error(
                                    node.span,
                                    ErrorKind::NotEnoughData,
                                    "Not enough data in the stack for `ge` intrinsic",
                                );
                            }
                        }
                    };
                    TypedNode {
                        span: node.span,
                        node: TypedIr::Intrinsic(i),
                        ins: w_ins,
                        outs: w_outs,
                    }
                }
                Hir::Bind(hir::Bind { bindings, body }) => {
                    let mut w_ins = Vec::new();
                    let mut bindings_hash = bindings
                        .iter()
                        .rev()
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
                                            .map(|ty| self.engine.insert(&ty));

                                        if let Ok(ty2) = ty2 {
                                            if let Err(msg) = self.engine.unify(ty, ty2) {
                                                return Some(error(
                                                    *span,
                                                    ErrorKind::UnificationError(msg),
                                                    "Binding type mismatch",
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
                    if let Some(bindings) = bindings_in_scope {
                        for (name, ty) in bindings {
                            if !bindings_hash.contains_key(name) {
                                bindings_hash.insert(name.clone(), *ty);
                            }
                        }
                    }
                    let body = self.typecheck_body(
                        ins,
                        heap,
                        body,
                        vars,
                        Some(&bindings_hash),
                        generics,
                        expected_outs,
                        in_const,
                    )?;
                    let mut a_ins = ins.clone().into_vec(heap).into_iter();
                    for _ in b_ins {
                        a_ins.next();
                    }
                    let w_outs: Vec<TermId> = a_ins.collect();
                    let n: TirNode = TypedNode {
                        span: node.span,
                        node: TypedIr::Bind(Bind {
                            bindings: bindings.clone(),
                            body,
                        }),
                        ins: w_ins,
                        outs: w_outs,
                    };
                    n
                }
                Hir::While(hir::While { cond, body }) => {
                    let mut ins_c = ins.clone();
                    let cond = self.typecheck_body(
                        &mut ins_c,
                        heap,
                        cond,
                        vars,
                        bindings_in_scope,
                        generics,
                        expected_outs,
                        in_const,
                    )?;
                    if let Some(ty) = ins_c.pop(heap) {
                        let bool = self.engine.insert(TypeInfo::Bool);
                        if let Err(msg) = self.engine.unify(ty, bool) {
                            return error(
                                node.span,
                                ErrorKind::UnificationError(msg),
                                "While condition expects to consume a bool",
                            );
                        }
                        if let Err(msg) = self.engine.unify_stacks(heap, ins.clone(), ins_c) {
                            return error(
                                node.span,
                                ErrorKind::UnificationError(msg),
                                "While condition must leave the stack in the same state",
                            );
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
                        vars,
                        bindings_in_scope,
                        generics,
                        expected_outs,
                        in_const,
                    )?;
                    if let Err(msg) = self.engine.unify_stacks(heap, ins.clone(), ins_b) {
                        return error(
                            node.span,
                            ErrorKind::UnificationError(msg),
                            "While body must leave the stack in the same state",
                        );
                    };

                    TypedNode {
                        span: node.span,
                        node: TypedIr::While(While { cond, body }),
                        ins: vec![],
                        outs: vec![],
                    }
                }
                Hir::If(hir::If { truth, lie }) => {
                    let bool = self.engine.insert(TypeInfo::Bool);
                    let b = ins.pop(heap).map(|b| self.engine.unify(b, bool));
                    if let Some(Err(msg)) = b {
                        return error(
                            node.span,
                            ErrorKind::UnificationError(msg),
                            "`if` expects to consume a bool",
                        );
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
                        vars,
                        bindings_in_scope,
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
                            vars,
                            bindings_in_scope,
                            generics,
                            expected_outs,
                            in_const,
                        )?;

                        if let Err(msg) = self.engine.unify_stacks(heap, ins_t.clone(), ins_l) {
                            return error(
                                node.span,
                                ErrorKind::UnificationError(msg),
                                "If branches must have identical outputs",
                            );
                        }
                        *ins = ins_t;
                        Some(lie)
                    } else {
                        None
                    };

                    TypedNode {
                        span: node.span,
                        node: TypedIr::If(If { truth, lie }),
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
                                    self.engine.insert(TypeInfo::Bool),
                                    TypedIr::Literal(l.clone()),
                                ),
                                Literal::Int(_) => (
                                    self.engine.insert(TypeInfo::I64),
                                    TypedIr::Literal(l.clone()),
                                ),
                                Literal::UInt(_) => (
                                    self.engine.insert(TypeInfo::U64),
                                    TypedIr::Literal(l.clone()),
                                ),
                                Literal::String(_) => todo!(),
                                Literal::Char(_) => (
                                    self.engine.insert(TypeInfo::Char),
                                    TypedIr::Literal(l.clone()),
                                ),
                            },
                            Hir::IgnorePattern => (
                                self.engine.insert(TypeInfo::Unknown),
                                TypedIr::IgnorePattern,
                            ),
                            Hir::Path(const_name) if self.is_const(const_name) => {
                                let const_ = self.checked_consts.get(const_name).cloned().unwrap();
                                let mut ts = TypeStack::default();
                                let r = self.typecheck_const_use(
                                    &mut ts,
                                    heap,
                                    const_,
                                    const_name,
                                    pattern.span,
                                );
                                (r.outs[0], TypedIr::ConstUse(const_name.clone()))
                            }
                            _ => todo!("Illegal?? pattern"),
                        };
                        let tir_pattern = TypedNode {
                            span: pattern.span,
                            node: tir_pat,
                            ins: vec![],
                            outs: vec![pat_ty],
                        };
                        if let Err(msg) = self.engine.unify(ty, pat_ty) {
                            return error(
                                pattern.span,
                                ErrorKind::UnificationError(msg),
                                "Pattern type does not match the input type",
                            );
                        }
                        let mut ins_b = ins.clone();
                        let body = self.typecheck_body(
                            &mut ins_b,
                            heap,
                            body,
                            vars,
                            bindings_in_scope,
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
                    let mut msg = String::new();
                    if bodies.array_windows::<2>().any(|[a, b]| {
                        if let Err(emsg) = self.engine.unify_stacks(heap, a.clone(), b.clone()) {
                            msg.push_str(&format!("{emsg}\n"));
                            true
                        } else {
                            false
                        }
                    }) {
                        return error(
                            node.span,
                            ErrorKind::UnificationError(msg),
                            "Cond branches must have equal effect on the stack",
                        );
                    }
                    *ins = bodies.pop().unwrap();

                    TypedNode {
                        span: node.span,
                        node: TypedIr::Cond(Cond {
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
                            let bool = self.engine.insert(TypeInfo::Bool);
                            ins.push(heap, bool);
                            w_outs.push(bool)
                        }
                        Literal::Int(_) => {
                            let u64 = self.engine.insert(TypeInfo::I64);
                            ins.push(heap, u64);
                            w_outs.push(u64)
                        }
                        Literal::UInt(_) => {
                            let u64 = self.engine.insert(TypeInfo::U64);
                            ins.push(heap, u64);
                            w_outs.push(u64)
                        }
                        Literal::String(_) => {
                            let char = self.engine.insert(TypeInfo::Char);
                            let cptr = self.engine.ptr_to(&Type::Concrete(char));
                            let u64 = self.engine.insert(TypeInfo::U64);
                            ins.push(heap, u64);
                            ins.push(heap, cptr);
                            w_outs.push(u64);
                            w_outs.push(cptr)
                        }
                        Literal::Char(_) => {
                            let char = self.engine.insert(TypeInfo::Char);
                            ins.push(heap, char);
                            w_outs.push(char)
                        }
                    };
                    TypedNode {
                        span: node.span,
                        node: TypedIr::Literal(l.clone()),
                        ins: vec![],
                        outs: w_outs,
                    }
                }
                Hir::IgnorePattern => todo!(),
                Hir::Return => {
                    if let Some(expected) = expected_outs {
                        if let Err(msg) =
                            self.engine
                                .unify_stacks(heap, expected.clone(), ins.clone())
                        {
                            return error(
                                node.span,
                                ErrorKind::UnificationError(msg),
                                "The stack at this point does not match expected proc outputs",
                            );
                        } else {
                            for _ in expected.clone().into_vec(heap) {
                                ins.pop(heap);
                            }
                        }
                    }
                    TypedNode {
                        span: node.span,
                        node: TypedIr::Return,
                        ins: ins.clone().into_vec(heap),
                        outs: vec![],
                    }
                }
                Hir::FieldAccess(hir::FieldAccess { field }) => {
                    let (in_ty, out_ty) = if let Some(ty) = ins.pop(heap) {
                        let field_ty = self.engine.get_struct_field_through_ptr(ty, field).unwrap();

                        let out_ty = self.engine.insert(TypeInfo::Ptr(field_ty));

                        ins.push(heap, out_ty);
                        (ty, out_ty)
                    } else {
                        return error(
                            node.span,
                            ErrorKind::NotEnoughData,
                            "There is no struct pointer on the stack",
                        );
                    };

                    TypedNode {
                        span: node.span,
                        node: TypedIr::FieldAccess(FieldAccess {
                            field: field.clone(),
                            ty: in_ty,
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

    fn gen_id(&mut self) -> GenId {
        let id = GenId(self.next_gen_id);
        self.next_gen_id += 1;
        id
    }
    fn abstract_to_concrete_type(
        &mut self,
        ty: &Spanned<types::Type>,
        generics: Option<&FnvHashMap<SmolStr, GenId>>,
    ) -> Result<Type, Error> {
        let span = ty.span;
        let kind = match &ty.inner {
            types::Type::Ptr(box t) => {
                let ty = self.abstract_to_concrete_type(
                    &Spanned {
                        span,
                        inner: t.clone(),
                    },
                    generics,
                )?;
                Type::Ptr(Box::new(ty))
            }
            types::Type::Primitive(ty) => Type::Concrete(self.engine.insert(*ty)),

            types::Type::Custom(types::Custom { name, params }) => {
                let ty = if let Some(ty) = self.known_structs.get(name) {
                    ty.clone()
                } else if let Some(Ref::Struct(ty)) = self.item_refs.get(name) {
                    self.known_structs.get(ty).unwrap().clone()
                } else if let Some(r) = self.unresloved_item_refs.get(name) {
                    if let Ref::Struct(ty) = self.resolve(r.clone()) {
                        self.known_structs.get(&ty).unwrap().clone()
                    } else {
                        todo!("This is a error yo")
                    }
                } else if let Some(ty) = self.structs.get(name).cloned() {
                    let mut generics_hash: FnvHashMap<SmolStr, GenId> = Default::default();
                    let mut generics: Vec<GenId> = Default::default();
                    for (name, gid) in ty.generics.into_iter().map(|ty| (ty.inner, self.gen_id())) {
                        generics.push(gid);
                        generics_hash.insert(name, gid);
                    }
                    let mut fields = FnvHashMap::default();
                    for (n, ty) in ty.fields {
                        let ty = self.abstract_to_concrete_type(&ty, Some(&generics_hash))?;
                        fields.insert(n, ty);
                    }
                    let s = Rc::new(KStruct {
                        generics,
                        fields,
                        typename: ty.name.clone(),
                    });
                    self.known_structs.insert(ty.name, s.clone());
                    s
                } else if let Some(g) = name.only() {
                    if let Some(id) = generics.and_then(|gs| gs.get(g)) {
                        return Ok(Type::Generic(*id));
                    } else {
                        return error(
                            span,
                            ErrorKind::Undefined,
                            "This type name is not found in the current scope",
                        );
                    }
                } else {
                    return error(
                        span,
                        ErrorKind::Undefined,
                        "This type name is not found in the current scope",
                    );
                };
                let pid = self.engine.insert_proto(ty.clone());
                if ty.generics.is_empty() {
                    let tid = self.engine.instantiate(pid, &Default::default());
                    Type::Concrete(tid)
                } else if let Some(params) = params {
                    let subs = ty
                        .generics
                        .iter()
                        .zip(params.tys.iter())
                        .map(|(gen, param)| {
                            let ty = self.abstract_to_concrete_type(param, generics)?;
                            let ty = self.engine.insert(&ty);
                            Ok((*gen, ty))
                        })
                        .collect::<Result<_, _>>()?;

                    let tid = self.engine.instantiate(pid, &subs);
                    Type::Concrete(tid)
                } else {
                    let tid = self.engine.instantiate(pid, &Default::default());
                    Type::Concrete(tid)
                }
            }
        };

        kind.okay()
    }

    // fn instantiate_type(
    //     &mut self,
    //     name: &ItemPath,
    //     params: impl Iterator<Item = types::Type>,
    // ) -> Result<Type, Error> {
    //     fn substitute_and_rename(
    //         proto: &types::Struct,
    //         params: impl Iterator<Item = types::Type>,
    //     ) -> types::Struct {
    //         fn find_generic(t: &mut types::Type, gen: &str, param: types::Type) {
    //             match t {
    //                 types::Type::Ptr(box r) => find_generic(r, gen, param),
    //                 types::Type::Custom(types::Custom { name, params: None }) => {
    //                     if let Some(n) = name.only() {
    //                         if n == gen {
    //                             *t = param;
    //                         }
    //                     }
    //                 }
    //                 types::Type::Custom(types::Custom {
    //                     name: _,
    //                     params: Some(_),
    //                 }) => {
    //                     todo!()
    //                 }
    //                 types::Type::Primitive(_) => (),
    //             }
    //         }
    //         let mut proto2 = proto.clone();
    //         let mut paramstr = String::from("[");
    //         for (i, (param, gen)) in params.zip(proto.generics.iter()).enumerate() {
    //             if i == 0 {
    //                 write!(paramstr, "{:?}", param).unwrap();
    //             } else {
    //                 write!(paramstr, " {:?}", param).unwrap();
    //             }
    //             for t in proto2.fields.values_mut() {
    //                 find_generic(t, &*gen.inner, param.clone())
    //             }
    //         }
    //         write!(paramstr, "]").unwrap();
    //         proto2.name.push(paramstr);
    //         proto2
    //     }

    //     let proto = self.structs.get(name).unwrap();
    //     let proto2 = substitute_and_rename(proto, params);
    //     let mut fields = FnvHashMap::default();
    //     for (n, ty) in proto2.fields {
    //         let ty = self.abstract_to_concrete_type(&ty, None)?;
    //         fields.insert(n, ty);
    //     }
    //     let id = if let Some(s) = self.known_structs.get(&proto2.name) {
    //         s.id
    //     } else {
    //         let generics = proto2
    //             .generics
    //             .iter()
    //             .map(|s| (s.inner.clone(), self.gen_id()))
    //             .collect();
    //         let id = self.type_id();
    //         self.known_structs.insert(
    //             proto2.name.clone(),
    //             Rc::new(KStruct {
    //                 fields,
    //                 typename: proto2.name,
    //                 generics,
    //             }),
    //         );
    //         id
    //     };

    //     Ok(Type::Concrete(ConcreteType::Custom(id)))
    //     todo!()
    // }

    fn is_proc(&self, name: &ItemPath) -> bool {
        self.known_procs.contains_key(name)
    }

    fn is_const(&self, name: &ItemPath) -> bool {
        self.checked_consts.contains_key(name)
    }

    fn is_mem(&self, name: &ItemPath) -> bool {
        self.known_mems.contains_key(name)
    }

    fn is_var(&self, name: &ItemPath) -> bool {
        self.known_gvars.contains_key(name)
    }
}
