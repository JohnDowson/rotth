use crate::{
    ast,
    hir::{self, HirKind, HirNode},
    iconst::IConst,
    span::Span,
    typecheck::{error, ErrorKind::*, Result, THeap, TypeStack},
    types::{StructId, StructIndex},
};
use fnv::FnvHashMap;
use somok::Somok;

#[derive(Debug, Clone)]
pub enum TopLevel {
    Proc(),
    Const(),
    Mem(),
    Var(),
}
pub struct TirNode {
    ins: Vec<Type>,
    outs: Vec<Type>,
    hir: HirKind,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type {
    pub ptr_depth: usize,
    pub kind: TypeKind,
}

impl Type {
    const BOOL: Self = Self {
        ptr_depth: 0,
        kind: TypeKind::Concrete(ConcreteType::Bool),
    };
    const CHAR: Self = Self {
        ptr_depth: 0,
        kind: TypeKind::Concrete(ConcreteType::Char),
    };
    const U64: Self = Self {
        ptr_depth: 0,
        kind: TypeKind::Concrete(ConcreteType::U64),
    };

    pub fn ptr_to(ty: Self) -> Self {
        let ptr_depth = ty.ptr_depth + 1;
        Self {
            ptr_depth,
            kind: ty.kind,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeKind {
    Generic(String),
    Concrete(ConcreteType),
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConcreteType {
    Bool,
    Char,

    U64,
    U32,
    U16,
    U8,

    I64,
    I32,
    I16,
    I8,

    Struct(StructId),
}
#[derive(Debug, Clone)]
struct KProc {
    generics: Vec<String>,
    ins: Vec<Type>,
    outs: Vec<Type>,
    body: Vec<HirNode>,
}
struct KConst {
    outs: Vec<Type>,
    body: Vec<HirNode>,
}
struct KMem {}
struct KGVar {}

pub struct Walker {
    typechecked_items: FnvHashMap<String, TopLevel>,
    known_consts: FnvHashMap<String, KConst>,
    known_procs: FnvHashMap<String, KProc>,
    known_gvars: FnvHashMap<String, KGVar>,
    known_mems: FnvHashMap<String, KMem>,
    structs: StructIndex,
    outputs: FnvHashMap<String, hir::TopLevel>,
    items: FnvHashMap<String, hir::TopLevel>,
    heap: THeap,
}
impl Walker {
    pub fn walk(
        items: FnvHashMap<String, hir::TopLevel>,
    ) -> Result<FnvHashMap<String, hir::TopLevel>> {
        let mut this = Walker {
            typechecked_items: Default::default(),
            known_consts: Default::default(),
            known_procs: Default::default(),
            known_gvars: Default::default(),
            known_mems: Default::default(),
            structs: Default::default(),
            outputs: Default::default(),
            items: Default::default(),
            heap: Default::default(),
        };

        for (name, item) in &items {
            match item {
                hir::TopLevel::Proc(proc) => this.register_proc(name, proc.clone())?,
                hir::TopLevel::Const(const_) => this.register_const(name, const_.clone())?,
                hir::TopLevel::Mem(_) => todo!(),
                hir::TopLevel::Var(_) => todo!(),
            };
        }
        this.items = items;
        this.typecheck_proc("main", Default::default())?;
        this.outputs.okay()
    }

    fn register_const(&mut self, proc_name: &str, const_: hir::Const) -> Result<()> {
        let outs = const_
            .outs
            .into_iter()
            .map(|ty| self.abstract_to_concrete_type(ty, &[]))
            .collect::<Result<Vec<_>>>()?;

        let known_const = KConst {
            outs,
            body: const_.body,
        };

        self.known_consts.insert(proc_name.to_string(), known_const);
        ().okay()
    }

    fn register_proc(&mut self, proc_name: &str, proc: hir::Proc) -> Result<()> {
        let generics = proc.generics;
        let ins = proc
            .ins
            .into_iter()
            .map(|ty| self.abstract_to_concrete_type(ty, &generics))
            .collect::<Result<Vec<_>>>()?;
        let outs = proc
            .outs
            .into_iter()
            .map(|ty| self.abstract_to_concrete_type(ty, &generics))
            .collect::<Result<Vec<_>>>()?;

        if proc_name == "main" && !(ins.is_empty() || matches!(&outs[..], [Type::U64])) {
            return error(
                proc.span,
                InvalidMain,
                "Main must have no inputs and a single `u64` output",
            );
        }

        let known_proc = KProc {
            generics,
            ins,
            outs,
            body: proc.body,
        };

        self.known_procs.insert(proc_name.to_string(), known_proc);
        ().okay()
    }

    fn typecheck_proc(
        &mut self,
        proc_name: &str,
        type_substitutions: FnvHashMap<String, Type>,
    ) -> Result<()> {
        let proc = if let Some(proc) = self.known_procs.get(proc_name) {
            proc
        } else {
            return error(
                Span::point("", 0),
                Undefined,
                format!("Procedure `{}` is not defined", proc_name),
            );
        };
        let mut stack = TypeStack::default();
        for ty in &proc.ins {
            match &ty.kind {
                TypeKind::Generic(g) => {
                    if let Some(ty) = type_substitutions.get(g) {
                        stack.push(&mut self.heap, ty.clone())
                    } else {
                        let span = self.items[proc_name].span();
                        return error(
                            span,
                            Undefined,
                            format!("Undefined type `{}` in this scope", g),
                        );
                    }
                }
                TypeKind::Concrete(_) => stack.push(&mut self.heap, ty.clone()),
            }
        }

        self.typecheck_body(proc_name, proc.body.clone(), &mut stack)?;

        todo!()
    }

    fn typecheck_body(
        &mut self,
        item_name: &str,
        body: Vec<HirNode>,
        stack: &mut TypeStack,
    ) -> Result<()> {
        for node in body {
            match &node.hir {
                HirKind::Word(w) => match w.as_str() {
                    rec if rec == item_name && !self.is_proc(rec) => {
                        return error(node.span.clone(), Unexpected, "Recursive const refinition")
                    }
                    rec if rec == item_name => {}
                    proc_name if self.is_proc(proc_name) => {
                        let proc = &self.known_procs[proc_name];
                    }
                    const_name if self.is_const(const_name) => {}
                    _ => todo!(),
                },
                HirKind::Intrinsic(_) => todo!(),
                HirKind::Bind(_) => todo!(),
                HirKind::While(_) => todo!(),
                HirKind::If(_) => todo!(),
                HirKind::Cond(_) => todo!(),
                HirKind::Literal(ref c) => match c {
                    IConst::Bool(_) => {
                        stack.push(&mut self.heap, Type::BOOL);
                    }
                    IConst::U64(_) => {
                        stack.push(&mut self.heap, Type::U64);
                    }
                    IConst::I64(_) => todo!(),
                    IConst::Char(_) => {
                        stack.push(&mut self.heap, Type::CHAR);
                    }
                    IConst::Str(_) => {
                        stack.push(&mut self.heap, Type::U64);
                        stack.push(&mut self.heap, Type::ptr_to(Type::CHAR));
                    }
                    IConst::Ptr(_) => todo!(),
                },
                HirKind::IgnorePattern => todo!(),
                HirKind::Return => todo!(),
                HirKind::FieldAccess(_) => todo!(),
                HirKind::Type(_) => todo!(),
            }
        }
        ().okay()
    }

    fn abstract_to_concrete_type(&mut self, ty: HirNode, generics: &[String]) -> Result<Type> {
        let span = ty.span;
        let ty = if let HirKind::Type(ty) = ty.hir {
            ty
        } else {
            unreachable!()
        };
        let kind = match ty.type_name.as_str() {
            "bool" => TypeKind::Concrete(ConcreteType::Bool),
            "char" => TypeKind::Concrete(ConcreteType::Char),

            "u64" => TypeKind::Concrete(ConcreteType::U64),
            "u32" => TypeKind::Concrete(ConcreteType::U32),
            "u16" => TypeKind::Concrete(ConcreteType::U16),
            "u8" => TypeKind::Concrete(ConcreteType::U8),

            "i64" => TypeKind::Concrete(ConcreteType::I64),
            "i32" => TypeKind::Concrete(ConcreteType::I32),
            "i16" => TypeKind::Concrete(ConcreteType::I16),
            "i8" => TypeKind::Concrete(ConcreteType::I8),

            name if generics.contains(&ty.type_name) => TypeKind::Generic(ty.type_name),
            name if self.is_struct(name) => {
                TypeKind::Concrete(ConcreteType::Struct(self.structs.name_to_id(name).unwrap()))
            }
            name => {
                return error(
                    span,
                    Undefined,
                    format!("Cannot find type `{}` in this scope", name),
                )
            }
        };

        Type {
            ptr_depth: ty.ptr_count,
            kind,
        }
        .okay()
    }

    fn is_struct(&self, name: &str) -> bool {
        self.structs.known(name)
    }

    fn is_proc(&self, proc_name: &str) -> bool {
        todo!()
    }

    fn is_const(&self, const_name: &str) -> bool {
        todo!()
    }
}
