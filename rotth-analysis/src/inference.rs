use fnv::FnvHashMap;
use internment::Intern;
use itempath::ItemPathBuf;
use rotth_parser::types::Primitive;
use std::{fmt::Write, rc::Rc};

use crate::{
    ctir::{CStruct, Field},
    tir::{GenId, KStruct, Type},
    typecheck::{THeap, TypeStack},
};

/// A identifier to uniquely refer to our type terms
#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub struct TermId(usize);

impl TermId {
    pub fn debug(&self, engine: &Engine) -> String {
        let mut b = String::new();
        engine.write_info(&engine.vars[self.0], &mut b).unwrap();
        b
    }
}

impl std::fmt::Debug for TermId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TermId({})", self.0)
    }
}
/// A identifier to uniquely refer to struct prototypes,
/// i.e. Vec is a prototype for Vec[T]
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct ProtoId(usize);
#[derive(Debug)]
pub struct StructProto {
    pub generics: Vec<GenId>,
    pub fields: FnvHashMap<Intern<String>, TermId>,
}

/// A identifier to uniquely refer to instantiated struct,
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct InstId(usize);
#[derive(Debug, Clone)]
pub struct StructInst {
    pub fields: FnvHashMap<Intern<String>, TermId>,
}

/// Information about a type term
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypeInfo {
    // No information about the type of this type term
    Unknown,
    // This type term is the same as another type term
    Ref(TermId),

    Ptr(TermId),

    Generic(GenId),

    Void,
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

    Struct(ProtoId, InstId),
}

#[derive(Clone, Debug)]
pub enum ReifiedType {
    Ptr(Box<ReifiedType>),
    Primitive(Primitive),
    Custom(CStruct),
}

impl ReifiedType {
    pub const BOOL: Self = Self::Primitive(Primitive::Bool);
    pub const CHAR: Self = Self::Primitive(Primitive::Char);
    pub const U64: Self = Self::Primitive(Primitive::U64);
    pub const U32: Self = Self::Primitive(Primitive::U32);
    pub const U16: Self = Self::Primitive(Primitive::U16);
    pub const U8: Self = Self::Primitive(Primitive::U8);

    pub const I64: Self = Self::Primitive(Primitive::I64);
    pub const I32: Self = Self::Primitive(Primitive::I32);
    pub const I16: Self = Self::Primitive(Primitive::I16);
    pub const I8: Self = Self::Primitive(Primitive::I8);

    pub const VOID: Self = Self::Primitive(Primitive::Void);

    pub fn size(&self) -> usize {
        match self {
            ReifiedType::Ptr(_) => 8,
            ReifiedType::Primitive(p) => p.size(),
            ReifiedType::Custom(c) => c.size(),
        }
    }
}

#[derive(Default)]
pub struct Engine {
    vars: Vec<TypeInfo>,
    vars_hash: FnvHashMap<TypeInfo, TermId>,
    protos: Vec<StructProto>,
    protos_hash: FnvHashMap<ItemPathBuf, ProtoId>,
    insts: Vec<StructInst>,
}

impl std::fmt::Debug for Engine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            writeln!(f, "Engine {{")?;
        } else {
            write!(f, "Engine {{")?;
        }

        for (id, info) in self.vars.iter().enumerate() {
            if f.alternate() {
                write!(f, "\t{id:?}: ")?;
                self.write_info(info, f)?;
                write!(f, ", ")?;
                writeln!(f)?;
            } else {
                write!(f, "{id:?}: {info:?},")?;
            }
        }

        write!(f, "}}")
    }
}

pub trait Insert<T> {
    fn insert(&mut self, ty: T) -> TermId;
    fn insert_unknown_generics(&mut self, ty: T) -> TermId;
}

impl Insert<&Type> for Engine {
    fn insert(&mut self, ty: &Type) -> TermId {
        match ty {
            Type::Generic(g) => self.insert(TypeInfo::Generic(*g)),
            Type::Concrete(c) => *c,
            Type::Ptr(box ty) => self.ptr_to(ty),
        }
    }

    fn insert_unknown_generics(&mut self, ty: &Type) -> TermId {
        match ty {
            Type::Generic(_) => self.insert(TypeInfo::Unknown),
            _ => self.insert(ty),
        }
    }
}

impl Insert<TypeInfo> for Engine {
    fn insert(&mut self, info: TypeInfo) -> TermId {
        match info {
            TypeInfo::Unknown => {
                let id = TermId(self.vars.len());
                self.vars_hash.insert(info, id);
                self.vars.push(info);
                id
            }
            info => {
                if let Some(id) = self.vars_hash.get(&info) {
                    *id
                } else {
                    let id = TermId(self.vars.len());
                    self.vars_hash.insert(info, id);
                    self.vars.push(info);
                    id
                }
            }
        }
    }
    fn insert_unknown_generics(&mut self, info: TypeInfo) -> TermId {
        match info {
            TypeInfo::Generic(_) => self.insert(TypeInfo::Unknown),
            _ => self.insert(info),
        }
    }
}

impl Insert<Primitive> for Engine {
    fn insert(&mut self, prim: Primitive) -> TermId {
        let info = match prim {
            Primitive::Void => TypeInfo::Void,
            Primitive::Bool => TypeInfo::Bool,
            Primitive::Char => TypeInfo::Char,
            Primitive::U64 => TypeInfo::U64,
            Primitive::U32 => TypeInfo::U32,
            Primitive::U16 => TypeInfo::U16,
            Primitive::U8 => TypeInfo::U8,
            Primitive::I64 => TypeInfo::I64,
            Primitive::I32 => TypeInfo::I32,
            Primitive::I16 => TypeInfo::I16,
            Primitive::I8 => TypeInfo::I8,
        };
        self.insert(info)
    }

    fn insert_unknown_generics(&mut self, prim: Primitive) -> TermId {
        self.insert(prim)
    }
}

impl Engine {
    pub fn get_struct_field_through_ptr(
        &self,
        term: TermId,
        field: Intern<String>,
    ) -> Option<TermId> {
        match &self.vars[term.0] {
            TypeInfo::Ptr(term) => match &self.vars[term.0] {
                TypeInfo::Struct(_, inst) => self.insts[inst.0].fields.get(&field).copied(),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn insert_proto(&mut self, proto: Rc<KStruct>) -> ProtoId {
        let KStruct {
            typename,
            generics,
            fields,
        } = &*proto;

        if let Some(id) = self.protos_hash.get(typename) {
            *id
        } else {
            let fields = fields
                .iter()
                .map(|(n, t)| {
                    let t = match t {
                        Type::Generic(g) => self.insert(TypeInfo::Generic(*g)),
                        Type::Concrete(t) => *t,
                        Type::Ptr(box ty) => self.ptr_to(ty),
                    };
                    (n.clone(), t)
                })
                .collect();

            let id = ProtoId(self.protos.len());
            self.protos_hash.insert(typename.clone(), id);
            self.protos.push(StructProto {
                generics: generics.clone(),
                fields,
            });
            id
        }
    }

    fn substitute_recursively(&mut self, term: TermId, subs: &FnvHashMap<GenId, TermId>) -> TermId {
        use TypeInfo::*;
        match self.vars[term.0] {
            Generic(gid) => {
                if let Some(t) = subs.get(&gid) {
                    *t
                } else {
                    self.insert(TypeInfo::Unknown)
                }
            }
            Ptr(term) => {
                let t = self.substitute_recursively(term, subs);
                self.insert(TypeInfo::Ptr(t))
            }
            Ref(_) => todo!(),
            Struct(pid, iid) => {
                let mut inst = self.insts[iid.0].clone();
                for term in inst.fields.values_mut() {
                    *term = self.substitute_recursively(*term, subs)
                }

                let iid = self.insts.len();
                self.insts.push(inst);
                self.insert(TypeInfo::Struct(pid, InstId(iid)))
            }
            _ => term,
        }
    }

    pub fn instantiate(&mut self, pid: ProtoId, subs: &FnvHashMap<GenId, TermId>) -> TermId {
        let proto = &self.protos[pid.0];

        let mut fields = proto.fields.clone();
        for term in fields.values_mut() {
            *term = self.substitute_recursively(*term, subs)
        }

        let iid = self.insts.len();
        self.insts.push(StructInst { fields });
        self.insert(TypeInfo::Struct(pid, InstId(iid)))
    }

    pub fn ptr_to(&mut self, ty: &Type) -> TermId {
        let t = match ty {
            Type::Generic(g) => self.insert(TypeInfo::Generic(*g)),
            Type::Concrete(t) => *t,
            Type::Ptr(box ty) => self.ptr_to(ty),
        };
        self.insert(TypeInfo::Ptr(t))
    }

    /// # Type inference in less than 100 lines of Rust
    ///
    /// - Do with it what you will
    /// - Licensed under (https://en.wikipedia.org/wiki/WTFPL)
    ///
    /// ~ zesterer
    pub fn unify_stacks(
        &mut self,
        heap: &mut THeap,
        mut expected: TypeStack,
        mut actual: TypeStack,
    ) -> Result<(), String> {
        let a_ty = expected.pop(heap);
        let b_ty = actual.pop(heap);
        match (a_ty, b_ty) {
            (None, None) => Ok(()),
            (None, Some(b_ty)) => Err(format!(
                "Stacks differ in depth, actual stack is {:?}, expected is []",
                {
                    actual.push(heap, b_ty);
                    actual
                        .into_vec(heap)
                        .into_iter()
                        .map(|t| self.term_to_string(t))
                        .collect::<Vec<_>>()
                }
            )),
            (Some(a_ty), None) => Err(format!(
                "Stacks differ in depth, expected stack is {:?}, actual is []",
                {
                    expected.push(heap, a_ty);
                    expected
                        .into_vec(heap)
                        .into_iter()
                        .map(|t| self.term_to_string(t))
                        .collect::<Vec<_>>()
                }
            )),
            (Some(a_ty), Some(b_ty)) => {
                self.unify(a_ty, b_ty)?;
                self.unify_stacks(heap, expected, actual)
            }
        }
    }

    /// Make the types of two type terms equivalent (or produce an error if
    /// there is a conflict between them)
    pub fn unify(&mut self, a: TermId, b: TermId) -> Result<(), String> {
        use TypeInfo::*;
        if a == b {
            return Ok(());
        }
        match (self.vars[a.0], self.vars[b.0]) {
            // Follow any references
            (Ref(a), Ref(b)) if a == b => Ok(()),
            (Ref(a), _) => self.unify(a, b),
            (_, Ref(b)) => self.unify(a, b),

            // When we don't know anything about either term, assume that
            // they match and make the one we know nothing about reference the
            // one we may know something about
            (Unknown, _) => {
                self.vars[a.0] = TypeInfo::Ref(b);
                Ok(())
            }
            (_, Unknown) => {
                self.vars[b.0] = TypeInfo::Ref(a);
                Ok(())
            }

            // Primitives are trivial to unify
            (Void, Void) => Ok(()),
            (Bool, Bool) => Ok(()),
            (Char, Char) => Ok(()),
            (U64, U64) => Ok(()),
            (U32, U32) => Ok(()),
            (U16, U16) => Ok(()),
            (U8, U8) => Ok(()),
            (I64, I64) => Ok(()),
            (I32, I32) => Ok(()),
            (I16, I16) => Ok(()),
            (I8, I8) => Ok(()),

            // When unifying complex types, we must check their sub-types. This
            // can be trivially implemented for tuples, sum types, etc.
            (Generic(a_id), Generic(b_id)) if a_id == b_id => Ok(()),
            (Struct(ap, ag), Struct(bp, bg)) if ap == bp => {
                let mut asubs = self.insts[ag.0].fields.iter().collect::<Vec<_>>();
                asubs.sort_by_key(|(name, _)| *name);
                // this gets around borrowship issue
                #[allow(clippy::needless_collect)]
                let asubs = asubs.into_iter().map(|(_, ty)| *ty).collect::<Vec<_>>();

                let mut bsubs = self.insts[bg.0].fields.iter().collect::<Vec<_>>();
                bsubs.sort_by_key(|(name, _)| *name);
                let bsubs = bsubs.into_iter().map(|(_, ty)| *ty).collect::<Vec<_>>();

                for (aty, bty) in asubs.into_iter().zip(bsubs) {
                    self.unify(aty, bty)?;
                }

                Ok(())
            }
            (Ptr(a), Ptr(b)) => self.unify(a, b),

            // If no previous attempts to unify were successful, raise an error
            (_, _) => Err(format!(
                "Conflict between {} and {}",
                self.term_to_string(a),
                self.term_to_string(b)
            )),
        }
    }

    pub fn term_to_string(&mut self, term: TermId) -> String {
        let mut buf = String::new();
        self.write_info(&self.vars[term.0], &mut buf).unwrap();
        buf
    }

    pub fn is_real(&self, id: TermId) -> bool {
        use TypeInfo::*;
        match self.vars[id.0] {
            Unknown => false,
            Ref(id) => self.is_real(id),
            Ptr(v) => self.is_real(v),
            Generic(_) => false,
            Void => true,
            Bool => true,
            Char => true,
            U64 => true,
            U32 => true,
            U16 => true,
            U8 => true,
            I64 => true,
            I32 => true,
            I16 => true,
            I8 => true,
            Struct(_, instid) => self.insts[instid.0]
                .fields
                .iter()
                .all(|(_, t)| self.is_real(*t)),
        }
    }

    pub fn reconstruct_lossy(&self, id: TermId) -> Type {
        use TypeInfo::*;
        match self.vars[id.0] {
            Unknown => todo!(),
            Ref(id) => self.reconstruct_lossy(id),
            Ptr(id) => Type::Ptr(Box::new(self.reconstruct_lossy(id))),
            Generic(g) => Type::Generic(g),
            Void => Type::Concrete(id),
            Bool => Type::Concrete(id),
            Char => Type::Concrete(id),
            U64 => Type::Concrete(id),
            U32 => Type::Concrete(id),
            U16 => Type::Concrete(id),
            U8 => Type::Concrete(id),
            I64 => Type::Concrete(id),
            I32 => Type::Concrete(id),
            I16 => Type::Concrete(id),
            I8 => Type::Concrete(id),
            Struct(_, _) => Type::Concrete(id),
        }
    }

    pub fn reify(
        &self,
        subs: &FnvHashMap<GenId, ReifiedType>,
        id: TermId,
    ) -> Result<ReifiedType, String> {
        use TypeInfo::*;
        match self.vars[id.0] {
            Unknown => Err("Cannot infer".to_string()),
            Ref(id) => self.reify(subs, id),
            Ptr(v) => Ok(ReifiedType::Ptr(Box::new(self.reify(subs, v)?))),
            Generic(id) => Ok(subs[&id].clone()),
            Void => Ok(ReifiedType::VOID),
            Bool => Ok(ReifiedType::BOOL),
            Char => Ok(ReifiedType::CHAR),
            U64 => Ok(ReifiedType::U64),
            U32 => Ok(ReifiedType::U32),
            U16 => Ok(ReifiedType::U16),
            U8 => Ok(ReifiedType::U8),
            I64 => Ok(ReifiedType::I64),
            I32 => Ok(ReifiedType::I32),
            I16 => Ok(ReifiedType::I16),
            I8 => Ok(ReifiedType::I8),
            Struct(_, inst) => {
                let mut offset = 0;
                let fields = self.insts[inst.0]
                    .fields
                    .iter()
                    .map(|(n, t)| {
                        let ty = self.reify(subs, *t)?;

                        let f_offset = offset;
                        offset += ty.size();
                        Ok((
                            *n,
                            Field {
                                ty,
                                offset: f_offset,
                            },
                        ))
                    })
                    .collect::<Result<_, String>>()?;
                Ok(ReifiedType::Custom(CStruct { fields }))
            }
        }
    }

    pub fn anonymize_generics(&mut self, term: TermId, subs: &FnvHashMap<GenId, TermId>) -> TermId {
        match self.vars[term.0] {
            TypeInfo::Generic(g) => subs[&g],
            TypeInfo::Ptr(id) => {
                let inner = self.anonymize_generics(id, subs);
                self.insert(TypeInfo::Ptr(inner))
            }
            _ => term,
        }
    }

    fn write_info(&self, info: &TypeInfo, f: &mut impl Write) -> std::fmt::Result {
        let proto_name = |id| {
            self.protos_hash
                .iter()
                .find_map(|(n, p)| if p == id { Some(n) } else { None })
                .unwrap()
        };
        match info {
            TypeInfo::Unknown => write!(f, "Unknown"),
            TypeInfo::Ref(id) => {
                write!(f, "Ref({:?})", id.0)
            }
            TypeInfo::Ptr(id) => {
                // write!(f, "&>{}", id.0)
                write!(f, "&>")?;
                if let TypeInfo::Struct(_, _) = &self.vars[id.0] {
                    write!(f, "{}", id.0)
                } else {
                    self.write_info(&self.vars[id.0], f)
                }
            }
            TypeInfo::Generic(g) => write!(f, "{g:?}"),
            TypeInfo::Void => write!(f, "void"),
            TypeInfo::Bool => write!(f, "bool"),
            TypeInfo::Char => write!(f, "char"),
            TypeInfo::U64 => write!(f, "u64"),
            TypeInfo::U32 => write!(f, "u32"),
            TypeInfo::U16 => write!(f, "u16"),
            TypeInfo::U8 => write!(f, "u8"),
            TypeInfo::I64 => write!(f, "i64"),
            TypeInfo::I32 => write!(f, "i32"),
            TypeInfo::I16 => write!(f, "i16"),
            TypeInfo::I8 => write!(f, "i8"),
            TypeInfo::Struct(p, g) => {
                writeln!(f, "struct {:?} {:?}: {{", proto_name(p), p)?;
                for (n, t) in &self.insts[g.0].fields {
                    write!(f, "\t\t{n}: ")?;
                    self.write_info(&self.vars[t.0], f)?;
                    writeln!(f)?;
                }
                write!(f, "\t}}")
            }
        }
    }

    pub fn debug_stack(&self, stack: TypeStack, heap: &THeap) -> String {
        let mut b = String::from("[ ");
        let stack = stack.into_vec(heap);
        let len = stack.len();
        for (i, t) in stack.into_iter().enumerate() {
            self.write_info(&self.vars[t.0], &mut b).unwrap();
            if i < len - 1 {
                write!(&mut b, ", ").unwrap();
            }
        }
        write!(&mut b, " ]").unwrap();
        b
    }
}
