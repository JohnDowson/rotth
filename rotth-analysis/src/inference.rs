use fnv::FnvHashMap;
use rotth_parser::{ast::ItemPathBuf, types::Primitive};
use smol_str::SmolStr;
use std::fmt::Write;
use std::rc::Rc;

use crate::{
    ctir::{CStruct, Field},
    tir::{ConcreteType, GenId, KStruct, Type, TypeId},
    typecheck::{THeap, TypeStack},
};
///! # Type inference in less than 100 lines of Rust
///!
///! - Do with it what you will
///! - Licensed under (https://en.wikipedia.org/wiki/WTFPL)
///!
///! ~ zesterer

/// A identifier to uniquely refer to our type terms
#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub struct TermId(usize);

impl std::fmt::Debug for TermId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TermId({})", self.0)
    }
}

/// Information about a type term
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
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

    Struct(TypeId),
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
#[derive(Debug)]
pub struct StructInfo {
    pub fields: FnvHashMap<SmolStr, TermId>,
}

pub fn type_to_info_with_generic_substitution_table(
    engine: &mut Engine,
    generics: &mut FnvHashMap<GenId, TermId>,
    structs: &FnvHashMap<ItemPathBuf, Rc<KStruct>>,
    term: &Type,
) -> TypeInfo {
    let mut type_to_id = |term| {
        let info = type_to_info_with_generic_substitution_table(engine, generics, structs, term);
        engine.insert(info)
    };
    let find_struct = |id| {
        structs
            .iter()
            .find_map(|(_, s)| if s.id == id { Some(s) } else { None })
            .unwrap()
    };
    match term {
        Type::Generic(g) => {
            if let Some(id) = generics.get(g) {
                TypeInfo::Ref(*id)
            } else {
                let id = engine.insert(TypeInfo::Unknown);
                generics.insert(*g, id);
                TypeInfo::Ref(id)
            }
        }
        Type::Concrete(c) => match c {
            ConcreteType::Ptr(box t) => TypeInfo::Ptr(type_to_id(t)),
            ConcreteType::Primitive(p) => match p {
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
            },
            ConcreteType::Custom(t) => {
                let s = find_struct(*t);

                let fields = s
                    .fields
                    .iter()
                    .map(|(n, t)| {
                        let t = type_to_info_with_generic_substitution_table(
                            engine, generics, structs, t,
                        );
                        let t = engine.insert(t);
                        (n.clone(), t)
                    })
                    .collect();
                let s = StructInfo { fields };
                engine.structs.insert(*t, s);

                TypeInfo::Struct(*t)
            }
        },
    }
}

pub fn type_to_info(
    engine: &mut Engine,
    structs: &FnvHashMap<ItemPathBuf, Rc<KStruct>>,
    term: &Type,
    generics_are_unknown: bool,
) -> TypeInfo {
    let mut type_to_id = |term| {
        let info = type_to_info(engine, structs, term, generics_are_unknown);
        engine.insert(info)
    };
    let find_struct = |id| {
        structs
            .iter()
            .find_map(|(_, s)| if s.id == id { Some(&**s) } else { None })
            .unwrap()
    };
    match term {
        Type::Generic(_) if generics_are_unknown => TypeInfo::Unknown,
        Type::Generic(g) => TypeInfo::Generic(*g),
        Type::Concrete(c) => match c {
            ConcreteType::Ptr(box t) => TypeInfo::Ptr(type_to_id(t)),
            ConcreteType::Primitive(p) => match p {
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
            },
            ConcreteType::Custom(t) => {
                let s = find_struct(*t);

                let fields = s
                    .fields
                    .iter()
                    .map(|(n, t)| {
                        let t = type_to_info(engine, structs, t, generics_are_unknown);
                        let t = engine.insert(t);
                        (n.clone(), t)
                    })
                    .collect();
                let s = StructInfo { fields };
                engine.structs.insert(*t, s);

                TypeInfo::Struct(*t)
            }
        },
    }
}

#[derive(Default)]
pub struct Engine {
    hash: FnvHashMap<TypeInfo, TermId>,
    structs: FnvHashMap<TypeId, StructInfo>,
    vars: Vec<TypeInfo>,
}

fn write_info(
    info: &TypeInfo,
    vars: &[TypeInfo],
    structs: &FnvHashMap<TypeId, StructInfo>,
    f: &mut impl Write,
) -> std::fmt::Result {
    match info {
        TypeInfo::Unknown => write!(f, "Unknown"),
        TypeInfo::Ref(id) => {
            write!(f, "Ref({:?})", id.0)
        }
        TypeInfo::Ptr(id) => {
            write!(f, "&>")?;
            write_info(&vars[id.0], vars, structs, f)
        }
        TypeInfo::Generic(g) => {
            write!(f, "{:?}", g)
        }
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
        TypeInfo::Struct(s) => {
            writeln!(f, "struct {:?}: {{", s)?;
            for (n, t) in &structs[s].fields {
                write!(f, "\t\t{n}: ")?;
                write_info(&vars[t.0], vars, structs, f)?;
                writeln!(f)?;
            }
            write!(f, "\t}}")
        }
    }
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
                write!(f, "\t{:?}: ", id)?;
                write_info(info, &self.vars, &self.structs, f)?;
                write!(f, ", ")?;
                writeln!(f)?;
            } else {
                write!(f, "{:?}: {:?},", id, info)?;
            }
        }

        write!(f, "}}")
    }
}

impl Engine {
    /// Create a new type term with whatever we have about its type
    pub fn insert(&mut self, info: TypeInfo) -> TermId {
        match info {
            TypeInfo::Unknown => {
                let id = TermId(self.vars.len());
                self.hash.insert(info, id);
                self.vars.push(info);
                id
            }
            info => {
                if let Some(id) = self.hash.get(&info) {
                    *id
                } else {
                    let id = TermId(self.vars.len());
                    self.hash.insert(info, id);
                    self.vars.push(info);
                    id
                }
            }
        }
    }

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
                        .map(|t| self.reconstruct(t))
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
                        .map(|t| self.reconstruct(t))
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
            (Struct(a_id), Struct(b_id)) if a_id == b_id => Ok(()),
            (Ptr(a), Ptr(b)) => self.unify(a, b),

            // If no previous attempts to unify were successful, raise an error
            (_, _) => Err(format!(
                "Conflict between {:?} and {:?}",
                self.reconstruct(a),
                self.reconstruct(b)
            )),
        }
    }

    pub fn get_struct(&self, id: TermId) -> Result<&StructInfo, String> {
        use TypeInfo::*;
        match self.vars[id.0] {
            Struct(id) => Ok(&self.structs[&id]),
            _ => Err(format!("{id:?} is not a struct")),
        }
    }

    pub fn get_struct_ptr(&self, id: TermId) -> Result<&StructInfo, String> {
        use TypeInfo::*;
        match self.vars[id.0] {
            Ptr(id) => self.get_struct(id),
            _ => Err(format!("{id:?} is not a pointer")),
        }
    }

    /// Attempt to reconstruct a concrete type from the given type term ID. This
    /// may fail if we don't yet have enough information to figure out what the
    /// type is.
    pub fn reconstruct(&self, id: TermId) -> Result<Type, String> {
        use TypeInfo::*;
        match self.vars[id.0] {
            Unknown => Err("Cannot infer".to_string()),
            Ref(id) => self.reconstruct(id),
            Ptr(v) => Ok(Type::Concrete(ConcreteType::Ptr(box self.reconstruct(v)?))),
            Generic(id) => Ok(Type::Generic(id)),
            Void => Ok(Type::VOID),
            Bool => Ok(Type::BOOL),
            Char => Ok(Type::CHAR),
            U64 => Ok(Type::U64),
            U32 => Ok(Type::U32),
            U16 => Ok(Type::U16),
            U8 => Ok(Type::U8),
            I64 => Ok(Type::I64),
            I32 => Ok(Type::I32),
            I16 => Ok(Type::I16),
            I8 => Ok(Type::I8),
            Struct(id) => Ok(Type::Concrete(ConcreteType::Custom(id))),
        }
    }

    pub fn reconstruct_lossy(&self, id: TermId) -> Type {
        use TypeInfo::*;
        match self.vars[id.0] {
            Unknown => Type::VOID,
            Ref(id) => self.reconstruct_lossy(id),
            Ptr(v) => Type::Concrete(ConcreteType::Ptr(box self.reconstruct_lossy(v))),
            Generic(id) => Type::Generic(id),
            Void => Type::VOID,
            Bool => Type::BOOL,
            Char => Type::CHAR,
            U64 => Type::U64,
            U32 => Type::U32,
            U16 => Type::U16,
            U8 => Type::U8,
            I64 => Type::I64,
            I32 => Type::I32,
            I16 => Type::I16,
            I8 => Type::I8,
            Struct(id) => Type::Concrete(ConcreteType::Custom(id)),
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
            Ptr(v) => Ok(ReifiedType::Ptr(box self.reify(subs, v)?)),
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
            Struct(id) => {
                let mut offset = 0;
                let fields = self.structs[&id]
                    .fields
                    .iter()
                    .map(|(n, t)| {
                        let ty = self.reify(subs, *t)?;

                        let f_offset = offset;
                        offset += ty.size();
                        Ok((
                            n.clone(),
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

    pub fn debug_stack(&self, stack: TypeStack, heap: &THeap) -> String {
        let mut b = String::new();
        for t in stack.into_vec(heap).into_iter() {
            write_info(&self.vars[t.0], &self.vars, &self.structs, &mut b).unwrap();
            write!(&mut b, ", ").unwrap();
        }
        b
    }
}

// # Example usage
// In reality, the most common approach will be to walk your AST, assigning type
// terms to each of your nodes with whatever information you have available. You
// will also need to call `engine.unify(x, y)` when you know two nodes have the
// same type, such as in the statement `x = y;`.
#[test]
fn test() {
    let mut engine = Engine::default();

    // proc [T] incptr &>T : &>T
    let var = engine.insert(TypeInfo::Unknown);

    let i = engine.insert(TypeInfo::Ptr(var));
    let o = engine.insert(TypeInfo::Ptr(var));

    // 0 cast &>u64 incptr
    // let u = engine.insert(TypeInfo::U64);
    let u = engine.insert(TypeInfo::Struct(TypeId(0)));
    let a = engine.insert(TypeInfo::Ptr(u));
    engine.unify(a, i).unwrap();

    // ...and compute the resulting type
    panic!("Final type = {:?}", engine.reconstruct(o));
}
