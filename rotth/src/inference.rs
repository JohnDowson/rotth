use fnv::FnvHashMap;
use rotth_parser::types::Primitive;

use crate::{
    tir::{ConcreteType, GenId, Type, TypeId},
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

pub fn type_to_info_with_generic_substitution_table(
    engine: &mut Engine,
    generics: &mut FnvHashMap<GenId, TermId>,
    term: &Type,
) -> TypeInfo {
    let mut type_to_id = |term| {
        let info = type_to_info_with_generic_substitution_table(engine, generics, term);
        engine.insert(info)
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
            ConcreteType::Custom(t) => TypeInfo::Struct(*t),
        },
    }
}

pub fn type_to_info(engine: &mut Engine, term: &Type, generics_are_unknown: bool) -> TypeInfo {
    let mut type_to_id = |term| {
        let info = type_to_info(engine, term, generics_are_unknown);
        engine.insert(info)
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
            ConcreteType::Custom(t) => TypeInfo::Struct(*t),
        },
    }
}

#[derive(Default)]
pub struct Engine {
    hash: FnvHashMap<TypeInfo, TermId>,
    vars: Vec<TypeInfo>,
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
                writeln!(f, "\t{:?}: {:?},", id, info)?;
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

    pub fn reconstruct_substituting(
        &self,
        subs: &FnvHashMap<GenId, Type>,
        id: TermId,
    ) -> Result<Type, String> {
        use TypeInfo::*;
        match self.vars[id.0] {
            Unknown => Err("Cannot infer".to_string()),
            Ref(id) => self.reconstruct_substituting(subs, id),
            Ptr(v) => Ok(Type::Concrete(ConcreteType::Ptr(
                box self.reconstruct_substituting(subs, v)?,
            ))),
            Generic(id) => {
                if let Some(t) = subs.get(&id) {
                    Ok(t.clone())
                } else {
                    Err("Cannot substitute generic".to_string())
                }
            }
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
