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
use std::collections::HashMap;

/// A identifier to uniquely refer to our type terms
#[derive(Clone, Copy, Eq, PartialEq, Hash)]
pub struct TermId(usize);

impl std::fmt::Debug for TermId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TermId({})", self.0)
    }
}

/// Information about a type term
#[derive(Clone, Copy, Debug, PartialEq)]
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

#[derive(Default, Debug)]
pub struct Engine {
    id_counter: usize, // Used to generate unique IDs
    vars: HashMap<TermId, TypeInfo>,
}

impl Engine {
    /// Create a new type term with whatever we have about its type
    pub fn insert(&mut self, info: TypeInfo) -> TermId {
        // Generate a new ID for our type term
        let id = TermId(self.id_counter);
        self.id_counter += 1;
        self.vars.insert(id, info);
        id
    }

    pub fn unify_stacks(
        &mut self,
        heap: &THeap,
        mut a: TypeStack,
        mut b: TypeStack,
    ) -> Result<(), String> {
        let a_ty = a.pop(heap);
        let b_ty = b.pop(heap);
        match (a_ty, b_ty) {
            (None, None) => Ok(()),
            (None, Some(_)) | (Some(_), None) => Err(String::from("Stacks differ in depth")),
            (Some(a_ty), Some(b_ty)) => {
                self.unify(a_ty, b_ty)?;
                self.unify_stacks(heap, a, b)
            }
        }
    }

    /// Make the types of two type terms equivalent (or produce an error if
    /// there is a conflict between them)
    pub fn unify(&mut self, a: TermId, b: TermId) -> Result<(), String> {
        use TypeInfo::*;
        // dbg! {(a,b)};
        // dbg! {&self.vars};
        if a == b {
            return Ok(());
        }
        // match dbg! {(self.vars[&a], self.vars[&b])} {
        match (self.vars[&a], self.vars[&b]) {
            // Follow any references
            (Ref(a), Ref(b)) if a == b => Ok(()),
            (Ref(a), _) => self.unify(a, b),
            (_, Ref(b)) => self.unify(a, b),

            // When we don't know anything about either term, assume that
            // they match and make the one we know nothing about reference the
            // one we may know something about
            (Unknown, _) => {
                self.vars.insert(a, TypeInfo::Ref(b));
                Ok(())
            }
            (_, Unknown) => {
                self.vars.insert(b, TypeInfo::Ref(a));
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
            (a, b) => Err(format!("Conflict between {:?} and {:?}", a, b)),
        }
    }

    /// Attempt to reconstruct a concrete type from the given type term ID. This
    /// may fail if we don't yet have enough information to figure out what the
    /// type is.
    pub fn reconstruct(&self, id: TermId) -> Result<Type, String> {
        use TypeInfo::*;
        match self.vars[&id] {
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
