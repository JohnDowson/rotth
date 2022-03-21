///! # Type inference in less than 100 lines of Rust
///!
///! - Do with it what you will
///! - Licensed under (https://en.wikipedia.org/wiki/WTFPL)
///!
///! ~ zesterer
use std::collections::HashMap;

use crate::types::{StructId, StructIndex, Type, ValueType};

/// A identifier to uniquely refer to our type terms
pub type TypeId = usize;

/// Information about a type term
#[derive(Clone, Copy, Debug)]
pub enum TypeInfo {
    // No information about the type of this type term
    Unknown,
    // This type term is the same as another type term
    Ref(TypeId),

    Ptr(TypeId),

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

#[derive(Default)]
pub struct Engine {
    id_counter: usize, // Used to generate unique IDs
    vars: HashMap<TypeId, TypeInfo>,
}

impl Engine {
    /// Create a new type term with whatever we have about its type
    pub fn insert(&mut self, info: TypeInfo) -> TypeId {
        // Generate a new ID for our type term
        self.id_counter += 1;
        let id = self.id_counter;
        self.vars.insert(id, info);
        id
    }

    /// Make the types of two type terms equivalent (or produce an error if
    /// there is a conflict between them)
    pub fn unify(&mut self, a: TypeId, b: TypeId, structs: &StructIndex) -> Result<(), String> {
        use TypeInfo::*;
        match (self.vars[&a], self.vars[&b]) {
            // Follow any references
            (Ref(a), _) => self.unify(a, b, structs),
            (_, Ref(b)) => self.unify(a, b, structs),

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
            (Struct(a_id), Struct(b_id)) if a_id == b_id => Ok(()),
            (Ptr(a), Ptr(b)) => self.unify(a, b, structs),

            // If no previous attempts to unify were successful, raise an error
            (a, b) => Err(format!("Conflict between {:?} and {:?}", a, b)),
        }
    }

    /// Attempt to reconstruct a concrete type from the given type term ID. This
    /// may fail if we don't yet have enough information to figure out what the
    /// type is.
    pub fn reconstruct(&self, id: TypeId) -> Result<Type, String> {
        use TypeInfo::*;
        match self.vars[&id] {
            Unknown => Err("Cannot infer".to_string()),
            Ref(id) => self.reconstruct(id),
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
            Struct(id) => Ok(Type {
                ptr_depth: 0,
                value_type: ValueType::Struct(id),
            }),
            Ptr(v) => {
                let mut depth = 1;
                let mut next = v;
                let ty = loop {
                    match self.vars[&next] {
                        Ptr(v) => {
                            next = v;
                            depth += 1;
                        }
                        ty => break ty,
                    }
                };
                let ty = match ty {
                    Unknown => return Err("Cannot infer".to_string()),
                    Ref(id) => self.reconstruct(id)?,
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
                    Struct(id) => Type {
                        ptr_depth: 0,
                        value_type: ValueType::Struct(id),
                    },
                    Ptr(_) => unreachable!(),
                };
                Ok(Type {
                    ptr_depth: depth,
                    value_type: ty.value_type,
                })
            }
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
    let structs = StructIndex::default();
    let mut engine = Engine::default();

    // proc {T} incptr &>T : &>T
    let var = engine.insert(TypeInfo::Unknown);

    let i = engine.insert(TypeInfo::Ptr(var));
    let o = engine.insert(TypeInfo::Ptr(var));

    // 0 cast &>u64 incptr
    let u = engine.insert(TypeInfo::U64);
    let a = engine.insert(TypeInfo::Ptr(u));
    engine.unify(a, i, &structs).unwrap();

    // ...and compute the resulting type
    panic!("Final type = {:?}", engine.reconstruct(o));
}
