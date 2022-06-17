use crate::ast::{ItemPath, ItemPathBuf};
use fnv::FnvHashMap;
use smol_str::SmolStr;
use spanner::Spanned;

#[derive(Clone, Eq, PartialEq)]
pub enum Type {
    Ptr(Box<Self>),
    Primitive(Primitive),
    Custom(ItemPathBuf),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Primitive {
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
}

impl Primitive {
    pub fn size(&self) -> usize {
        match self {
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
        }
    }
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Ptr(box ty) => {
                write!(f, "&>")?;
                ty.fmt(f)
            }
            Type::Primitive(p) => p.fmt(f),
            Type::Custom(s) => s.fmt(f),
        }
    }
}

pub struct StructBuilder<'i> {
    index: &'i mut StructIndex,
    generics: Vec<Spanned<SmolStr>>,
    fields: FnvHashMap<SmolStr, Spanned<Type>>,
    name: ItemPathBuf,
}

impl<'i> StructBuilder<'i> {
    pub fn field(&mut self, name: SmolStr, ty: Spanned<Type>) -> &mut Self {
        self.fields.insert(name, ty);
        self
    }

    pub fn finish(self) {
        let struct_ = Struct {
            name: self.name,
            generics: self.generics,
            fields: self.fields,
        };
        self.index.structs.push(struct_);
    }
}

#[derive(Default, Debug, Clone)]
pub struct StructIndex {
    structs: Vec<Struct>,
}

impl StructIndex {
    pub fn new_struct(
        &'_ mut self,
        name: ItemPathBuf,
        generics: Vec<Spanned<SmolStr>>,
    ) -> StructBuilder<'_> {
        StructBuilder {
            index: self,
            fields: Default::default(),
            generics,
            name,
        }
    }

    pub fn get(&self, name: &ItemPath) -> Option<&Struct> {
        self.structs.iter().find(|s| s.name == name)
    }
}

impl IntoIterator for StructIndex {
    type Item = Struct;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.structs.into_iter()
    }
}

impl<'s> IntoIterator for &'s StructIndex {
    type Item = &'s Struct;

    type IntoIter = std::slice::Iter<'s, Struct>;

    fn into_iter(self) -> Self::IntoIter {
        self.structs.iter()
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Struct {
    pub name: ItemPathBuf,
    pub generics: Vec<Spanned<SmolStr>>,
    pub fields: FnvHashMap<SmolStr, Spanned<Type>>,
}
