use crate::ast::GenericParams;
use fnv::FnvHashMap;
use itempath::{path, ItemPath, ItemPathBuf};
use smol_str::SmolStr;
use spanner::Spanned;
use std::fmt::Write;

#[derive(Clone, Eq, PartialEq)]
pub enum Type {
    Ptr(Box<Self>),
    Primitive(Primitive),
    Custom(Custom),
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.type_name().fmt(f)
    }
}

impl Type {
    pub fn type_name(&self) -> ItemPathBuf {
        match self {
            Type::Ptr(box ty) => {
                let mut name = ty.type_name();
                if let Some(s) = name.segment_mut(0) {
                    *s = format!("&>{s}").into();
                }
                name
            }
            Type::Primitive(p) => match p {
                Primitive::Void => path!(void),
                Primitive::Bool => path!(bool),
                Primitive::Char => path!(char),
                Primitive::U64 => path!(u64),
                Primitive::U32 => path!(u32),
                Primitive::U16 => path!(u16),
                Primitive::U8 => path!(u8),
                Primitive::I64 => path!(i64),
                Primitive::I32 => path!(i32),
                Primitive::I16 => path!(i16),
                Primitive::I8 => path!(i8),
            },
            Type::Custom(Custom { name, params }) => {
                let mut base = name.clone();
                if let Some(params) = params {
                    let mut paramstr = String::from("[");
                    for (i, param) in params.tys.iter().enumerate() {
                        if i == 0 {
                            write!(paramstr, "{:?}", param.type_name()).unwrap();
                        } else {
                            write!(paramstr, " {:?}", param.type_name()).unwrap();
                        }
                    }
                    write!(paramstr, "]").unwrap();
                    base.push(paramstr);
                }
                base
            }
        }
    }
}

#[derive(Clone, Eq, PartialEq)]
pub struct Custom {
    pub name: ItemPathBuf,
    pub params: Option<Spanned<GenericParams>>,
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
