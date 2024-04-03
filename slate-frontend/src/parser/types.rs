use crate::parser::ast::GenericParams;
use itempath::{path, ItemPathBuf};
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
                    for (i, param) in params.inner.tys.iter().enumerate() {
                        if i == 0 {
                            write!(paramstr, "{:?}", param.inner.type_name()).unwrap();
                        } else {
                            write!(paramstr, " {:?}", param.inner.type_name()).unwrap();
                        }
                    }
                    write!(paramstr, "]").unwrap();
                    base.push(paramstr.into());
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
