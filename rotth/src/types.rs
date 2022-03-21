use crate::ast::TopLevel;
use fnv::FnvHashMap;

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Type {
    pub ptr_depth: usize,
    pub value_type: ValueType,
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{:?}",
            "&>".repeat(self.ptr_depth as usize),
            self.value_type
        )
    }
}
impl Type {
    pub const BOOL: Self = Type {
        ptr_depth: 0,
        value_type: ValueType::Primitive(Primitive::Bool),
    };

    pub const CHAR: Self = Type {
        ptr_depth: 0,
        value_type: ValueType::Primitive(Primitive::Char),
    };

    pub const U64: Self = Type {
        ptr_depth: 0,
        value_type: ValueType::Primitive(Primitive::U64),
    };
    pub const U32: Self = Type {
        ptr_depth: 0,
        value_type: ValueType::Primitive(Primitive::U32),
    };
    pub const U16: Self = Type {
        ptr_depth: 0,
        value_type: ValueType::Primitive(Primitive::U16),
    };
    pub const U8: Self = Type {
        ptr_depth: 0,
        value_type: ValueType::Primitive(Primitive::U8),
    };

    pub const I64: Self = Type {
        ptr_depth: 0,
        value_type: ValueType::Primitive(Primitive::I64),
    };
    pub const I32: Self = Type {
        ptr_depth: 0,
        value_type: ValueType::Primitive(Primitive::I32),
    };
    pub const I16: Self = Type {
        ptr_depth: 0,
        value_type: ValueType::Primitive(Primitive::I16),
    };
    pub const I8: Self = Type {
        ptr_depth: 0,
        value_type: ValueType::Primitive(Primitive::I8),
    };

    pub const ANY: Self = Type {
        ptr_depth: 0,
        value_type: ValueType::Any,
    };

    pub fn ptr_to(ty: Self) -> Self {
        let ptr_depth = ty.ptr_depth + 1;
        Self {
            ptr_depth,
            value_type: ty.value_type,
        }
    }

    pub fn pointee(&self) -> Self {
        assert!(self.is_ptr());
        let ptr_depth = self.ptr_depth - 1;
        Self {
            ptr_depth,
            value_type: self.value_type,
        }
    }

    pub fn is_ptr(&self) -> bool {
        self.ptr_depth > 0
    }

    pub fn is_ptr_to(&self, ty: Self) -> bool {
        self.is_ptr()
            && self.ptr_depth.saturating_sub(1) == ty.ptr_depth
            && self.value_type == ty.value_type
    }

    pub fn size(&self, struct_index: &StructIndex) -> usize {
        if self.ptr_depth > 0 {
            8
        } else {
            match self.value_type {
                ValueType::Primitive(p) => p.size(),
                ValueType::Any => unreachable!("Naked any type"),
                ValueType::Struct(s) => struct_index[s].size,
            }
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ValueType {
    Primitive(Primitive),
    Struct(StructId),
    Any,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Primitive {
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
    fn size(&self) -> usize {
        match self {
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
    fields: FnvHashMap<String, Type>,
    name: String,
}

impl<'i> StructBuilder<'i> {
    pub fn field(&mut self, name: String, ty: Type) -> &mut Self {
        self.fields.insert(name, ty);
        self
    }
    pub fn finish(self) -> StructId {
        let mut fields = FnvHashMap::default();
        let mut curr_offset = 0;
        for (name, ty) in self.fields {
            let field = Field {
                ty,
                offset: curr_offset,
            };
            curr_offset += ty.size(self.index);
            fields.insert(name, field);
        }

        let struct_ = Struct {
            name: self.name,
            fields,
            size: curr_offset,
        };
        let id = self.index.structs.len();
        self.index.structs.push(struct_);
        StructId(id)
    }
}

#[derive(Default)]
pub struct StructIndex {
    structs: Vec<Struct>,
}

impl StructIndex {
    pub fn new_struct(&'_ mut self, name: String) -> StructBuilder<'_> {
        StructBuilder {
            index: self,
            fields: Default::default(),
            name,
        }
    }
    pub fn known(&self, name: &str) -> bool {
        self.structs.iter().any(|s| s.name == name)
    }
    pub fn id_names(&'_ self) -> impl Iterator<Item = (StructId, &'_ str)> {
        self.structs
            .iter()
            .enumerate()
            .map(|(i, s)| (StructId(i), &*s.name))
    }
    pub fn name_to_id(&self, name: &str) -> Option<StructId> {
        self.id_names()
            .find_map(|(i, n)| if n == name { Some(i) } else { None })
    }
}

impl std::ops::Index<StructId> for StructIndex {
    type Output = Struct;

    fn index(&self, index: StructId) -> &Self::Output {
        &self.structs[index.0]
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct StructId(usize);

#[derive(Debug, PartialEq, Eq)]
pub struct Struct {
    pub name: String,
    pub fields: FnvHashMap<String, Field>,
    pub size: usize,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Field {
    pub ty: Type,
    pub offset: usize,
}

pub fn define_structs(structs: FnvHashMap<String, TopLevel>) -> StructIndex {
    let mut index = StructIndex::default();
    for (name, struct_) in structs {
        if let TopLevel::Struct(s) = &struct_ {
            let mut builder = index.new_struct(name);
            for field in &s.body {
                let field = coerce_ast!(field => REF StructField || unreachable!());
                let name = coerce_ast!(field.name => REF Word || unreachable!());
                let ty = coerce_ast!(field.ty => REF Type || unreachable!())
                    .clone()
                    .to_primitive_type();

                builder.field(name.clone(), ty);
            }
            builder.finish();
        } else {
            unreachable!();
        }
    }
    index
}
