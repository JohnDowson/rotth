#[derive(Copy, Clone, Eq)]
pub struct Type {
    ptr_depth: u8,
    value_type: ValueType,
}

impl std::hash::Hash for Type {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // unsound btw
        self.ptr_depth.hash(state);
        self.value_type.hash(state);
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        if self.value_type == ValueType::Any {
            true
        } else {
            self.value_type == other.value_type && self.ptr_depth == other.ptr_depth
        }
    }
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
        value_type: ValueType::Bool,
    };

    pub const CHAR: Self = Type {
        ptr_depth: 0,
        value_type: ValueType::Char,
    };

    pub const U64: Self = Type {
        ptr_depth: 0,
        value_type: ValueType::U64,
    };
    pub const U32: Self = Type {
        ptr_depth: 0,
        value_type: ValueType::U32,
    };
    pub const U16: Self = Type {
        ptr_depth: 0,
        value_type: ValueType::U16,
    };
    pub const U8: Self = Type {
        ptr_depth: 0,
        value_type: ValueType::U8,
    };

    pub const I64: Self = Type {
        ptr_depth: 0,
        value_type: ValueType::I64,
    };
    pub const I32: Self = Type {
        ptr_depth: 0,
        value_type: ValueType::I32,
    };
    pub const I16: Self = Type {
        ptr_depth: 0,
        value_type: ValueType::I16,
    };
    pub const I8: Self = Type {
        ptr_depth: 0,
        value_type: ValueType::I8,
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

    pub fn is_ptr(&self) -> bool {
        self.ptr_depth > 0
    }
    pub fn is_ptr_to(&self, ty: Self) -> bool {
        self.is_ptr()
            && Self {
                ptr_depth: self.ptr_depth.saturating_sub(1),
                value_type: self.value_type,
            } == ty
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum ValueType {
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

    Any,
}
