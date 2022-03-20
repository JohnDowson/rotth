#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum IConst {
    Bool(bool),
    U64(u64),
    I64(i64),
    Char(char),
    Str(String),
    Ptr(u64),
}

impl IConst {
    pub fn as_bool(&self) -> Option<&bool> {
        if let Self::Bool(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_u64(&self) -> Option<&u64> {
        if let Self::U64(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_i64(&self) -> Option<&i64> {
        if let Self::I64(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_str(&self) -> Option<&String> {
        if let Self::Str(v) = self {
            Some(v)
        } else {
            None
        }
    }
}
