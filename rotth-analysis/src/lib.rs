#![feature(assert_matches)]
#![feature(iter_intersperse)]
#![feature(box_syntax, box_patterns)]
#![feature(string_remove_matches)]
#![feature(type_alias_impl_trait)]
#![feature(array_windows)]

use ctir::ConcreteError;
use inference::TypeInfo;
use spanner::Span;

pub mod ctir;
pub mod inference;
pub mod tir;
pub mod typecheck;

pub fn error<T, M: ToString>(span: Span, kind: ErrorKind, message: M) -> Result<T, Error> {
    Err(Error::new(Some(span), kind, message))
}

pub fn concrete_error<T, M: ToString>(
    span: Option<Span>,
    kind: ConcreteError,
    message: M,
) -> Result<T, Error> {
    Err(Error::new(span, ErrorKind::Concrete(kind), message))
}

#[derive(Debug)]
pub struct Error {
    pub span: Option<Span>,
    pub kind: ErrorKind,
    pub message: String,
}

impl Error {
    fn new(span: Option<Span>, kind: ErrorKind, message: impl ToString) -> Error {
        Error {
            span,
            kind,
            message: message.to_string(),
        }
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    TypeMismatch {
        expected: Vec<TypeInfo>,
        actual: Vec<TypeInfo>,
    },
    UnificationError(String),
    UnsupportedOperation,
    NotEnoughData,
    Undefined,
    InvalidMain,
    InvalidWhile,
    CompStop,
    Unexpected,
    CallInConst,
    Concrete(ConcreteError),
}
