#![feature(assert_matches)]
#![feature(iter_intersperse)]
#![feature(box_patterns)]
#![feature(string_remove_matches)]
#![feature(type_alias_impl_trait)]
#![feature(array_windows)]

pub mod emit;
// pub mod eval;
// pub mod lir;
pub mod lir2;

#[macro_export]
macro_rules! ops {
    [$($op:ident $(($($arg:expr),+))?),* $(,)?] => {
        vec![$(
            SpannedOp {
                op: Op::$op $(($($arg),+))?,
                span: None,
            },
        )*]
    }
}

use rotth_analysis::ctir::ConcreteError;
use rotth_analysis::Error as TypecheckError;
use rotth_parser::ParserError;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error<'i> {
    #[error("IO error {0}")]
    IO(#[from] std::io::Error),
    #[error("Lexer error")]
    Lexer,
    #[error("Parser error {0:?}")]
    Parser(ParserError<'i>),
    #[error("Typecheck error {0:?}")]
    Typecheck(TypecheckError),
    #[error("Concretisation error {0:?}")]
    Concrete(ConcreteError),
}

impl<'i> From<ParserError<'i>> for Error<'i> {
    fn from(e: ParserError<'i>) -> Self {
        Self::Parser(e)
    }
}
impl<'i> From<TypecheckError> for Error<'i> {
    fn from(e: TypecheckError) -> Self {
        Self::Typecheck(e)
    }
}
