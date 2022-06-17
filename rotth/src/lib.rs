#![feature(assert_matches)]
#![feature(iter_intersperse)]
#![feature(box_syntax, box_patterns)]
#![feature(string_remove_matches)]
#![feature(type_alias_impl_trait)]
#![feature(array_windows)]

pub mod emit;
pub mod eval;
pub mod lir;

use rotth_analysis::ctir::ConcreteError;
use rotth_analysis::Error as TypecheckError;
use rotth_parser::ParserError;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("IO error {0}")]
    IO(#[from] std::io::Error),
    #[error("Lexer error")]
    Lexer,
    #[error("Parser error {0:?}")]
    Parser(ParserError),
    #[error("Typecheck error {0:?}")]
    Typecheck(TypecheckError),
    #[error("Concretisation error {0:?}")]
    Concrete(ConcreteError),
}

impl From<ParserError> for Error {
    fn from(e: ParserError) -> Self {
        Self::Parser(e)
    }
}
impl From<TypecheckError> for Error {
    fn from(e: TypecheckError) -> Self {
        Self::Typecheck(e)
    }
}
