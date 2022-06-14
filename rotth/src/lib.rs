#![feature(assert_matches)]
#![feature(iter_intersperse)]
#![feature(box_syntax, box_patterns)]
#![feature(string_remove_matches)]
#![feature(type_alias_impl_trait)]
#![feature(array_windows)]

pub mod inference;
// pub mod lir;
pub mod ctir;
pub mod tir;
pub mod typecheck;

use rotth_parser::ParserError;
use thiserror::Error;
use typecheck::TypecheckError;

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
}

impl From<ParserError> for Error {
    fn from(e: ParserError) -> Self {
        Self::Parser(e)
    }
}
