#![feature(assert_matches)]
#![feature(vec_into_raw_parts)]
#![feature(iter_intersperse)]
#![feature(entry_insert)]

pub mod emit;
pub mod eval;
pub mod hir;
pub mod lexer;
pub mod lir;
pub mod resolver;
pub mod span;
pub mod typecheck;

use chumsky::prelude::Simple;
use lexer::Token;
use span::Span;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("IO error {0}")]
    IO(#[from] std::io::Error),
    #[error("Lexer error")]
    Lexer(Vec<Simple<char, Span>>),
    #[error("Parser error")]
    Parser(Vec<Simple<Token, Span>>),
    #[error("Redefinition error")]
    Redefinition(Vec<RedefinitionError>),
}

#[derive(Debug)]
pub struct RedefinitionError {
    pub redefining_item: Span,
    pub redefined_item: Span,
}

pub type Result<T> = std::result::Result<T, Error>;
