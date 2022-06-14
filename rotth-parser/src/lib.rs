#![feature(box_patterns, box_syntax)]
#![feature(iter_intersperse)]
#![feature(generic_associated_types)]
#![feature(path_file_prefix)]

pub mod ast;
pub mod hir;
pub mod types;

use chumsky::prelude::Simple;
use rotth_lexer::Token;
use spanner::Span;
use thiserror::Error;

#[derive(Debug, Error)]
#[error("{0:?}")]
pub struct ParserError(pub Vec<Error>);

impl From<Vec<Simple<Token, Span>>> for ParserError {
    fn from(es: Vec<Simple<Token, Span>>) -> Self {
        Self(es.into_iter().map(Error::from).collect())
    }
}
#[derive(Debug, Error)]
pub enum Error {
    #[error("{0:?}")]
    Parser(Simple<Token, Span>),
    #[error("{0:?}")]
    Redefinition(#[from] Redefinition),
    #[error("UnresolvedInclude {0:?}")]
    UnresolvedInclude(Span),
}

impl From<Simple<Token, Span>> for Error {
    fn from(e: Simple<Token, Span>) -> Self {
        Self::Parser(e)
    }
}

#[derive(Debug, Error)]
#[error("RedefinitionError")]
pub struct Redefinition {
    pub redefining_item: Span,
    pub redefined_item: Span,
}
