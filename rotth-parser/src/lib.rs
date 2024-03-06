#![feature(box_patterns)]
#![feature(iter_intersperse)]
#![feature(path_file_prefix)]

pub mod ast;
pub mod hir;
pub mod types;

use chumsky::error::Rich;
use rotth_lexer::Token;
use spanner::Span;
use thiserror::Error;

#[derive(Debug, Error)]
#[error("{0:?}")]
pub struct ParserError<'i>(pub Vec<Error<'i>>);

impl<'i> From<Vec<Rich<'i, Token, Span>>> for ParserError<'i> {
    fn from(es: Vec<Rich<'i, Token, Span>>) -> Self {
        Self(es.into_iter().map(Error::from).collect())
    }
}
#[derive(Debug, Error)]
pub enum Error<'i> {
    #[error("{0:?}")]
    Parser(Rich<'i, Token, Span>),
    #[error("{0:?}")]
    Redefinition(#[from] Redefinition),
    #[error("UnresolvedInclude {0:?}")]
    UnresolvedInclude(Span),
}

impl<'i> From<Rich<'i, Token, Span>> for Error<'i> {
    fn from(e: Rich<'i, Token, Span>) -> Self {
        Self::Parser(e)
    }
}

#[derive(Debug, Error)]
#[error("RedefinitionError")]
pub struct Redefinition {
    pub redefining_item: Span,
    pub redefined_item: Span,
}
