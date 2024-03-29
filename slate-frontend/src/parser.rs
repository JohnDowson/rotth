use chumsky::{
    input::{Input, Stream},
    Parser,
};
use spanner::Span;

use crate::{lexer::Token, ParserError};

pub use self::ast::Module;

mod ast;
mod parsers;
mod types;

pub fn parse(tokens: Vec<(Token, Span)>) -> Result<Module, ParserError<'static>> {
    let len = tokens.len();
    let path = tokens.first().unwrap().1.file;
    parsers::file()
        .parse(Stream::from_iter(tokens).spanned(Span::point(path, len)))
        .into_result()
        .map_err(|e| e.into())
}
