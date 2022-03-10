use crate::{
    hir::{parse, TopLevel},
    lexer::lex,
    span::Span,
    Result,
};
use somok::Somok;
use std::path::PathBuf;

pub fn resolve_include(
    path: PathBuf,
    existing: &mut Vec<(String, (TopLevel, Span))>,
) -> Result<()> {
    let tokens = lex(path)?;

    let ast = parse(tokens)?;

    existing.extend(ast);
    ().okay()
}
