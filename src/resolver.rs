use crate::{
    ast::{parse, TopLevel},
    lexer::lex,
    Result,
};
use somok::Somok;
use std::path::Path;

pub fn resolve_include(
    included_from: &Path,
    path: &Path,
    existing: &mut Vec<TopLevel>,
) -> Result<()> {
    let source = if path.is_relative() {
        included_from.parent().unwrap().join(path)
    } else {
        path.into()
    };
    let tokens = lex(source)?;

    let ast = parse(tokens)?;

    existing.extend(ast.into_iter().map(|(_, i)| i));
    ().okay()
}
