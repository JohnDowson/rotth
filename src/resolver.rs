use crate::{
    ast::{parse, TopLevel},
    lexer::lex,
    Result,
};
use somok::Somok;
use std::path::PathBuf;

pub fn resolve_include(path: PathBuf, existing: &mut Vec<TopLevel>) -> Result<()> {
    let source = path.canonicalize()?;
    std::env::set_current_dir(&source.parent().unwrap())?;

    let tokens = lex(source)?;

    let ast = parse(tokens)?;

    existing.extend(ast.into_iter().map(|(_, i)| i));
    ().okay()
}
