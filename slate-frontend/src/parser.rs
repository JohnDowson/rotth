use chumsky::{
    input::{Input, Stream},
    Parser,
};
use internment::Intern;
use itempath::ItemPathBuf;
use spanner::{Span, Spanned};

use crate::{
    lexer::{self, Token},
    ParserError,
};

use self::ast::{Module, ModuleDef, ResolvedModule, Word};

pub mod ast;
pub mod flat_ast;
mod flat_parsers;
mod parsers;
mod types;

#[tracing::instrument]
fn parse_file(tokens: Vec<(Token, Span)>, eoi: Span) -> Result<Module, ParserError<'static>> {
    parsers::file()
        .parse(Stream::from_iter(tokens).spanned(eoi))
        .into_result()
        .map_err(|e| e.into())
}

#[tracing::instrument]
pub fn parse(
    tokens: Vec<(Token, Span)>,
    eoi: Span,
) -> Result<ResolvedModule, ParserError<'static>> {
    let root = parse_file(tokens, eoi)?;
    let path = eoi.file.file_stem().unwrap().to_string_lossy();
    let path = Intern::<String>::from_ref(&*path);
    resolve_modules(root, ItemPathBuf::from(vec![path]))
}

#[tracing::instrument]
pub fn resolve_modules(
    Module {
        funcs,
        consts,
        statics,
        structs,
        inherent_impls,
        traits,
        trait_impls,
        modules,
        uses,
    }: Module,
    path: ItemPathBuf,
) -> Result<ResolvedModule, ParserError<'static>> {
    let mut errors = ParserError(Vec::new());
    let mut resolved_module = ResolvedModule {
        funcs,
        consts,
        statics,
        structs,
        inherent_impls,
        traits,
        trait_impls,
        modules: Vec::new(),
        uses,
    };
    for Spanned {
        span,
        inner:
            ModuleDef {
                module: _,
                name:
                    Spanned {
                        span: _,
                        inner: Word(name),
                    },
            },
    } in modules
    {
        let mut file = (*span.file).clone();
        file.set_extension("");
        file.push(&**name);
        file.set_extension("sl");

        let Ok(src) = std::fs::read_to_string(&file) else {
            errors.0.push(crate::Error::UnresolvedModule(
                span,
                Intern::from_ref(&*file),
            ));
            continue;
        };
        let (tokens, eoi) = lexer::lex(&src, Intern::from_ref(&file));
        let mut path = path.clone();
        path.push(name);
        let module = parse_file(tokens, eoi)?;
        match resolve_modules(module, path) {
            Ok(module) => resolved_module.modules.push(span.spanned(module)),
            Err(es) => {
                errors.0.extend(es.0);
                continue;
            }
        };
    }

    if !errors.0.is_empty() {
        Err(errors)
    } else {
        Ok(resolved_module)
    }
}
