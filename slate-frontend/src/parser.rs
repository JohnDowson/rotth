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

pub use self::ast::Module;
use self::ast::{ModuleDef, Word};

mod ast;
mod parsers;
mod types;

pub fn parse(tokens: Vec<(Token, Span)>, eoi: Span) -> Result<Module, ParserError<'static>> {
    parsers::file()
        .parse(Stream::from_iter(tokens).spanned(eoi))
        .into_result()
        .map_err(|e| e.into())
}

pub fn resolve_modules(root: Module, path: ItemPathBuf) -> Result<(), ParserError<'static>> {
    let mut errors = ParserError(Vec::new());
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
    } in root.modules
    {
        let mut file = (*span.file).clone();
        file.push(&**name);
        file.set_extension("sl");

        let Ok(src) = std::fs::read_to_string(&file) else {
            errors.0.push(crate::Error::UnresolvedInclude(span));
            continue;
        };
        let (tokens, eoi) = lexer::lex(&src, Intern::from_ref(&file));
        let mut path = path.clone();
        path.push(name);
        let module = parse(tokens, eoi)?;
        let module = match resolve_modules(module, path) {
            Ok(module) => module,
            Err(es) => errors.0.extend(es.0),
        };
    }
    Ok(())
}
