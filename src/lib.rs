#![feature(assert_matches)]
#![feature(iter_intersperse)]
#![feature(box_syntax)]
#![feature(box_patterns)]

#[macro_export]
macro_rules! coerce_ast {
    ($node:expr => $kind:tt || None) => {
        if let AstKind::$kind(ast) = $node.ast {
            Some(ast)
        } else {
            None
        }
    };
    ($node:expr => $kind:tt || $or:expr) => {
        if let AstKind::$kind(ast) = $node.ast {
            ast
        } else {
            $or
        }
    };
    ($node:expr => REF $kind:tt || None) => {
        if let AstKind::$kind(ast) = &$node.ast {
            Some(ast)
        } else {
            None
        }
    };
    ($node:expr => REF $kind:tt || $or:expr) => {
        if let AstKind::$kind(ast) = &$node.ast {
            ast
        } else {
            $or
        }
    };
}

pub mod ast;
pub mod emit;
pub mod eval;
pub mod hir;
pub mod iconst;
pub mod lexer;
pub mod lir;
pub mod resolver;
pub mod span;
pub mod typecheck;
pub mod types;

use chumsky::prelude::Simple;
use lexer::Token;
use span::Span;
use thiserror::Error;
use typecheck::TypecheckError;

#[derive(Debug, Error)]
pub enum Error {
    #[error("IO error {0}")]
    IO(#[from] std::io::Error),
    #[error("Lexer error {0:?}")]
    Lexer(Vec<Simple<char, Span>>),
    #[error("Parser error {0:?}")]
    Parser(Vec<Simple<Token, Span>>),
    #[error("Redefinition error {0:?}")]
    Redefinition(Vec<RedefinitionError>),
    #[error("Typecheck error {0:?}")]
    Typecheck(TypecheckError),
}

impl From<TypecheckError> for Error {
    fn from(e: TypecheckError) -> Self {
        Self::Typecheck(e)
    }
}

#[derive(Debug)]
pub struct RedefinitionError {
    pub redefining_item: Span,
    pub redefined_item: Span,
}

pub type Result<T> = std::result::Result<T, Error>;
