#![feature(assert_matches)]
#![feature(vec_into_raw_parts)]
#![feature(iter_intersperse)]

pub mod emit;
pub mod eval;
pub mod hir;
pub mod lexer;
pub mod lir;
pub mod span;
pub mod typecheck;
