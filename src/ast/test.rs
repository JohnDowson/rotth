use super::*;
use crate::lexer::lex_string;
use chumsky::{Parser, Stream};
use std::assert_matches::assert_matches;

#[test]
fn test_body() {
    let tokens = lex_string(
        indoc::indoc! {r#"
            0 while dup 15 <= do
                bind i: u64 do
                    i 3 mod
                    0 = dup if "Foo" puts end
                    i 5 mod
                    0 = dup if "Bar" puts end
                    or not if i putu end
                    "\n" puts
                    i 1 +
                end
            end drop
            0
        "#}
        .into(),
        "./".try_into().unwrap(),
    )
    .unwrap();
    let ast = body().then_ignore(end()).parse(Stream::from_iter(
        tokens.last().unwrap().1.clone(),
        tokens.into_iter(),
    ));
    assert_matches!(
        ast,
        Ok(AstNode {
            span: _,
            ast: AstKind::Body(_)
        })
    )
}
#[test]
fn test_const() {
    let tokens = lex_string(
        indoc::indoc! {r#"
            const FOO: u64 do 1 end
        "#}
        .into(),
        "./".try_into().unwrap(),
    )
    .unwrap();
    let ast = const_().then_ignore(end()).parse(Stream::from_iter(
        tokens.last().unwrap().1.clone(),
        tokens.into_iter(),
    ));
    assert_matches!(
        ast,
        Ok(TopLevel::Const(Const {
            const_: _,
            name: _,
            signature: _,
            do_: _,
            body: _,
            end: _,
        }))
    )
}
#[test]
fn test_mem() {
    let tokens = lex_string(
        indoc::indoc! {r#"
            mem FOO do 1 end
        "#}
        .into(),
        "./".try_into().unwrap(),
    )
    .unwrap();
    let ast = mem().then_ignore(end()).parse(Stream::from_iter(
        tokens.last().unwrap().1.clone(),
        tokens.into_iter(),
    ));
    assert_matches!(
        ast,
        Ok(TopLevel::Mem(Mem {
            mem: _,
            name: _,
            do_: _,
            body: _,
            end: _,
        }))
    )
}
#[test]
fn test_include() {
    let tokens = lex_string(
        indoc::indoc! {r#"
            include "./foo.rh"
        "#}
        .into(),
        "./".try_into().unwrap(),
    )
    .unwrap();
    let ast = include().then_ignore(end()).parse(Stream::from_iter(
        tokens.last().unwrap().1.clone(),
        tokens.into_iter(),
    ));
    assert_matches!(
        ast,
        Ok(TopLevel::Include(Include {
            include: _,
            path: _
        }))
    )
}
#[test]
fn test_proc() {
    let tokens = lex_string(
        indoc::indoc! {r#"
            proc foo u64 : u64 do
                1 +
            end
        "#}
        .into(),
        "./".try_into().unwrap(),
    )
    .unwrap();
    let ast = proc().then_ignore(end()).parse(Stream::from_iter(
        tokens.last().unwrap().1.clone(),
        tokens.into_iter(),
    ));
    assert_matches!(
        ast,
        Ok(TopLevel::Proc(Proc {
            proc: _,
            name: _,
            signature: _,
            do_: _,
            body: _,
            end: _
        }))
    )
}
