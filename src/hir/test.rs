use super::*;
use crate::span::Span;
#[test]
fn test_procs() {
    use chumsky::Stream;
    let src = "proc foo : int do 1 end
    proc bar : int do 2 end";
    let tokens = crate::lexer::lexer()
        .parse(Stream::from_iter(
            Span::new(src.len(), src.len()),
            src.chars().enumerate().map(|(i, c)| (c, Span::point(i))),
        ))
        .unwrap();
    panic! {"{:?}", procs()
    .parse(Stream::from_iter(
        tokens.last().unwrap().1,
        tokens.into_iter(),
    ))
    .unwrap()};
}

#[test]
fn test_proc() {
    use chumsky::Stream;
    let src = "proc foo int : int do end";
    let tokens = crate::lexer::lexer()
        .parse(Stream::from_iter(
            Span::new(src.len(), src.len()),
            src.chars().enumerate().map(|(i, c)| (c, Span::point(i))),
        ))
        .unwrap();
    proc()
        .parse(Stream::from_iter(
            tokens.last().unwrap().1,
            tokens.into_iter(),
        ))
        .unwrap();
}
#[test]
fn test_cond() {
    use chumsky::Stream;
    let src = "if
            thing
        else
            other
        end";
    let tokens = crate::lexer::lexer()
        .parse(Stream::from_iter(
            Span::new(src.len(), src.len()),
            src.chars().enumerate().map(|(i, c)| (c, Span::point(i))),
        ))
        .unwrap();
    body()
        .parse(Stream::from_iter(
            tokens.last().unwrap().1,
            tokens.into_iter(),
        ))
        .unwrap();
}
#[test]
fn test_body() {
    use chumsky::Stream;
    let src = "bind _ do foo 1 + end";
    let tokens = crate::lexer::lexer()
        .parse(Stream::from_iter(
            Span::new(src.len(), src.len()),
            src.chars().enumerate().map(|(i, c)| (c, Span::point(i))),
        ))
        .unwrap();
    println! {"{:?}", &tokens};
    panic! {"{:?}",
    body()
    .parse(Stream::from_iter(
        tokens.last().unwrap().1,
        tokens.into_iter(),
    ))
    .unwrap()};
}

#[test]
fn test_const() {
    use chumsky::Stream;
    let src = "const foo : uint do 69 end";
    let tokens = crate::lexer::lexer()
        .parse(Stream::from_iter(
            Span::new(src.len(), src.len()),
            src.chars().enumerate().map(|(i, c)| (c, Span::point(i))),
        ))
        .unwrap();
    println! {"{:?}", &tokens};
    panic! {"{:?}",
    constant()
    .parse(Stream::from_iter(
        tokens.last().unwrap().1,
        tokens.into_iter(),
    ))
    .unwrap()};
}
