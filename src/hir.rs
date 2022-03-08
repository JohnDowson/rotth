use std::collections::HashMap;

use chumsky::prelude::*;
use somok::Somok;

use crate::{
    lexer::{KeyWord, Token},
    span::Span,
};

#[derive(Clone)]
pub enum Const {
    Bool(u64),
    U64(u64),
    I64(u64),
}

impl std::fmt::Debug for Const {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(arg0) => f.debug_tuple("Bool").field(&(*arg0 != 0)).finish(),
            Self::U64(arg0) => f.debug_tuple("U64").field(arg0).finish(),
            Self::I64(arg0) => f.debug_tuple("I64").field(&(*arg0 as i64)).finish(),
        }
    }
}

impl Const {
    pub fn new<T: ToConst>(v: T) -> Self {
        v.to_const()
    }
    pub fn bytes(&self) -> u64 {
        match self {
            Const::Bool(c) => *c,
            Const::U64(c) => *c,
            Const::I64(c) => *c,
        }
    }
}

pub trait ToConst {
    fn to_const(self) -> Const;
}

impl ToConst for bool {
    fn to_const(self) -> Const {
        Const::Bool(self as u64)
    }
}
impl ToConst for u64 {
    fn to_const(self) -> Const {
        Const::U64(self)
    }
}
impl ToConst for i64 {
    fn to_const(self) -> Const {
        Const::I64(self as u64)
    }
}

#[derive(Debug, Clone)]
pub struct Signature {
    pub ins: Vec<Type>,
    pub outs: Vec<Type>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Type {
    Bool,
    U64,
    I64,
}

#[derive(Debug, Clone)]
pub struct Proc {
    pub signature: Signature,
    pub body: Vec<AstNode>,
}

fn proc() -> impl Parser<Token, (String, (Proc, Span)), Error = Simple<Token, Span>> {
    let ty = filter_map(|s, t| match &t {
        Token::Word(ty) => match &**ty {
            "int" => Type::I64.okay(),
            "uint" => Type::U64.okay(),
            "bool" => Type::Bool.okay(),
            _ => Simple::expected_input_found(
                s,
                vec![
                    Some(Token::Word("int".to_string())),
                    Some(Token::Word("uint".to_string())),
                    Some(Token::Word("bool".to_string())),
                ],
                Some(t),
            )
            .error(),
        },
        _ => Simple::expected_input_found(
            s,
            vec![
                Some(Token::Word("int".to_string())),
                Some(Token::Word("uint".to_string())),
                Some(Token::Word("bool".to_string())),
            ],
            Some(t),
        )
        .error(),
    });
    let sig_sep = just(Token::SigSep);
    let signature = ty
        .repeated()
        .then(sig_sep.then(ty.repeated().at_least(1)).or_not())
        .map(|(ins, outs)| {
            let outs = if let Some((_, types)) = outs {
                types
            } else {
                vec![]
            };
            Signature { ins, outs }
        });

    let identifier = filter_map(|s, t| {
        if let Token::Word(n) = t {
            n.okay()
        } else {
            Simple::expected_input_found(s, vec![Some(Token::Word("word".to_string()))], Some(t))
                .error()
        }
    });

    just(Token::KeyWord(KeyWord::Proc))
        .ignored()
        .then(identifier)
        .then(signature)
        .then_ignore(just(Token::KeyWord(KeyWord::Do)))
        .then(body())
        .then_ignore(just(Token::KeyWord(KeyWord::End)))
        .map_with_span(|((((), name), signature), body), s| (name, (Proc { signature, body }, s)))
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

#[derive(Debug, Clone)]
pub struct AstNode {
    pub span: Span,
    pub ast: AstKind,
}

#[derive(Debug, Clone)]
pub enum AstKind {
    Literal(Const),
    Word(String),
    If(If),
    While(While),
}

#[derive(Debug, Clone)]
pub struct If {
    pub truth: Vec<AstNode>,
    pub lie: Option<Vec<AstNode>>,
}

#[derive(Debug, Clone)]
pub struct While {
    pub cond: Vec<AstNode>,
    pub body: Vec<AstNode>,
}

fn body() -> impl Parser<Token, Vec<AstNode>, Error = Simple<Token, Span>> + Clone {
    recursive(|body| {
        let word =
            filter(|t| matches!(t, Token::Word(_))).map_with_span(|token, span| match token {
                Token::Word(w) => AstNode {
                    ast: AstKind::Word(w),
                    span,
                },
                _ => unreachable!(),
            });

        let num = filter(|t| matches!(t, Token::Num(_))).map_with_span(|token, span| AstNode {
            ast: AstKind::Literal(if let Token::Num(n) = token {
                n.parse::<u64>().unwrap().to_const()
            } else {
                unreachable!()
            }),
            span,
        });

        let bool = filter(|t| matches!(t, Token::Word(_))).try_map(|token, span| match &token {
            Token::Word(w) => match w.as_str() {
                "true" => AstNode {
                    ast: AstKind::Literal(true.to_const()),
                    span,
                }
                .okay(),
                "false" => AstNode {
                    ast: AstKind::Literal(false.to_const()),
                    span,
                }
                .okay(),
                _ => Simple::expected_input_found(
                    span,
                    vec![
                        Some(Token::Word("true".to_string())),
                        Some(Token::Word("false".to_string())),
                    ],
                    Some(token),
                )
                .error(),
            },
            _ => unreachable!(),
        });

        let while_ = {
            just(Token::KeyWord(KeyWord::While))
                .ignore_then(body.clone())
                .then_ignore(just(Token::KeyWord(KeyWord::Do)))
                .then(body.clone())
                .then_ignore(just(Token::KeyWord(KeyWord::End)))
                .map_with_span(|(cond, body), span| AstNode {
                    ast: AstKind::While(While { cond, body }),
                    span,
                })
        };

        let cond = {
            let truth = body
                .clone()
                .then(
                    just(Token::KeyWord(KeyWord::Else))
                        .ignore_then(body.clone())
                        .or_not(),
                )
                .then_ignore(just(Token::KeyWord(KeyWord::End)));

            just(Token::KeyWord(KeyWord::If))
                .ignored()
                .then(truth)
                .map_with_span(|((), (truth, lie)), span| AstNode {
                    ast: AstKind::If(If { truth, lie }),
                    span,
                })
        };
        choice((bool, word, num, cond, while_)).repeated()
    })
}

#[test]
fn test_body() {
    use chumsky::Stream;
    let src = "while true do end";
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

pub fn procs() -> impl Parser<Token, HashMap<String, (Proc, Span)>, Error = Simple<Token, Span>> {
    proc().repeated().at_least(1).then_ignore(end()).collect()
}

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
