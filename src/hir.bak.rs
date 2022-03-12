use crate::{
    iconst::IConst,
    lexer::{KeyWord, Token},
    resolver::resolve_include,
    span::Span,
    types::Type,
    Error, RedefinitionError,
};
use chumsky::{prelude::*, Stream};
use somok::Somok;
use std::{
    collections::{hash_map::Entry, HashMap},
    path::PathBuf,
};

#[derive(Debug, Clone)]
pub struct Signature {
    pub ins: Vec<Type>,
    pub outs: Vec<Type>,
}

#[derive(Debug, Clone)]
pub struct Proc {
    pub signature: Signature,
    pub body: Vec<AstNode>,
}
fn ty() -> impl Parser<Token, Type, Error = Simple<Token, Span>> {
    let value_type = filter_map(|s, t| match &t {
        Token::Word(ty) => match &**ty {
            "u64" => Type::U64.okay(),
            "u32" => Type::U32.okay(),
            "u16" => Type::U16.okay(),
            "u8" => Type::U8.okay(),

            "i64" => Type::I64.okay(),
            "i32" => Type::I32.okay(),
            "i16" => Type::I16.okay(),
            "i8" => Type::I8.okay(),

            "bool" => Type::BOOL.okay(),
            "char" => Type::CHAR.okay(),
            _ => Simple::expected_input_found(
                s,
                vec![Some(Token::Word("type".to_string()))],
                Some(t),
            )
            .error(),
        },
        _ => Simple::expected_input_found(
            s,
            vec![Some(Token::Word("some-type".to_string()))],
            Some(t),
        )
        .error(),
    });
    let any = just(Token::Word("()".to_string())).to(Type::ANY);
    let ptr_type = recursive(|p_ty| {
        just(Token::Ptr)
            .ignore_then(choice((p_ty, choice((value_type, any)))))
            .map(Type::ptr_to)
    });
    choice((value_type, ptr_type))
}

fn proc() -> impl Parser<Token, (String, (TopLevel, Span)), Error = Simple<Token, Span>> {
    let sig_sep = just(Token::SigSep);
    let signature = ty()
        .repeated()
        .then(sig_sep.then(ty().repeated().at_least(1)).or_not())
        .map(|(ins, outs)| {
            let outs = if let Some((_, types)) = outs {
                types
            } else {
                vec![]
            };
            Signature { ins, outs }
        });

    just(Token::KeyWord(KeyWord::Proc))
        .ignored()
        .then(identifier())
        .then(signature)
        .then_ignore(just(Token::KeyWord(KeyWord::Do)))
        .then(body())
        .then_ignore(just(Token::KeyWord(KeyWord::End)))
        .map_with_span(|((((), name), signature), body), s| {
            (name, (TopLevel::Proc(Proc { signature, body }), s))
        })
}

#[derive(Debug, Clone)]
pub struct AstNode {
    pub span: Span,
    pub ast: AstKind,
}

#[derive(Debug, Clone)]
pub enum AstKind {
    Literal(IConst),
    Word(String),
    Intrinsic(Intrinsic),
    If(If),
    Cond(Cond),
    Return,
    While(While),
    Bind(Bind),
}

#[derive(Debug, Clone)]
pub struct Cond {
    pub branches: Vec<CondBranch>,
    pub other: Vec<AstNode>,
}

#[derive(Debug, Clone)]
pub struct CondBranch {
    pub pattern: AstNode,
    pub body: Vec<AstNode>,
}

#[derive(Debug, Clone)]
pub enum Intrinsic {
    Drop,
    Dup,
    Swap,
    Over,

    Cast(Type),

    ReadU64,
    ReadU8,
    WriteU64,
    WriteU8,

    CompStop,
    Dump,
    Print,

    Syscall0,
    Syscall1,
    Syscall2,
    Syscall3,
    Syscall4,
    Syscall5,
    Syscall6,

    Argc,
    Argv,

    Add,
    Sub,
    Divmod,
    Mul,

    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug, Clone)]
pub struct Const {
    pub body: Vec<AstNode>,
    pub types: Vec<Type>,
}

#[derive(Debug, Clone)]
pub struct Bind {
    pub bindings: Vec<Binding>,
    pub body: Vec<AstNode>,
}

#[derive(Debug, Clone)]
pub enum Binding {
    Ignore,
    Bind { name: String, ty: Type },
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

fn word() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    filter_map(|span, token| {
        match token {
            Token::Word(ref w)
                if matches!(
                    w.as_str(),
                    "drop"
                        | "dup"
                        | "swap"
                        | "over"
                        | "&?&"
                        | "&?"
                        | "print"
                        | "="
                        | "!="
                        | "<"
                        | "<="
                        | ">"
                        | ">="
                ) =>
            {
                return Simple::expected_input_found(
                    span,
                    vec![Some(Token::Word("not-intrinsic".to_string()))],
                    Some(token),
                )
                .error();
            }
            Token::Word(w) => AstNode {
                ast: AstKind::Word(w),
                span,
            },
            _ => {
                return Simple::expected_input_found(
                    span,
                    vec![Some(Token::Word("not-intrinsic".to_string()))],
                    Some(token),
                )
                .error();
            }
        }
        .okay()
    })
}

fn intrinsic() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    filter_map(|span, token| match &token {
        Token::Word(w) => AstNode {
            ast: match w.as_str() {
                "drop" => AstKind::Intrinsic(Intrinsic::Drop),
                "dup" => AstKind::Intrinsic(Intrinsic::Dup),
                "swap" => AstKind::Intrinsic(Intrinsic::Swap),
                "over" => AstKind::Intrinsic(Intrinsic::Over),

                "@u64" => AstKind::Intrinsic(Intrinsic::ReadU64),
                "@u8" => AstKind::Intrinsic(Intrinsic::ReadU8),
                "!u64" => AstKind::Intrinsic(Intrinsic::WriteU64),
                "!u8" => AstKind::Intrinsic(Intrinsic::WriteU8),

                "&?&" => AstKind::Intrinsic(Intrinsic::CompStop),
                "&?" => AstKind::Intrinsic(Intrinsic::Dump),
                "print" => AstKind::Intrinsic(Intrinsic::Print),

                "syscall0" => AstKind::Intrinsic(Intrinsic::Syscall0),
                "syscall1" => AstKind::Intrinsic(Intrinsic::Syscall1),
                "syscall2" => AstKind::Intrinsic(Intrinsic::Syscall2),
                "syscall3" => AstKind::Intrinsic(Intrinsic::Syscall3),
                "syscall4" => AstKind::Intrinsic(Intrinsic::Syscall4),
                "syscall5" => AstKind::Intrinsic(Intrinsic::Syscall5),
                "syscall6" => AstKind::Intrinsic(Intrinsic::Syscall6),

                "argc" => AstKind::Intrinsic(Intrinsic::Argc),
                "argv" => AstKind::Intrinsic(Intrinsic::Argv),

                "+" => AstKind::Intrinsic(Intrinsic::Add),
                "-" => AstKind::Intrinsic(Intrinsic::Sub),
                "*" => AstKind::Intrinsic(Intrinsic::Mul),
                "divmod" => AstKind::Intrinsic(Intrinsic::Divmod),

                "=" => AstKind::Intrinsic(Intrinsic::Eq),
                "!=" => AstKind::Intrinsic(Intrinsic::Ne),
                "<" => AstKind::Intrinsic(Intrinsic::Lt),
                "<=" => AstKind::Intrinsic(Intrinsic::Le),
                ">" => AstKind::Intrinsic(Intrinsic::Gt),
                ">=" => AstKind::Intrinsic(Intrinsic::Ge),
                _ => {
                    return Simple::expected_input_found(
                        span,
                        vec![Some(Token::Word("some-intrinsic".to_string()))],
                        Some(token),
                    )
                    .error()
                }
            },
            span,
        }
        .okay(),
        _ => Simple::expected_input_found(
            span,
            vec![Some(Token::Word("some-intrinsic".to_string()))],
            Some(token),
        )
        .error(),
    })
}

fn identifier() -> impl Parser<Token, String, Error = Simple<Token, Span>> {
    filter(|t| matches!(t, Token::Word(_))).map(|token| match token {
        Token::Word(w) => w,
        _ => unreachable!(),
    })
}

fn body() -> impl Parser<Token, Vec<AstNode>, Error = Simple<Token, Span>> + Clone {
    recursive(|body| {
        let name_type = identifier()
            .then_ignore(just(Token::SigSep))
            .then(ty())
            .map(|(name, ty)| Binding::Bind { name, ty });

        let ignore = just(Token::Ignore).to(Binding::Ignore);

        let bind = just(Token::KeyWord(KeyWord::Bind))
            .ignore_then(choice((ignore, name_type)).repeated().at_least(1))
            .then_ignore(just(Token::KeyWord(KeyWord::Do)))
            .then(body.clone())
            .then_ignore(just(Token::KeyWord(KeyWord::End)))
            .map_with_span(|(bindings, body), span| AstNode {
                ast: AstKind::Bind(Bind { bindings, body }),
                span,
            });

        let num = filter(|t| matches!(t, Token::Num(_))).map_with_span(|token, span| AstNode {
            ast: AstKind::Literal(if let Token::Num(n) = token {
                IConst::U64(n.parse().unwrap())
            } else {
                unreachable!()
            }),
            span,
        });

        let char =
            filter(|token| matches!(token, Token::Char(_))).map_with_span(
                |token, span| match token {
                    Token::Char(c) => AstNode {
                        span,
                        ast: AstKind::Literal(IConst::Char(c)),
                    },
                    _ => unreachable!(),
                },
            );

        let string =
            filter(|token| matches!(token, Token::Str(_))).map_with_span(
                |token, span| match token {
                    Token::Str(s) => AstNode {
                        span,
                        ast: AstKind::Literal(IConst::Str(s)),
                    },
                    _ => unreachable!(),
                },
            );

        let bool = filter(|t| matches!(t, Token::Word(_))).try_map(|token, span| match &token {
            Token::Word(w) => match w.as_str() {
                "true" => AstNode {
                    ast: AstKind::Literal(IConst::Bool(true)),
                    span,
                }
                .okay(),
                "false" => AstNode {
                    ast: AstKind::Literal(IConst::Bool(false)),
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

        let if_ = {
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

        let ret = just(Token::KeyWord(KeyWord::Return)).map_with_span(|_, span| AstNode {
            span,
            ast: AstKind::Return,
        });

        let cast = just(Token::KeyWord(KeyWord::Cast))
            .ignore_then(ty())
            .map_with_span(|ty, span| AstNode {
                span,
                ast: AstKind::Intrinsic(Intrinsic::Cast(ty)),
            });

        let pattern = choice((num, char, bool, word()));
        let cond_branch = pattern
            .then_ignore(just(Token::KeyWord(KeyWord::Do)))
            .then(body.clone())
            .map(|(pattern, body)| CondBranch { pattern, body });
        let cond = just(Token::KeyWord(KeyWord::Cond))
            .ignore_then(
                cond_branch
                    .separated_by(just(Token::KeyWord(KeyWord::Else)))
                    .at_least(1),
            )
            .then(
                just(Token::KeyWord(KeyWord::Otherwise))
                    .ignore_then(body)
                    .then_ignore(just(Token::KeyWord(KeyWord::End))),
            )
            .map_with_span(|(branches, other), span| AstNode {
                span,
                ast: AstKind::Cond(Cond { branches, other }),
            });

        choice((
            bool,
            intrinsic(),
            word(),
            char,
            string,
            num,
            ret,
            if_,
            cond,
            while_,
            bind,
            cast,
        ))
        .repeated()
    })
}

fn constant() -> impl Parser<Token, (String, (TopLevel, Span)), Error = Simple<Token, Span>> {
    just(Token::KeyWord(KeyWord::Const))
        .ignore_then(identifier())
        .then_ignore(just(Token::SigSep))
        .then(ty().repeated().at_least(1))
        .then_ignore(just(Token::KeyWord(KeyWord::Do)))
        .then(body())
        .then_ignore(just(Token::KeyWord(KeyWord::End)))
        .map_with_span(|((name, types), body), span| {
            (name, (TopLevel::Const(Const { body, types }), span))
        })
}

fn include() -> impl Parser<Token, (String, (TopLevel, Span)), Error = Simple<Token, Span>> {
    just(Token::KeyWord(KeyWord::Include))
        .ignore_then(
            filter(|token| matches!(token, Token::Str(_))).map(|token| match token {
                Token::Str(s) => s.into(),
                _ => unreachable!(),
            }),
        )
        .map_with_span(|path, span| (String::new(), (TopLevel::Include(path), span)))
}

#[derive(Debug, Clone)]
pub struct Mem {
    pub body: Vec<AstNode>,
}

#[derive(Debug, Clone)]
pub enum TopLevel {
    Proc(Proc),
    Const(Const),
    Include(PathBuf),
    Mem(Mem),
}
impl TopLevel {
    pub fn as_proc(&self) -> Option<&Proc> {
        if let Self::Proc(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_const(&self) -> Option<&Const> {
        if let Self::Const(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

fn mem() -> impl Parser<Token, (String, (TopLevel, Span)), Error = Simple<Token, Span>> {
    just(Token::KeyWord(KeyWord::Mem))
        .ignore_then(identifier())
        .then_ignore(just(Token::KeyWord(KeyWord::Do)))
        .then(body())
        .then_ignore(just(Token::KeyWord(KeyWord::End)))
        .map_with_span(|(name, body), span| (name, (TopLevel::Mem(Mem { body }), span)))
}

fn toplevel_items(
) -> impl Parser<Token, Vec<(String, (TopLevel, Span))>, Error = Simple<Token, Span>> {
    choice((proc(), constant(), include(), mem()))
        .repeated()
        .then_ignore(end())
        .collect()
}

pub fn parse(tokens: Vec<(Token, Span)>) -> Result<HashMap<String, (TopLevel, Span)>, Error> {
    let items = match toplevel_items().parse(Stream::from_iter(
        tokens.last().unwrap().1.clone(),
        tokens.into_iter(),
    )) {
        Ok(items) => items,
        Err(es) => return Error::Parser(es).error(),
    };

    let (includes, mut items) = items
        .into_iter()
        .partition::<Vec<_>, _>(|(_, (item, _))| matches!(item, TopLevel::Include(_)));

    for (_, (include, _)) in includes {
        if let TopLevel::Include(path) = include {
            resolve_include(path, &mut items)?;
        } else {
            unreachable!();
        }
    }

    let mut res = HashMap::new();
    let mut errors = Vec::new();

    for (name, (item, span)) in items {
        match res.entry(name) {
            Entry::Occupied(it) => {
                let redefined: &(TopLevel, Span) = it.get();
                errors.push(RedefinitionError {
                    redefining_item: span,
                    redefined_item: redefined.1.clone(),
                });
            }
            Entry::Vacant(v) => {
                v.insert((item, span));
            }
        }
    }

    if errors.is_empty() {
        res.okay()
    } else {
        Error::Redefinition(errors).error()
    }
}
