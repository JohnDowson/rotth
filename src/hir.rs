use crate::{
    lexer::{KeyWord, Token},
    span::Span,
};
use chumsky::prelude::*;
use somok::Somok;
use std::collections::HashMap;
#[cfg(test)]
mod test;

#[derive(Clone, Debug)]
pub enum IConst {
    Bool(bool),
    U64(u64),
    I64(i64),
    Str(String),
    Ptr(u64),
}

impl IConst {
    pub fn as_bool(&self) -> Option<&bool> {
        if let Self::Bool(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_u64(&self) -> Option<&u64> {
        if let Self::U64(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_i64(&self) -> Option<&i64> {
        if let Self::I64(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_str(&self) -> Option<&String> {
        if let Self::Str(v) = self {
            Some(v)
        } else {
            None
        }
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
    Ptr,
}

#[derive(Debug, Clone)]
pub struct Proc {
    pub signature: Signature,
    pub body: Vec<AstNode>,
}
fn ty() -> impl Parser<Token, Type, Error = Simple<Token, Span>> {
    filter_map(|s, t| match &t {
        Token::Word(ty) => match &**ty {
            "int" => Type::I64.okay(),
            "uint" => Type::U64.okay(),
            "bool" => Type::Bool.okay(),
            "ptr" => Type::Ptr.okay(),
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
    })
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
    While(While),
    Bind(Bind),
}

#[derive(Debug, Clone)]
pub enum Intrinsic {
    Drop,
    Dup,
    Swap,
    Over,

    PtrAdd,
    PtrSub,
    ReadU8,
    WriteU8,

    CompStop,
    Dump,
    Print,

    Syscall3,

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
    pub ty: Type,
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
        Token::Word(w) => match w.as_str() {
            "drop" => AstNode {
                ast: AstKind::Intrinsic(Intrinsic::Drop),
                span,
            }
            .okay(),
            "dup" => AstNode {
                ast: AstKind::Intrinsic(Intrinsic::Dup),
                span,
            }
            .okay(),
            "swap" => AstNode {
                ast: AstKind::Intrinsic(Intrinsic::Swap),
                span,
            }
            .okay(),
            "over" => AstNode {
                ast: AstKind::Intrinsic(Intrinsic::Over),
                span,
            }
            .okay(),

            "@u8" => AstNode {
                ast: AstKind::Intrinsic(Intrinsic::ReadU8),
                span,
            }
            .okay(),
            "!u8" => AstNode {
                ast: AstKind::Intrinsic(Intrinsic::WriteU8),
                span,
            }
            .okay(),
            "ptr+" => AstNode {
                ast: AstKind::Intrinsic(Intrinsic::PtrAdd),
                span,
            }
            .okay(),
            "ptr-" => AstNode {
                ast: AstKind::Intrinsic(Intrinsic::PtrSub),
                span,
            }
            .okay(),

            "&?&" => AstNode {
                ast: AstKind::Intrinsic(Intrinsic::CompStop),
                span,
            }
            .okay(),
            "&?" => AstNode {
                ast: AstKind::Intrinsic(Intrinsic::Dump),
                span,
            }
            .okay(),
            "print" => AstNode {
                ast: AstKind::Intrinsic(Intrinsic::Print),
                span,
            }
            .okay(),
            "syscall3" => AstNode {
                ast: AstKind::Intrinsic(Intrinsic::Syscall3),
                span,
            }
            .okay(),

            "+" => AstNode {
                ast: AstKind::Intrinsic(Intrinsic::Add),
                span,
            }
            .okay(),
            "-" => AstNode {
                ast: AstKind::Intrinsic(Intrinsic::Sub),
                span,
            }
            .okay(),
            "*" => AstNode {
                ast: AstKind::Intrinsic(Intrinsic::Mul),
                span,
            }
            .okay(),
            "divmod" => AstNode {
                ast: AstKind::Intrinsic(Intrinsic::Divmod),
                span,
            }
            .okay(),

            "=" => AstNode {
                ast: AstKind::Intrinsic(Intrinsic::Eq),
                span,
            }
            .okay(),
            "!=" => AstNode {
                ast: AstKind::Intrinsic(Intrinsic::Ne),
                span,
            }
            .okay(),
            "<" => AstNode {
                ast: AstKind::Intrinsic(Intrinsic::Lt),
                span,
            }
            .okay(),
            "<=" => AstNode {
                ast: AstKind::Intrinsic(Intrinsic::Le),
                span,
            }
            .okay(),
            ">" => AstNode {
                ast: AstKind::Intrinsic(Intrinsic::Gt),
                span,
            }
            .okay(),
            ">=" => AstNode {
                ast: AstKind::Intrinsic(Intrinsic::Ge),
                span,
            }
            .okay(),
            _ => Simple::expected_input_found(
                span,
                vec![
                    Some(Token::Word("drop".to_string())),
                    Some(Token::Word("dup".to_string())),
                    Some(Token::Word("swap".to_string())),
                    Some(Token::Word("over".to_string())),
                    Some(Token::Word("&?&".to_string())),
                    Some(Token::Word("&?".to_string())),
                    Some(Token::Word("print".to_string())),
                    Some(Token::Word("+".to_string())),
                    Some(Token::Word("-".to_string())),
                    Some(Token::Word("*".to_string())),
                    Some(Token::Word("divmod".to_string())),
                    Some(Token::Word("=".to_string())),
                    Some(Token::Word("!=".to_string())),
                    Some(Token::Word("<".to_string())),
                    Some(Token::Word("<=".to_string())),
                    Some(Token::Word(">".to_string())),
                    Some(Token::Word(">=".to_string())),
                ],
                Some(token),
            )
            .error(),
        },
        _ => Simple::expected_input_found(
            span,
            vec![
                Some(Token::Word("drop".to_string())),
                Some(Token::Word("dup".to_string())),
                Some(Token::Word("swap".to_string())),
                Some(Token::Word("over".to_string())),
                Some(Token::Word("&?&".to_string())),
                Some(Token::Word("&?".to_string())),
                Some(Token::Word("print".to_string())),
                Some(Token::Word("+".to_string())),
                Some(Token::Word("-".to_string())),
                Some(Token::Word("*".to_string())),
                Some(Token::Word("divmod".to_string())),
                Some(Token::Word("=".to_string())),
                Some(Token::Word("!=".to_string())),
                Some(Token::Word("<".to_string())),
                Some(Token::Word("<=".to_string())),
                Some(Token::Word(">".to_string())),
                Some(Token::Word(">=".to_string())),
            ],
            Some(token),
        )
        .error(),
    })
}

fn word_or_intrinsic() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    choice((intrinsic(), word()))
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
        let ignore = filter_map(|span, token| {
            if matches!(token, Token::Ignore) {
                Binding::Ignore.okay()
            } else {
                Simple::expected_input_found(
                    span,
                    vec![
                        Some(Token::Word("int".to_string())),
                        Some(Token::Word("uint".to_string())),
                        Some(Token::Word("bool".to_string())),
                    ],
                    Some(token),
                )
                .error()
            }
        });

        let bind = just(Token::KeyWord(KeyWord::Bind))
            .ignore_then(choice((name_type, ignore)).repeated().at_least(1))
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
        choice((bool, word_or_intrinsic(), string, num, cond, while_, bind)).repeated()
    })
}

fn constant() -> impl Parser<Token, (String, (TopLevel, Span)), Error = Simple<Token, Span>> {
    just(Token::KeyWord(KeyWord::Const))
        .ignore_then(identifier())
        .then_ignore(just(Token::SigSep))
        .then(ty())
        .then_ignore(just(Token::KeyWord(KeyWord::Do)))
        .then(body())
        .then_ignore(just(Token::KeyWord(KeyWord::End)))
        .map_with_span(|((name, ty), body), span| {
            (name, (TopLevel::Const(Const { body, ty }), span))
        })
}

fn include() -> impl Parser<Token, (String, (TopLevel, Span)), Error = Simple<Token, Span>> {
    just(Token::KeyWord(KeyWord::Include))
        .ignore_then(
            filter(|token| matches!(token, Token::Str(_))).map(|token| match token {
                Token::Str(s) => s,
                _ => unreachable!(),
            }),
        )
        .map_with_span(|path, span| (String::new(), (TopLevel::Include(path), span)))
}

#[derive(Debug, Clone)]
pub enum TopLevel {
    Proc(Proc),
    Const(Const),
    Include(String),
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

    pub fn as_include(&self) -> Option<&String> {
        if let Self::Include(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

pub fn procs() -> impl Parser<Token, HashMap<String, (TopLevel, Span)>, Error = Simple<Token, Span>>
{
    choice((proc(), constant(), include()))
        .repeated()
        .then_ignore(end())
        .collect()
}
