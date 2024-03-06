use chumsky::{input::ValueInput, prelude::*};
use logos::Logos;
use rotth_lexer::Token;
use smol_str::SmolStr;
use somok::Either;
use spanner::{Span, Spanned};
use std::path::{Path, PathBuf};

use crate::types::{Custom, Primitive, Type};

use super::{
    Bind, Cast, Cond, CondBranch, Const, ConstSignature, Else, Expr, FieldAccess, File,
    GenericParams, Generics, If, Include, ItemPathBuf, Keyword, Literal, Mem, NameTypePair, Proc,
    ProcSignature, Punctuation, Qualifiers, Read, Struct, TopLevel, Var, While, Word, Write,
};

trait RParser<'i, I, O>
where
    I: Input<'i, Token = Token, Span = Span>,
    Self: Parser<'i, I, Spanned<O>, extra::Err<Rich<'i, Token, Span>>> + Clone,
{
}

impl<'i, I, O, T> RParser<'i, I, O> for T
where
    T: Parser<'i, I, Spanned<O>, extra::Err<Rich<'i, Token, Span>>> + Clone,
    I: ValueInput<'i, Token = Token, Span = Span>,
{
}

pub(super) fn ty<'i, I>() -> impl RParser<'i, I, Type>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    recursive(|ty| {
        let generic_params = lbracket()
            .then(ty.repeated().at_least(1))
            .then(rbracket())
            .map_with_span(|((left_bracket, tys), right_bracket), span| Spanned {
                span,
                inner: GenericParams {
                    left_bracket,
                    tys,
                    right_bracket,
                },
            });
        just(Token::Ptr)
            .repeated()
            .then(path().then(generic_params.or_not()))
            .map_with_span(|(ptr, (ty, params)), span| {
                let mut ty = if let Some(type_name) = ty.only() {
                    match type_name {
                        "()" => Type::Primitive(Primitive::Void),
                        "bool" => Type::Primitive(Primitive::Bool),
                        "char" => Type::Primitive(Primitive::Char),

                        "u64" => Type::Primitive(Primitive::U64),
                        "u32" => Type::Primitive(Primitive::U32),
                        "u16" => Type::Primitive(Primitive::U16),
                        "u8" => Type::Primitive(Primitive::U8),

                        "i64" => Type::Primitive(Primitive::I64),
                        "i32" => Type::Primitive(Primitive::I32),
                        "i16" => Type::Primitive(Primitive::I16),
                        "i8" => Type::Primitive(Primitive::I8),
                        _ => Type::Custom(Custom {
                            name: ty.inner,
                            params,
                        }),
                    }
                } else {
                    Type::Custom(Custom {
                        name: ty.inner,
                        params,
                    })
                };
                for _ in ptr {
                    ty = Type::Ptr(Box::new(ty))
                }

                Spanned { span, inner: ty }
            })
    })
}

pub(super) fn parse_string<'i>(
    s: &'i str,
    path: &'static Path,
) -> Result<SmolStr, Rich<'i, Token, Span>> {
    #[derive(Logos)]
    pub enum StrToken {
        #[regex(r"\\.", |l| l.slice().chars().nth(1))]
        Escaped(char),
        #[regex(r#"[^\\"]"#, |l| l.slice().chars().next())]
        Char(char),
        #[token(r#"""#)]
        End,
    }

    StrToken::lexer(s)
        .spanned()
        .filter_map(|(t, s)| match t.ok()? {
            StrToken::Escaped(c) => match c {
                'n' => Some(Ok('\n')),
                't' => Some(Ok('\t')),
                '"' => Some(Ok('"')),
                _ => Some(Err(Rich::custom(
                    Span::new(path, s.start, s.end),
                    "Invalid character escape",
                ))),
            },
            StrToken::Char(c) => Some(Ok(c)),
            StrToken::End => None,
        })
        .collect()
}

pub(super) fn literal<'i, I>() -> impl RParser<'i, I, Literal>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::Bool(b) = span => Spanned { span: span.span(), inner: Literal::Bool(b)},
        Token::Num(n) = span => {
            if let Ok(i) = n.parse::<i64>(){
                Spanned{ span: span.span(),inner: Literal::Int(i) }
            }
            else {
                Spanned{ span: span.span(), inner: Literal::UInt(n.parse().unwrap()) }
            }
        },
        Token::String(s) = span => Spanned{ span: span.span(), inner: Literal::String(parse_string(&s, span.context())?) },
        Token::Char(c) = span => Spanned { span: span.span(), inner: Literal::Char(c) },
    }
}

pub(super) fn literal_expr<'i>(
) -> impl Parser<'i, Token, Spanned<Expr>, extra::Err<Rich<'i, Token, Span>>> + Clone {
    literal().map(|Spanned { span, inner }| Spanned {
        span,
        inner: Expr::Literal(inner),
    })
}

pub(super) fn include_path<'i>(
) -> impl Parser<'i, Token, Spanned<PathBuf>, extra::Err<Rich<'i, Token, Span>>> + Clone {
    select! {
        Token::String(s) = span => Spanned {
            span,
            inner: {
                let mut chars = (*s).chars();
                chars.next();
                chars.next_back();
                PathBuf::from(chars.as_str())
            }
        },
    }
}

pub(super) fn kw_include<'i>(
) -> impl Parser<'i, Token, Spanned<Keyword>, extra::Err<Rich<'i, Token, Span>>> + Clone {
    select! {
        Token::KwInclude = span => Spanned { span, inner: Keyword::Include },
    }
}

pub(super) fn kw_from<'i>(
) -> impl Parser<'i, Token, Spanned<Keyword>, extra::Err<Rich<'i, Token, Span>>> + Clone {
    select! {
        Token::KwFrom = span => Spanned { span, inner: Keyword::From },
    }
}

pub(super) fn kw_bind<'i>(
) -> impl Parser<'i, Token, Spanned<Keyword>, extra::Err<Rich<'i, Token, Span>>> + Clone {
    select! {
        Token::KwBind = span => Spanned { span, inner: Keyword::Bind },
    }
}
pub(super) fn kw_while<'i>(
) -> impl Parser<'i, Token, Spanned<Keyword>, extra::Err<Rich<'i, Token, Span>>> + Clone {
    select! {
        Token::KwWhile = span => Spanned { span, inner: Keyword::While },
    }
}
pub(super) fn kw_cond<'i>(
) -> impl Parser<'i, Token, Spanned<Keyword>, extra::Err<Rich<'i, Token, Span>>> + Clone {
    select! {
        Token::KwCond = span => Spanned { span, inner: Keyword::Cond },
    }
}
pub(super) fn kw_if<'i>(
) -> impl Parser<'i, Token, Spanned<Keyword>, extra::Err<Rich<'i, Token, Span>>> + Clone {
    select! {
        Token::KwIf = span => Spanned { span, inner: Keyword::If },
    }
}
pub(super) fn kw_else<'i>(
) -> impl Parser<'i, Token, Spanned<Keyword>, extra::Err<Rich<'i, Token, Span>>> + Clone {
    select! {
        Token::KwElse = span => Spanned { span, inner: Keyword::Else },
    }
}
pub(super) fn kw_do<'i>(
) -> impl Parser<'i, Token, Spanned<Keyword>, extra::Err<Rich<'i, Token, Span>>> + Clone {
    select! {
        Token::KwDo = span => Spanned { span, inner: Keyword::Do },
    }
}
pub(super) fn kw_end<'i>(
) -> impl Parser<'i, Token, Spanned<Keyword>, extra::Err<Rich<'i, Token, Span>>> + Clone {
    select! {
        Token::KwEnd = span => Spanned { span, inner: Keyword::End },
    }
}
pub(super) fn kw_ret<'i>(
) -> impl Parser<'i, Token, Spanned<Keyword>, extra::Err<Rich<'i, Token, Span>>> + Clone {
    select! {
        Token::KwReturn = span => Spanned { span, inner: Keyword::Return },
    }
}
pub(super) fn kw_cast<'i>(
) -> impl Parser<'i, Token, Spanned<Keyword>, extra::Err<Rich<'i, Token, Span>>> + Clone {
    select! {
        Token::KwCast = span => Spanned { span, inner: Keyword::Cast },
    }
}
pub(super) fn kw_proc<'i>(
) -> impl Parser<'i, Token, Spanned<Keyword>, extra::Err<Rich<'i, Token, Span>>> + Clone {
    select! {
        Token::KwProc = span => Spanned { span, inner: Keyword::Proc },
    }
}
pub(super) fn kw_const<'i>(
) -> impl Parser<'i, Token, Spanned<Keyword>, extra::Err<Rich<'i, Token, Span>>> + Clone {
    select! {
        Token::KwConst = span => Spanned { span, inner: Keyword::Const },
    }
}
pub(super) fn kw_mem<'i>(
) -> impl Parser<'i, Token, Spanned<Keyword>, extra::Err<Rich<'i, Token, Span>>> + Clone {
    select! {
        Token::KwMem = span => Spanned { span, inner: Keyword::Mem },
    }
}
pub(super) fn kw_var<'i>(
) -> impl Parser<'i, Token, Spanned<Keyword>, extra::Err<Rich<'i, Token, Span>>> + Clone {
    select! {
        Token::KwVar = span => Spanned { span, inner: Keyword::Var },
    }
}
pub(super) fn kw_struct<'i>(
) -> impl Parser<'i, Token, Spanned<Keyword>, extra::Err<Rich<'i, Token, Span>>> + Clone {
    select! {
        Token::KwStruct = span => Spanned { span, inner: Keyword::Struct },
    }
}

pub(super) fn word<'i>(
) -> impl Parser<'i, Token, Spanned<Word>, extra::Err<Rich<'i, Token, Span>>> + Clone {
    select! {
        Token::Word(w) = span => Spanned { span, inner: Word(w) },
        Token::Operator(w) = span => Spanned { span, inner: Word(w) },
    }
}

pub(super) fn word_expr<'i>(
) -> impl Parser<'i, Token, Spanned<Expr>, extra::Err<Rich<'i, Token, Span>>> + Clone {
    select! {
        Token::Word(w) = span => Spanned { span, inner: Expr::Word(Word(w)) },
        Token::Operator(w) = span => Spanned { span, inner: Expr::Word(Word(w)) },
    }
}

pub(super) fn path<'i>(
) -> impl Parser<'i, Token, Spanned<ItemPathBuf>, extra::Err<Rich<'i, Token, Span>>> + Clone {
    select! {
        Token::Word(w) =>  w,
        Token::Operator(w) =>  w,
    }
    .separated_by(just(Token::PathSep))
    .at_least(1)
    .map_with_span(|ws, span| Spanned {
        span,
        inner: ItemPathBuf::from(ws),
    })
}

pub(super) fn path_expr<'i>(
) -> impl Parser<'i, Token, Spanned<Expr>, extra::Err<Rich<'i, Token, Span>>> + Clone {
    select! {
        Token::Word(w) =>  w,
        Token::Operator(w) =>  w,
    }
    .separated_by(just(Token::PathSep))
    .at_least(1)
    .map_with_span(|ws, span| Spanned {
        span,
        inner: Expr::Path(ItemPathBuf::from(ws)),
    })
}

pub(super) fn separator<'i>(
) -> impl Parser<'i, Token, Spanned<Punctuation>, extra::Err<Rich<'i, Token, Span>>> + Clone {
    select! {
        Token::SigSep = span => Spanned { span, inner: Punctuation::Colon },
    }
}

pub(super) fn read_expr<'i>(
) -> impl Parser<'i, Token, Spanned<Expr>, extra::Err<Rich<'i, Token, Span>>> + Clone {
    select! {
        t @ Token::Read = span => Spanned { span, inner: t },
    }
    .then(ty())
    .map_with_span(|(read, ty), span| Spanned {
        span,
        inner: Expr::Read(Read { read, ty }),
    })
}

pub(super) fn write_expr<'i>(
) -> impl Parser<'i, Token, Spanned<Expr>, extra::Err<Rich<'i, Token, Span>>> + Clone {
    select! {
        t @ Token::Write = span => Spanned { span, inner: t },
    }
    .then(ty())
    .map_with_span(|(write, ty), span| Spanned {
        span,
        inner: Expr::Write(Write { write, ty }),
    })
}

pub(super) fn var<'i>(
) -> impl Parser<'i, Token, Spanned<Var>, extra::Err<Rich<'i, Token, Span>>> + Clone {
    kw_var()
        .then(word())
        .then(separator())
        .then(ty())
        .map_with_span(|(((var, name), sep), ty), span| Spanned {
            span,
            inner: Var { var, name, sep, ty },
        })
}

pub(super) fn var_toplevel<'i>(
) -> impl Parser<'i, Token, Spanned<TopLevel>, extra::Err<Rich<'i, Token, Span>>> + Clone {
    var().map(|Spanned { span, inner }| Spanned {
        span,
        inner: TopLevel::Var(inner),
    })
}

pub(super) fn var_expr<'i>(
) -> impl Parser<'i, Token, Spanned<Expr>, extra::Err<Rich<'i, Token, Span>>> + Clone {
    var().map(|Spanned { span, inner }| Spanned {
        span,
        inner: Expr::Var(inner),
    })
}

pub(super) fn accessor<'i>(
) -> impl Parser<'i, Token, Spanned<Punctuation>, extra::Err<Rich<'i, Token, Span>>> + Clone {
    just(Token::FieldAccess).map_with_span(|_, span| Spanned {
        span,
        inner: Punctuation::RArrow,
    })
}

pub(super) fn expr<'i, 'd>(
) -> impl Parser<'i, Token, Spanned<Expr>, extra::Err<Rich<'i, Token, Span>>> + Clone + 'd {
    recursive(|expr: Recursive<'_, Token, Spanned<Expr>, _>| {
        let bind = kw_bind()
            .then(
                name_type_pair()
                    .map(|b| b.map(Either::Right))
                    .or(word().map(|b| b.map(Either::Left)))
                    .repeated()
                    .at_least(1),
            )
            .then(kw_do())
            .then(expr.clone().repeated())
            .then(kw_end())
            .map_with_span(|((((bind, bindings), do_), body), end), span| Spanned {
                span,
                inner: Expr::Bind(Bind {
                    bind,
                    bindings,
                    do_,
                    body,
                    end,
                }),
            });

        let while_ = kw_while()
            .then(expr.clone().repeated())
            .then(kw_do())
            .then(expr.clone().repeated())
            .then(kw_end())
            .map_with_span(|((((while_, cond), do_), body), end), span| Spanned {
                span,
                inner: Expr::While(While {
                    while_,
                    cond,
                    do_,
                    body,
                    end,
                }),
            });

        let lie = kw_else()
            .then(expr.clone().repeated())
            .map(|(else_, body)| Else { else_, body });

        let if_ = kw_if()
            .then(expr.clone().repeated())
            .then(lie.or_not())
            .then(kw_end())
            .map_with_span(|(((if_, truth), lie), end), span| Spanned {
                span,
                inner: Expr::If(If {
                    if_,
                    truth,
                    lie,
                    end,
                }),
            });

        let cast = kw_cast()
            .then(ty())
            .map_with_span(|(cast, ty), span| Spanned {
                span,
                inner: Expr::Cast(Cast { cast, ty }),
            });

        let pat = choice((literal_expr(), word_expr(), path_expr()));
        let cond_branch = kw_else()
            .then(pat.clone())
            .then(kw_do())
            .then(expr.clone().repeated())
            .map_with_span(|(((else_, pat), do_), body), span| Spanned {
                span,
                inner: CondBranch {
                    else_,
                    pat,
                    do_,
                    body,
                },
            });

        let cond = kw_cond()
            .then(pat)
            .then(kw_do())
            .then(expr.clone().repeated())
            .then(cond_branch.repeated())
            .then(kw_end())
            .map_with_span(
                |(((((cond, pat), do_), body), branches), end), span| Spanned {
                    span,
                    inner: Expr::Cond(Box::new(Cond {
                        cond,
                        pat,
                        do_,
                        body,
                        branches,
                        end,
                    })),
                },
            );

        let field_access = accessor()
            .then(word())
            .map_with_span(|(access, field), span| Spanned {
                span,
                inner: Expr::FieldAccess(Box::new(FieldAccess { access, field })),
            });

        let ret_expr = kw_ret().map(|Spanned { span, inner }| Spanned {
            span,
            inner: Expr::Keyword(inner),
        });

        let comp_stop = just(Token::CompStop).map_with_span(|_, span| Spanned {
            span,
            inner: Expr::CompStop,
        });

        choice((
            comp_stop,
            literal_expr(),
            var_expr(),
            path_expr(),
            word_expr(),
            read_expr(),
            write_expr(),
            bind,
            while_,
            if_,
            cond,
            cast,
            ret_expr,
            field_access,
        ))
    })
}

pub(super) fn proc_signature<'i>(
) -> impl Parser<'i, Token, Spanned<ProcSignature>, extra::Err<Rich<'i, Token, Span>>> + Clone {
    ty().repeated()
        .then(separator().then(ty().repeated().at_least(1)).or_not())
        .map_with_span(|(ins, maybe_outs), span| {
            let (sep, outs) = if let Some((sep, outs)) = maybe_outs {
                (Some(sep), Some(outs))
            } else {
                (None, None)
            };
            Spanned {
                span,
                inner: ProcSignature { ins, sep, outs },
            }
        })
}

pub(super) fn lbracket<'i>(
) -> impl Parser<'i, Token, Spanned<Punctuation>, extra::Err<Rich<'i, Token, Span>>> + Clone {
    just(Token::LBracket).map_with_span(|_, span| Spanned {
        span,
        inner: Punctuation::LBracket,
    })
}
pub(super) fn rbracket<'i>(
) -> impl Parser<'i, Token, Spanned<Punctuation>, extra::Err<Rich<'i, Token, Span>>> + Clone {
    just(Token::RBracket).map_with_span(|_, span| Spanned {
        span,
        inner: Punctuation::RBracket,
    })
}

pub(super) fn generics<'i>(
) -> impl Parser<'i, Token, Spanned<Generics>, extra::Err<Rich<'i, Token, Span>>> {
    lbracket()
        .then(word().repeated().at_least(1))
        .then(rbracket())
        .map_with_span(|((left_bracket, tys), right_bracket), span| Spanned {
            span,
            inner: Generics {
                left_bracket,
                tys,
                right_bracket,
            },
        })
}

pub(super) fn proc<'i, 'd>(
) -> impl Parser<'i, Token, Spanned<TopLevel>, extra::Err<Rich<'i, Token, Span>>> + 'd {
    kw_proc()
        .debug("kw_proc")
        .then(generics().or_not().debug("Generics"))
        .then(word().debug("Proc name"))
        .then(proc_signature().debug("Proc signature"))
        .then(kw_do().debug("kw_do"))
        .then(expr().debug("expr").repeated())
        .then(kw_end().debug("kw_end"))
        .map_with_span(
            |((((((proc, generics), name), signature), do_), body), end), span| Spanned {
                span,
                inner: TopLevel::Proc(Proc {
                    proc,
                    generics,
                    name,
                    signature,
                    do_,
                    body,
                    end,
                }),
            },
        )
        .debug("proc")
}

pub(super) fn const_signature<'i>(
) -> impl Parser<'i, Token, Spanned<ConstSignature>, extra::Err<Rich<'i, Token, Span>>> + Clone {
    separator()
        .then(ty().repeated().at_least(1))
        .map_with_span(|(sep, tys), span| Spanned {
            span,
            inner: ConstSignature { sep, tys },
        })
}

pub(super) fn const_<'i, 'd>(
) -> impl Parser<'i, Token, Spanned<TopLevel>, extra::Err<Rich<'i, Token, Span>>> + Clone + 'd {
    kw_const()
        .then(word())
        .then(const_signature())
        .then(kw_do())
        .then(expr().repeated())
        .then(kw_end())
        .map_with_span(
            |(((((const_, name), signature), do_), body), end), span| Spanned {
                span,
                inner: TopLevel::Const(Const {
                    const_,
                    name,
                    signature,
                    do_,
                    body,
                    end,
                }),
            },
        )
}

pub(super) fn mem<'i, 'd>(
) -> impl Parser<'i, Token, Spanned<TopLevel>, extra::Err<Rich<'i, Token, Span>>> + Clone + 'd {
    kw_mem()
        .then(word())
        .then(kw_do())
        .then(expr().repeated())
        .then(kw_end())
        .map_with_span(|((((mem, name), do_), body), end), span| Spanned {
            span,
            inner: TopLevel::Mem(Mem {
                mem,
                name,
                do_,
                body,
                end,
            }),
        })
}

pub(super) fn name_type_pair<'i>(
) -> impl Parser<'i, Token, Spanned<NameTypePair>, extra::Err<Rich<'i, Token, Span>>> + Clone {
    word()
        .then(separator())
        .then(ty())
        .map_with_span(|((name, sep), ty), span| Spanned {
            span,
            inner: NameTypePair { name, sep, ty },
        })
}

pub(super) fn struct_<'i>(
) -> impl Parser<'i, Token, Spanned<TopLevel>, extra::Err<Rich<'i, Token, Span>>> {
    kw_struct()
        .then(generics().or_not())
        .then(word())
        .then(kw_do())
        .then(name_type_pair().repeated())
        .then(kw_end())
        .map_with_span(
            |(((((struct_, generics), name), do_), body), end), span| Spanned {
                span,
                inner: TopLevel::Struct(Struct {
                    struct_,
                    generics,
                    name,
                    do_,
                    body,
                    end,
                }),
            },
        )
}

pub(super) fn include<'i>(
) -> impl Parser<'i, Token, Spanned<TopLevel>, extra::Err<Rich<'i, Token, Span>>> + Clone {
    kw_include()
        .then(path().repeated().then(kw_from()).or_not())
        .then(include_path())
        .map_with_span(|((include, qualifiers), path), span| Spanned {
            span,
            inner: TopLevel::Include(Include {
                include,
                qualifiers: if let Some((items, from)) = qualifiers {
                    Some(Qualifiers { items, from })
                } else {
                    None
                },
                path,
            }),
        })
}

pub(super) fn file<'i, 'd>() -> impl Parser<'i, Token, File, extra::Err<Rich<'i, Token, Span>>> + 'd
{
    choice((
        include(),
        proc(),
        const_(),
        mem(),
        var_toplevel(),
        struct_(),
    ))
    .repeated()
    .then_ignore(end())
    .or(end().to(vec![]))
    .map(File)
}
