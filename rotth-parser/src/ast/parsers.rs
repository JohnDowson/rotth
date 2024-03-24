use chumsky::{input::ValueInput, prelude::*};
use internment::Intern;
use logos::Logos;
use rotth_lexer::Token;
use smol_str::SmolStr;
use somok::Either;
use spanner::{Span, Spanned};
use std::path::PathBuf;

use crate::types::{Custom, Primitive, Type};

use super::{
    Bind, Cast, Cond, CondBranch, Const, ConstSignature, Else, Expr, FieldAccess, File,
    GenericParams, Generics, If, ItemPathBuf, Keyword, Literal, ModuleDef, NameTypePair, Proc,
    ProcSignature, Punctuation, Read, Struct, TopLevel, Use, Var, While, Word, Write,
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

fn ty<'i, I>() -> impl RParser<'i, I, Type>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    recursive(|ty| {
        let generic_params = lbracket()
            .then(ty.repeated().at_least(1).collect())
            .then(rbracket())
            .map_with(|((left_bracket, tys), right_bracket), span| Spanned {
                span: span.span(),
                inner: GenericParams {
                    left_bracket,
                    tys,
                    right_bracket,
                },
            });
        just(Token::Ptr)
            .repeated()
            .collect::<Vec<_>>()
            .then(path().then(generic_params.or_not()))
            .map_with(|(ptr, (ty, params)), span| {
                let mut ty = if let Some(type_name) = ty.only() {
                    match type_name {
                        "void" => Type::Primitive(Primitive::Void),
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

                Spanned {
                    span: span.span(),
                    inner: ty,
                }
            })
    })
}

pub(super) fn parse_string(
    s: &str,
    path: Intern<PathBuf>,
) -> Result<SmolStr, Rich<'_, Token, Span>> {
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

fn literal<'i, I>() -> impl RParser<'i, I, Literal>
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
        Token::String(s) = span => {
            let span: spanner::Span = span.span();
            let s = parse_string(&s, span.context()).ok()?;
            Spanned{ span, inner: Literal::String(s) }},
        Token::Char(c) = span => Spanned { span: span.span(), inner: Literal::Char(c) },
    }
}

pub(super) fn literal_expr<'i, I>(
) -> impl Parser<'i, I, Spanned<Expr>, extra::Err<Rich<'i, Token, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    literal().map(|Spanned { span, inner }| Spanned {
        span,
        inner: Expr::Literal(inner),
    })
}

pub(super) fn kw_module<'i, I>(
) -> impl Parser<'i, I, Spanned<Keyword>, extra::Err<Rich<'i, Token, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::KwModule = span => Spanned { span: span.span(), inner: Keyword::Module },
    }
}

pub(super) fn kw_use<'i, I>(
) -> impl Parser<'i, I, Spanned<Keyword>, extra::Err<Rich<'i, Token, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::KwUse= span => Spanned { span: span.span(), inner: Keyword::Use },
    }
}

pub(super) fn kw_from<'i, I>(
) -> impl Parser<'i, I, Spanned<Keyword>, extra::Err<Rich<'i, Token, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::KwFrom = span => Spanned { span: span.span(), inner: Keyword::From },
    }
}

pub(super) fn kw_bind<'i, I>(
) -> impl Parser<'i, I, Spanned<Keyword>, extra::Err<Rich<'i, Token, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::KwBind = span => Spanned { span: span.span(), inner: Keyword::Bind },
    }
}
pub(super) fn kw_while<'i, I>(
) -> impl Parser<'i, I, Spanned<Keyword>, extra::Err<Rich<'i, Token, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::KwWhile = span => Spanned { span: span.span(), inner: Keyword::While },
    }
}
pub(super) fn kw_cond<'i, I>(
) -> impl Parser<'i, I, Spanned<Keyword>, extra::Err<Rich<'i, Token, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::KwCond = span => Spanned { span: span.span(), inner: Keyword::Cond },
    }
}
pub(super) fn kw_if<'i, I>(
) -> impl Parser<'i, I, Spanned<Keyword>, extra::Err<Rich<'i, Token, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::KwIf = span => Spanned { span: span.span(), inner: Keyword::If },
    }
}
pub(super) fn kw_else<'i, I>(
) -> impl Parser<'i, I, Spanned<Keyword>, extra::Err<Rich<'i, Token, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::KwElse = span => Spanned { span: span.span(), inner: Keyword::Else },
    }
}
pub(super) fn kw_do<'i, I>(
) -> impl Parser<'i, I, Spanned<Keyword>, extra::Err<Rich<'i, Token, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::KwDo = span => Spanned { span: span.span(), inner: Keyword::Do },
    }
}
pub(super) fn kw_end<'i, I>(
) -> impl Parser<'i, I, Spanned<Keyword>, extra::Err<Rich<'i, Token, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::KwEnd = span => Spanned { span: span.span(), inner: Keyword::End },
    }
}
pub(super) fn kw_ret<'i, I>(
) -> impl Parser<'i, I, Spanned<Keyword>, extra::Err<Rich<'i, Token, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::KwReturn = span => Spanned { span: span.span(), inner: Keyword::Return },
    }
}
pub(super) fn kw_cast<'i, I>(
) -> impl Parser<'i, I, Spanned<Keyword>, extra::Err<Rich<'i, Token, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::KwCast = span => Spanned { span: span.span(), inner: Keyword::Cast },
    }
}
pub(super) fn kw_proc<'i, I>(
) -> impl Parser<'i, I, Spanned<Keyword>, extra::Err<Rich<'i, Token, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::KwProc = span => Spanned { span: span.span(), inner: Keyword::Proc },
    }
}

pub(super) fn kw_const<'i, I>(
) -> impl Parser<'i, I, Spanned<Keyword>, extra::Err<Rich<'i, Token, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::KwConst = span => Spanned { span: span.span(), inner: Keyword::Const },
    }
}

pub(super) fn kw_var<'i, I>(
) -> impl Parser<'i, I, Spanned<Keyword>, extra::Err<Rich<'i, Token, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::KwVar = span => Spanned { span: span.span(), inner: Keyword::Var },
    }
}

pub(super) fn kw_struct<'i, I>(
) -> impl Parser<'i, I, Spanned<Keyword>, extra::Err<Rich<'i, Token, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::KwStruct = span => Spanned { span: span.span(), inner: Keyword::Struct },
    }
}

pub(super) fn word<'i, I>(
) -> impl Parser<'i, I, Spanned<Word>, extra::Err<Rich<'i, Token, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::Word(w) = span => Spanned { span: span.span(), inner: Word(w) },
        Token::Operator(w) = span => Spanned { span: span.span(), inner: Word(w) },
    }
}

pub(super) fn word_expr<'i, I>(
) -> impl Parser<'i, I, Spanned<Expr>, extra::Err<Rich<'i, Token, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::Word(w) = span => Spanned { span: span.span(), inner: Expr::Word(Word(w)) },
        Token::Operator(w) = span => Spanned { span: span.span(), inner: Expr::Word(Word(w)) },
    }
}

pub(super) fn path<'i, I>(
) -> impl Parser<'i, I, Spanned<ItemPathBuf>, extra::Err<Rich<'i, Token, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::Word(w) =>  w,
        Token::Operator(w) =>  w,
    }
    .separated_by(just(Token::PathSep))
    .at_least(1)
    .collect::<Vec<_>>()
    .map_with(|ws, span| Spanned {
        span: span.span(),
        inner: ItemPathBuf::from(ws),
    })
}

pub(super) fn path_expr<'i, I>(
) -> impl Parser<'i, I, Spanned<Expr>, extra::Err<Rich<'i, Token, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::Word(w) =>  w,
        Token::Operator(w) =>  w,
    }
    .separated_by(just(Token::PathSep))
    .at_least(1)
    .collect::<Vec<_>>()
    .map_with(|ws, span| Spanned {
        span: span.span(),
        inner: Expr::Path(ItemPathBuf::from(ws)),
    })
}

pub(super) fn separator<'i, I>(
) -> impl Parser<'i, I, Spanned<Punctuation>, extra::Err<Rich<'i, Token, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::SigSep = span => Spanned { span: span.span(), inner: Punctuation::Colon },
    }
}

pub(super) fn read_expr<'i, I>(
) -> impl Parser<'i, I, Spanned<Expr>, extra::Err<Rich<'i, Token, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        t @ Token::Read = span => Spanned { span: span.span(), inner: t },
    }
    .then(ty())
    .map_with(|(read, ty), span| Spanned {
        span: span.span(),
        inner: Expr::Read(Read { read, ty }),
    })
}

pub(super) fn write_expr<'i, I>(
) -> impl Parser<'i, I, Spanned<Expr>, extra::Err<Rich<'i, Token, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        t @ Token::Write = span => Spanned { span: span.span(), inner: t },
    }
    .then(ty())
    .map_with(|(write, ty), span| Spanned {
        span: span.span(),
        inner: Expr::Write(Write { write, ty }),
    })
}

pub(super) fn var<'i, I>(
) -> impl Parser<'i, I, Spanned<Var>, extra::Err<Rich<'i, Token, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    kw_var()
        .then(word())
        .then(separator())
        .then(ty())
        .map_with(|(((var, name), sep), ty), span| Spanned {
            span: span.span(),
            inner: Var { var, name, sep, ty },
        })
}

pub(super) fn var_toplevel<'i, I>(
) -> impl Parser<'i, I, Spanned<TopLevel>, extra::Err<Rich<'i, Token, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    var().map(|Spanned { span, inner }| Spanned {
        span,
        inner: TopLevel::Var(inner),
    })
}

pub(super) fn var_expr<'i, I>(
) -> impl Parser<'i, I, Spanned<Expr>, extra::Err<Rich<'i, Token, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    var().map(|Spanned { span, inner }| Spanned {
        span,
        inner: Expr::Var(inner),
    })
}

pub(super) fn accessor<'i, I>(
) -> impl Parser<'i, I, Spanned<Punctuation>, extra::Err<Rich<'i, Token, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::FieldAccess).map_with(|_, span| Spanned {
        span: span.span(),
        inner: Punctuation::RArrow,
    })
}

pub(super) fn expr<'i: 'd, 'd, I>(
) -> impl Parser<'i, I, Spanned<Expr>, extra::Err<Rich<'i, Token, Span>>> + Clone + 'd
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    recursive(|expr| {
        let bind = kw_bind()
            .then(
                name_type_pair()
                    .map(|b| b.map(Either::Right))
                    .or(word().map(|b| b.map(Either::Left)))
                    .repeated()
                    .at_least(1)
                    .collect::<Vec<_>>(),
            )
            .then(kw_do())
            .then(expr.clone().repeated().collect())
            .then(kw_end())
            .map_with(|((((bind, bindings), do_), body), end), span| Spanned {
                span: span.span(),
                inner: Expr::Bind(Bind {
                    bind,
                    bindings,
                    do_,
                    body,
                    end,
                }),
            });

        let while_ = kw_while()
            .then(expr.clone().repeated().collect())
            .then(kw_do())
            .then(expr.clone().repeated().collect())
            .then(kw_end())
            .map_with(|((((while_, cond), do_), body), end), span| Spanned {
                span: span.span(),
                inner: Expr::While(While {
                    while_,
                    cond,
                    do_,
                    body,
                    end,
                }),
            });

        let lie = kw_else()
            .then(expr.clone().repeated().collect())
            .map(|(else_, body)| Else { else_, body });

        let if_ = kw_if()
            .then(expr.clone().repeated().collect())
            .then(lie.or_not())
            .then(kw_end())
            .map_with(|(((if_, truth), lie), end), span| Spanned {
                span: span.span(),
                inner: Expr::If(If {
                    if_,
                    truth,
                    lie,
                    end,
                }),
            });

        let cast = kw_cast().then(ty()).map_with(|(cast, ty), span| Spanned {
            span: span.span(),
            inner: Expr::Cast(Cast { cast, ty }),
        });

        let pat = choice((literal_expr(), word_expr(), path_expr()));
        let cond_branch = kw_else()
            .then(pat.clone())
            .then(kw_do())
            .then(expr.clone().repeated().collect())
            .map_with(|(((else_, pat), do_), body), span| Spanned {
                span: span.span(),
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
            .then(expr.clone().repeated().collect())
            .then(cond_branch.repeated().collect())
            .then(kw_end())
            .map_with(
                |(((((cond, pat), do_), body), branches), end), span| Spanned {
                    span: span.span(),
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
            .map_with(|(access, field), span| Spanned {
                span: span.span(),
                inner: Expr::FieldAccess(Box::new(FieldAccess { access, field })),
            });

        let ret_expr = kw_ret().map(|Spanned { span, inner }| Spanned {
            span,
            inner: Expr::Keyword(inner),
        });

        let comp_stop = just(Token::CompStop).map_with(|_, span| Spanned {
            span: span.span(),
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

pub(super) fn proc_signature<'i, I>(
) -> impl Parser<'i, I, Spanned<ProcSignature>, extra::Err<Rich<'i, Token, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    ty().repeated()
        .collect::<Vec<_>>()
        .then(
            separator()
                .then(ty().repeated().at_least(1).collect::<Vec<_>>())
                .or_not(),
        )
        .map_with(|(ins, maybe_outs), span| {
            let (sep, outs) = if let Some((sep, outs)) = maybe_outs {
                (Some(sep), Some(outs))
            } else {
                (None, None)
            };
            Spanned {
                span: span.span(),
                inner: ProcSignature { ins, sep, outs },
            }
        })
}

pub(super) fn lbracket<'i, I>(
) -> impl Parser<'i, I, Spanned<Punctuation>, extra::Err<Rich<'i, Token, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::LBracket).map_with(|_, span| Spanned {
        span: span.span(),
        inner: Punctuation::LBracket,
    })
}
pub(super) fn rbracket<'i, I>(
) -> impl Parser<'i, I, Spanned<Punctuation>, extra::Err<Rich<'i, Token, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::RBracket).map_with(|_, span| Spanned {
        span: span.span(),
        inner: Punctuation::RBracket,
    })
}

pub(super) fn generics<'i, I>(
) -> impl Parser<'i, I, Spanned<Generics>, extra::Err<Rich<'i, Token, Span>>>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    lbracket()
        .then(word().repeated().at_least(1).collect::<Vec<_>>())
        .then(rbracket())
        .map_with(|((left_bracket, tys), right_bracket), span| Spanned {
            span: span.span(),
            inner: Generics {
                left_bracket,
                tys,
                right_bracket,
            },
        })
}

pub(super) fn proc<'i: 'd, 'd, I>(
) -> impl Parser<'i, I, Spanned<TopLevel>, extra::Err<Rich<'i, Token, Span>>> + 'd
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    kw_proc()
        .then(generics().or_not())
        .then(word())
        .then(proc_signature())
        .then(kw_do())
        .then(expr().repeated().collect::<Vec<_>>())
        .then(kw_end())
        .map_with(
            |((((((proc, generics), name), signature), do_), body), end), span| Spanned {
                span: span.span(),
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
}

pub(super) fn const_signature<'i, I>(
) -> impl Parser<'i, I, Spanned<ConstSignature>, extra::Err<Rich<'i, Token, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    separator()
        .then(ty().repeated().at_least(1).collect())
        .map_with(|(sep, tys), span| Spanned {
            span: span.span(),
            inner: ConstSignature { sep, tys },
        })
}

pub(super) fn const_<'i: 'd, 'd, I>(
) -> impl Parser<'i, I, Spanned<TopLevel>, extra::Err<Rich<'i, Token, Span>>> + Clone + 'd
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    kw_const()
        .then(word())
        .then(const_signature())
        .then(kw_do())
        .then(expr().repeated().collect())
        .then(kw_end())
        .map_with(
            |(((((const_, name), signature), do_), body), end), span| Spanned {
                span: span.span(),
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

pub(super) fn name_type_pair<'i, I>(
) -> impl Parser<'i, I, Spanned<NameTypePair>, extra::Err<Rich<'i, Token, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    word()
        .then(separator())
        .then(ty())
        .map_with(|((name, sep), ty), span| Spanned {
            span: span.span(),
            inner: NameTypePair { name, sep, ty },
        })
}

pub(super) fn struct_<'i, I>(
) -> impl Parser<'i, I, Spanned<TopLevel>, extra::Err<Rich<'i, Token, Span>>>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    kw_struct()
        .then(generics().or_not())
        .then(word())
        .then(kw_do())
        .then(name_type_pair().repeated().collect())
        .then(kw_end())
        .map_with(
            |(((((struct_, generics), name), do_), body), end), span| Spanned {
                span: span.span(),
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

pub(super) fn module<'i, I>(
) -> impl Parser<'i, I, Spanned<TopLevel>, extra::Err<Rich<'i, Token, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    kw_module()
        .then(word())
        .map_with(|(module, name), span| Spanned {
            span: span.span(),
            inner: TopLevel::Module(ModuleDef { module, name }),
        })
}

pub(super) fn use_<'i, I>(
) -> impl Parser<'i, I, Spanned<TopLevel>, extra::Err<Rich<'i, Token, Span>>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    kw_use().then(word().then(kw_from())).then(path()).map_with(
        |((use_, (name, from)), path), span| Spanned {
            span: span.span(),
            inner: TopLevel::Use(Use {
                use_,
                name,
                from,
                path,
            }),
        },
    )
}

pub(super) fn file<'i: 'd, 'd, I>(
) -> impl Parser<'i, I, File, extra::Err<Rich<'i, Token, Span>>> + 'd
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    choice((
        module(),
        proc(),
        const_(),
        var_toplevel(),
        struct_(),
        use_(),
    ))
    .repeated()
    .collect()
    .then_ignore(end())
    .or(end().to(vec![]))
    .map(File)
}
