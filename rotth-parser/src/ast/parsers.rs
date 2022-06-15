use chumsky::prelude::*;
use logos::Logos;
use rotth_lexer::Token;
use somok::Either;
use spanner::{Span, Spanned};
use std::path::{Path, PathBuf};

use crate::types::{Primitive, Type};

use super::{
    Bind, Cast, Cond, CondBranch, Const, ConstSignature, Else, Expr, FieldAccess, File, Generics,
    If, Include, ItemPathBuf, Keyword, Literal, Mem, NameTypePair, Proc, ProcSignature,
    Punctuation, Struct, TopLevel, Var, While, Word,
};

pub(super) fn ty() -> impl Parser<Token, Spanned<Type>, Error = Simple<Token, Span>> + Clone {
    just(Token::Ptr)
        .repeated()
        .then(path())
        .map_with_span(|(ptr, ty), span| {
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
                    _ => Type::Custom(ty.inner),
                }
            } else {
                Type::Custom(ty.inner)
            };
            for _ in ptr {
                ty = Type::Ptr(box ty)
            }

            Spanned { span, inner: ty }
        })
}

pub(super) fn parse_string(s: &str, path: &'static Path) -> Result<String, Simple<Token, Span>> {
    #[derive(Logos)]
    pub enum StrToken {
        #[regex(r"\\.", |l| l.slice().chars().nth(1))]
        Escaped(char),
        #[regex(r"[^\\]", |l| l.slice().chars().next())]
        Char(char),
        #[token(r#"""#)]
        End,
        #[error]
        Error,
    }

    StrToken::lexer(s)
        .spanned()
        .filter_map(|(t, s)| match t {
            StrToken::Escaped(c) => match c {
                'n' => Some(Ok('\n')),
                't' => Some(Ok('\t')),
                '"' => Some(Ok('"')),
                _ => Some(Err(Simple::custom(
                    Span::new(path, s.start, s.end),
                    "Invalid character escape",
                ))),
            },
            StrToken::Char(c) => Some(Ok(c)),
            StrToken::End => None,
            StrToken::Error => Some(Err(Simple::custom(
                Span::new(path, s.start, s.end),
                "Invalid string character",
            ))),
        })
        .collect()
}

pub(super) fn literal() -> impl Parser<Token, Spanned<Literal>, Error = Simple<Token, Span>> + Clone
{
    select! {
        Token::Bool(b), span => Spanned { span, inner: Literal::Bool(b)},
        Token::Num(n), span => Spanned{ span, inner: Literal::Num(n.parse().unwrap()) },
        Token::String(s), span => Spanned{ span, inner: Literal::String(parse_string(&*s, span.context())?) },
        Token::Char(c), span => Spanned { span, inner: Literal::Char(c) },
    }
}

pub(super) fn literal_expr(
) -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token, Span>> + Clone {
    literal().map(|Spanned { span, inner }| Spanned {
        span,
        inner: Expr::Literal(inner),
    })
}

pub(super) fn include_path(
) -> impl Parser<Token, Spanned<PathBuf>, Error = Simple<Token, Span>> + Clone {
    select! {
        Token::String(s), span => Spanned {
            span,
            inner: {
                let mut chars = (&*s).chars();
                chars.next();
                chars.next_back();
                PathBuf::from(chars.as_str())
            }
        },
    }
}

pub(super) fn kw_include(
) -> impl Parser<Token, Spanned<Keyword>, Error = Simple<Token, Span>> + Clone {
    select! {
        Token::KwInclude, span => Spanned { span, inner: Keyword::Include },
    }
}

pub(super) fn kw_bind() -> impl Parser<Token, Spanned<Keyword>, Error = Simple<Token, Span>> + Clone
{
    select! {
        Token::KwBind, span => Spanned { span, inner: Keyword::Bind },
    }
}
pub(super) fn kw_while() -> impl Parser<Token, Spanned<Keyword>, Error = Simple<Token, Span>> + Clone
{
    select! {
        Token::KwWhile, span => Spanned { span, inner: Keyword::While },
    }
}
pub(super) fn kw_cond() -> impl Parser<Token, Spanned<Keyword>, Error = Simple<Token, Span>> + Clone
{
    select! {
        Token::KwCond, span => Spanned { span, inner: Keyword::Cond },
    }
}
pub(super) fn kw_if() -> impl Parser<Token, Spanned<Keyword>, Error = Simple<Token, Span>> + Clone {
    select! {
        Token::KwIf, span => Spanned { span, inner: Keyword::If },
    }
}
pub(super) fn kw_else() -> impl Parser<Token, Spanned<Keyword>, Error = Simple<Token, Span>> + Clone
{
    select! {
        Token::KwElse, span => Spanned { span, inner: Keyword::Else },
    }
}
pub(super) fn kw_do() -> impl Parser<Token, Spanned<Keyword>, Error = Simple<Token, Span>> + Clone {
    select! {
        Token::KwDo, span => Spanned { span, inner: Keyword::Do },
    }
}
pub(super) fn kw_end() -> impl Parser<Token, Spanned<Keyword>, Error = Simple<Token, Span>> + Clone
{
    select! {
        Token::KwEnd, span => Spanned { span, inner: Keyword::End },
    }
}
pub(super) fn kw_ret() -> impl Parser<Token, Spanned<Keyword>, Error = Simple<Token, Span>> + Clone
{
    select! {
        Token::KwReturn, span => Spanned { span, inner: Keyword::Return },
    }
}
pub(super) fn kw_cast() -> impl Parser<Token, Spanned<Keyword>, Error = Simple<Token, Span>> + Clone
{
    select! {
        Token::KwCast, span => Spanned { span, inner: Keyword::Cast },
    }
}
pub(super) fn kw_proc() -> impl Parser<Token, Spanned<Keyword>, Error = Simple<Token, Span>> + Clone
{
    select! {
        Token::KwProc, span => Spanned { span, inner: Keyword::Proc },
    }
}
pub(super) fn kw_const() -> impl Parser<Token, Spanned<Keyword>, Error = Simple<Token, Span>> + Clone
{
    select! {
        Token::KwConst, span => Spanned { span, inner: Keyword::Const },
    }
}
pub(super) fn kw_mem() -> impl Parser<Token, Spanned<Keyword>, Error = Simple<Token, Span>> + Clone
{
    select! {
        Token::KwMem, span => Spanned { span, inner: Keyword::Mem },
    }
}
pub(super) fn kw_var() -> impl Parser<Token, Spanned<Keyword>, Error = Simple<Token, Span>> + Clone
{
    select! {
        Token::KwVar, span => Spanned { span, inner: Keyword::Var },
    }
}
pub(super) fn kw_struct(
) -> impl Parser<Token, Spanned<Keyword>, Error = Simple<Token, Span>> + Clone {
    select! {
        Token::KwStruct, span => Spanned { span, inner: Keyword::Struct },
    }
}

pub(super) fn word() -> impl Parser<Token, Spanned<Word>, Error = Simple<Token, Span>> + Clone {
    select! {
        Token::Word(w), span => Spanned { span, inner: Word(w) },
    }
}

pub(super) fn word_expr() -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token, Span>> + Clone
{
    select! {
        Token::Word(w), span => Spanned { span, inner: Expr::Word(Word(w)) },
    }
}

pub(super) fn path() -> impl Parser<Token, Spanned<ItemPathBuf>, Error = Simple<Token, Span>> + Clone
{
    select! {
        Token::Word(w) =>  w,
    }
    .separated_by(just(Token::PathSep))
    .at_least(1)
    .map_with_span(|ws, span| Spanned {
        span,
        inner: ItemPathBuf::from(ws),
    })
}

pub(super) fn path_expr() -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token, Span>> + Clone
{
    select! {
        Token::Word(w) =>  w,
    }
    .separated_by(just(Token::PathSep))
    .at_least(1)
    .map_with_span(|ws, span| Spanned {
        span,
        inner: Expr::Path(ItemPathBuf::from(ws)),
    })
}

pub(super) fn separator(
) -> impl Parser<Token, Spanned<Punctuation>, Error = Simple<Token, Span>> + Clone {
    select! {
        Token::SigSep, span => Spanned { span, inner: Punctuation::Colon },
    }
}

pub(super) fn var() -> impl Parser<Token, Spanned<Var>, Error = Simple<Token, Span>> + Clone {
    kw_var()
        .then(word())
        .then(separator())
        .then(ty())
        .map_with_span(|(((var, name), sep), ty), span| Spanned {
            span,
            inner: Var { var, name, sep, ty },
        })
}

pub(super) fn var_toplevel(
) -> impl Parser<Token, Spanned<TopLevel>, Error = Simple<Token, Span>> + Clone {
    var().map(|Spanned { span, inner }| Spanned {
        span,
        inner: TopLevel::Var(inner),
    })
}

pub(super) fn var_expr() -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token, Span>> + Clone {
    var().map(|Spanned { span, inner }| Spanned {
        span,
        inner: Expr::Var(inner),
    })
}

pub(super) fn accessor(
) -> impl Parser<Token, Spanned<Punctuation>, Error = Simple<Token, Span>> + Clone {
    just(Token::FieldAccess).map_with_span(|_, span| Spanned {
        span,
        inner: Punctuation::RArrow,
    })
}

pub(super) fn expr<'d>(
) -> impl Parser<Token, Spanned<Expr>, Error = Simple<Token, Span>> + Clone + 'd {
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
                    inner: Expr::Cond(box Cond {
                        cond,
                        pat,
                        do_,
                        body,
                        branches,
                        end,
                    }),
                },
            );

        let field_access = accessor()
            .then(word())
            .map_with_span(|(access, field), span| Spanned {
                span,
                inner: Expr::FieldAccess(box FieldAccess { access, field }),
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

pub(super) fn proc_signature(
) -> impl Parser<Token, Spanned<ProcSignature>, Error = Simple<Token, Span>> + Clone {
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

pub(super) fn lbracket(
) -> impl Parser<Token, Spanned<Punctuation>, Error = Simple<Token, Span>> + Clone {
    just(Token::LBracket).map_with_span(|_, span| Spanned {
        span,
        inner: Punctuation::LBracket,
    })
}
pub(super) fn rbracket(
) -> impl Parser<Token, Spanned<Punctuation>, Error = Simple<Token, Span>> + Clone {
    just(Token::RBracket).map_with_span(|_, span| Spanned {
        span,
        inner: Punctuation::RBracket,
    })
}

pub(super) fn generics() -> impl Parser<Token, Spanned<Generics>, Error = Simple<Token, Span>> {
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

pub(super) fn proc<'d>() -> impl Parser<Token, Spanned<TopLevel>, Error = Simple<Token, Span>> + 'd
{
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

pub(super) fn const_signature(
) -> impl Parser<Token, Spanned<ConstSignature>, Error = Simple<Token, Span>> + Clone {
    separator()
        .then(ty().repeated().at_least(1))
        .map_with_span(|(sep, tys), span| Spanned {
            span,
            inner: ConstSignature { sep, tys },
        })
}

pub(super) fn const_<'d>(
) -> impl Parser<Token, Spanned<TopLevel>, Error = Simple<Token, Span>> + Clone + 'd {
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

pub(super) fn mem<'d>(
) -> impl Parser<Token, Spanned<TopLevel>, Error = Simple<Token, Span>> + Clone + 'd {
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

pub(super) fn name_type_pair(
) -> impl Parser<Token, Spanned<NameTypePair>, Error = Simple<Token, Span>> + Clone {
    word()
        .then(separator())
        .then(ty())
        .map_with_span(|((name, sep), ty), span| Spanned {
            span,
            inner: NameTypePair { name, sep, ty },
        })
}

pub(super) fn struct_() -> impl Parser<Token, Spanned<TopLevel>, Error = Simple<Token, Span>> + Clone
{
    kw_struct()
        .then(word())
        .then(kw_do())
        .then(name_type_pair().repeated())
        .then(kw_end())
        .map_with_span(|((((struct_, name), do_), body), end), span| Spanned {
            span,
            inner: TopLevel::Struct(Struct {
                struct_,
                name,
                do_,
                body,
                end,
            }),
        })
}

pub(super) fn include() -> impl Parser<Token, Spanned<TopLevel>, Error = Simple<Token, Span>> + Clone
{
    kw_include()
        .then(word().repeated())
        .then(include_path())
        .map_with_span(|((include, qualifiers), path), span| Spanned {
            span,
            inner: TopLevel::Include(Include {
                include,
                qualifiers,
                path,
            }),
        })
}

pub(super) fn file<'d>() -> impl Parser<Token, File, Error = Simple<Token, Span>> + 'd {
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
