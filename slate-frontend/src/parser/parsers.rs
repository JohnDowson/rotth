use std::path::PathBuf;

use chumsky::{input::ValueInput, pratt, prelude::*, primitive::select};
use internment::Intern;
use itempath::ItemPathBuf;
use logos::Logos;
use spanner::{Span, Spanned};

use crate::lexer::Token;

use super::{
    ast::{
        Binary, Call, Cast, Const, ConstSignature, Else, Expr, FieldAccess, GenericParams,
        Generics, If, Keyword, Let, Literal, Match, MatchBranch, Module, ModuleDef, NameTypePair,
        Proc, ProcSignature, Punctuation, Read, Static, Struct, TopLevel, Unary, Use, While, Word,
    },
    types::{Custom, Primitive, Type},
};

trait RParser<'i, I, O>
where
    Self: Parser<'i, I, Spanned<O>, extra::Full<Rich<'i, Token, Span>, Vec<usize>, usize>> + Clone,
    I: ValueInput<'i, Token = Token, Span = Span>,
{
}

impl<'i, I, O, T> RParser<'i, I, O> for T
where
    T: Parser<'i, I, Spanned<O>, extra::Full<Rich<'i, Token, Span>, Vec<usize>, usize>> + Clone,
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
        just(Token::OpAnd)
            .repeated()
            .collect::<Vec<_>>()
            .then(path().then(generic_params.or_not()))
            .map_with(|(ptr, (ty, params)), span| {
                let mut ty = if let Some(type_name) = ty.inner.first() {
                    match &**type_name {
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

fn parse_string(s: &str, path: Intern<PathBuf>) -> Result<Intern<String>, Rich<'_, Token, Span>> {
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
        .collect::<Result<String, _>>()
        .map(Into::into)
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
    .labelled("Literal")
}

fn literal_expr<'i, I>() -> impl RParser<'i, I, Expr>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    literal().map(|Spanned { span, inner }| Spanned {
        span,
        inner: Expr::Literal(inner),
    })
}

fn kw_module<'i, I>() -> impl RParser<'i, I, Keyword>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwModule).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Keyword::Module,
    })
}

fn kw_use<'i, I>() -> impl RParser<'i, I, Keyword>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwUse).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Keyword::Use,
    })
}

fn kw_from<'i, I>() -> impl RParser<'i, I, Keyword>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwFrom).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Keyword::From,
    })
}

fn kw_let<'i, I>() -> impl RParser<'i, I, Keyword>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwLet).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Keyword::Let,
    })
}

fn kw_while<'i, I>() -> impl RParser<'i, I, Keyword>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwWhile).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Keyword::While,
    })
}
fn kw_match<'i, I>() -> impl RParser<'i, I, Keyword>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwMatch).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Keyword::Match,
    })
}
fn kw_if<'i, I>() -> impl RParser<'i, I, Keyword>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwIf).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Keyword::If,
    })
}
fn kw_else<'i, I>() -> impl RParser<'i, I, Keyword>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwElse).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Keyword::Else,
    })
}
fn kw_do<'i, I>() -> impl RParser<'i, I, Keyword>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwDo).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Keyword::Do,
    })
}
fn kw_ret<'i, I>() -> impl RParser<'i, I, Keyword>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwReturn).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Keyword::Return,
    })
}
fn kw_cast<'i, I>() -> impl RParser<'i, I, Keyword>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwCast).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Keyword::Cast,
    })
}
fn kw_proc<'i, I>() -> impl RParser<'i, I, Keyword>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwProc).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Keyword::Proc,
    })
}

fn kw_const<'i, I>() -> impl RParser<'i, I, Keyword>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwConst).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Keyword::Const,
    })
}

fn kw_static<'i, I>() -> impl RParser<'i, I, Keyword>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwStatic).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Keyword::Static,
    })
}

fn kw_struct<'i, I>() -> impl RParser<'i, I, Keyword>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwStruct).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Keyword::Struct,
    })
}

fn op_and<'i, I>() -> impl RParser<'i, I, Punctuation>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::OpAnd).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Punctuation::And,
    })
}

fn op_mul<'i, I>() -> impl RParser<'i, I, Punctuation>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::OpMul).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Punctuation::Mul,
    })
}

fn op_div<'i, I>() -> impl RParser<'i, I, Punctuation>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::OpDiv).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Punctuation::Div,
    })
}

fn op_plus<'i, I>() -> impl RParser<'i, I, Punctuation>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::OpPlus).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Punctuation::Plus,
    })
}

fn op_minus<'i, I>() -> impl RParser<'i, I, Punctuation>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::OpMinus).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Punctuation::Minus,
    })
}

fn op_assign<'i, I>() -> impl RParser<'i, I, Punctuation>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::OpAssign).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Punctuation::Assign,
    })
}

fn op_eq<'i, I>() -> impl RParser<'i, I, Punctuation>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::OpEq).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Punctuation::Eq,
    })
}

fn op_dot<'i, I>() -> impl RParser<'i, I, Punctuation>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::OpDot).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Punctuation::Dot,
    })
}

fn fat_arrow<'i, I>() -> impl RParser<'i, I, Punctuation>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::FatArrow).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Punctuation::FatArrow,
    })
}

fn word<'i, I>() -> impl RParser<'i, I, Word>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::Word(w) = span => Spanned { span: span.span(), inner: Word(w) },
    }
}

fn word_expr<'i, I>() -> impl RParser<'i, I, Expr>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::Word(w) = span => Spanned { span: span.span(), inner: Expr::Word(Word(w)) },
    }
}

fn path<'i, I>() -> impl RParser<'i, I, ItemPathBuf>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::Word(w) =>  w,
    }
    .separated_by(just(Token::DoubleColon))
    .at_least(1)
    .collect::<Vec<_>>()
    .map_with(|ws, span| Spanned {
        span: span.span(),
        inner: ItemPathBuf::from(ws),
    })
}

fn path_expr<'i, I>() -> impl RParser<'i, I, Expr>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::Word(w) =>  w,
    }
    .separated_by(just(Token::DoubleColon))
    .at_least(1)
    .collect::<Vec<_>>()
    .map_with(|ws, span| Spanned {
        span: span.span(),
        inner: Expr::Path(ItemPathBuf::from(ws)),
    })
}

fn separator<'i, I>() -> impl RParser<'i, I, Punctuation>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::Colon).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Punctuation::Colon,
    })
}

fn read_expr<'i, I>() -> impl RParser<'i, I, Expr>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        t @ Token::OpMul = span => Spanned { span: span.span(), inner: t },
    }
    .then(ty())
    .map_with(|(read, ty), span| Spanned {
        span: span.span(),
        inner: Expr::Read(Read { read, ty }),
    })
}

fn static_<'i, I>() -> impl RParser<'i, I, Static>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    kw_static()
        .then(word())
        .then(separator())
        .then(ty())
        .map_with(|(((static_, name), sep), ty), span| Spanned {
            span: span.span(),
            inner: Static {
                static_,
                name,
                sep,
                ty,
            },
        })
}

fn static_toplevel<'i, I>() -> impl RParser<'i, I, TopLevel>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    static_().map(|Spanned { span, inner }| Spanned {
        span,
        inner: TopLevel::Var(inner),
    })
}

fn static_expr<'i, I>() -> impl RParser<'i, I, Expr>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    static_().map(|Spanned { span, inner }| Spanned {
        span,
        inner: Expr::Static(inner),
    })
}

fn expr<'i: 'd, 'd, I>() -> impl RParser<'i, I, Expr>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    recursive(|expr| {
        let while_ = kw_while()
            .then(expr.clone())
            .then(kw_do().map_with(|d, e| {
                e.state().push(0);
                d
            }))
            .then(body(expr.clone()))
            .map_with(|(((while_, cond), do_), body), span| Spanned {
                span: span.span(),
                inner: Expr::While(Box::new(While {
                    while_,
                    cond,
                    do_,
                    body,
                })),
            });

        let lie = kw_else()
            .then(expr.clone().repeated().collect())
            .map(|(else_, body)| Else { else_, body });

        let if_ = kw_if()
            .then(expr.clone().repeated().collect())
            .then(lie.or_not())
            .map_with(|((if_, truth), lie), span| Spanned {
                span: span.span(),
                inner: Expr::If(If { if_, truth, lie }),
            });

        let cast = kw_cast().then(ty()).map_with(|(cast, ty), span| Spanned {
            span: span.span(),
            inner: Expr::Cast(Cast { cast, ty }),
        });

        let pat = choice((literal_expr(), word_expr(), path_expr()));
        let match_branch = pat
            .clone()
            .then(fat_arrow())
            .then(choice((
                expr.clone().map(|e| vec![e]),
                kw_do().ignore_then(body(expr.clone())),
            )))
            .map_with(|((pat, fat_arrow), body), extra| Spanned {
                span: extra.span(),
                inner: MatchBranch {
                    pat,
                    fat_arrow,
                    body,
                },
            })
            .labelled("match branch");

        let match_ = kw_match()
            .then(expr.clone())
            .then(kw_do())
            .then(
                indent_increase().ignore_with_ctx(
                    match_branch
                        .separated_by(
                            select(|x, e| match x {
                                Token::Indent(level) if level == *dbg! {e.ctx()} => Some(()),
                                _ => None,
                            })
                            .labelled("matching indent level"),
                        )
                        .collect(),
                ),
            )
            .map_with(|(((match_, expr), do_), branches), extra| Spanned {
                span: extra.span(),
                inner: Expr::Cond(Box::new(Match {
                    match_,
                    expr,
                    do_,
                    branches,
                })),
            });

        let ret_expr = kw_ret().map(|Spanned { span, inner }| Spanned {
            span,
            inner: Expr::Keyword(inner),
        });

        let let_ = kw_let()
            .then(word())
            .then(op_assign())
            .then(expr.clone())
            .map_with(|(((let_, pat), assign), body), extra| Spanned {
                span: extra.span(),
                inner: Expr::Let(Box::new(Let {
                    let_,
                    pat,
                    assign,
                    body,
                })),
            });

        let call = lparen()
            .then(expr.separated_by(just(Token::Comma)).collect::<Vec<_>>())
            .then(rparen());

        choice((
            literal_expr(),
            static_expr(),
            path_expr(),
            word_expr(),
            read_expr(),
            let_,
            while_,
            if_,
            match_,
            cast,
            ret_expr,
        ))
        .pratt((
            pratt::prefix(1, op_and::<I>(), |op, expr, span| Spanned {
                span,
                inner: Expr::Ref(Box::new(Unary { op, expr })),
            }),
            pratt::infix(pratt::left(5), op_mul::<I>(), |left, op, right, span| {
                Spanned {
                    span,
                    inner: Expr::Mul(Box::new(Binary { left, op, right })),
                }
            }),
            pratt::prefix(1, op_mul::<I>(), |op, expr, span| Spanned {
                span,
                inner: Expr::Deref(Box::new(Unary { op, expr })),
            }),
            pratt::infix(pratt::left(5), op_div::<I>(), |left, op, right, span| {
                Spanned {
                    span,
                    inner: Expr::Div(Box::new(Binary { left, op, right })),
                }
            }),
            pratt::infix(pratt::left(4), op_plus::<I>(), |left, op, right, span| {
                Spanned {
                    span,
                    inner: Expr::Add(Box::new(Binary { left, op, right })),
                }
            }),
            pratt::infix(pratt::left(4), op_minus::<I>(), |left, op, right, span| {
                Spanned {
                    span,
                    inner: Expr::Sub(Box::new(Binary { left, op, right })),
                }
            }),
            pratt::infix(pratt::left(3), op_eq::<I>(), |left, op, right, span| {
                Spanned {
                    span,
                    inner: Expr::Eq(Box::new(Binary { left, op, right })),
                }
            }),
            pratt::infix(pratt::left(3), op_assign::<I>(), |left, op, right, span| {
                Spanned {
                    span,
                    inner: Expr::Assign(Box::new(Binary { left, op, right })),
                }
            }),
            pratt::postfix(6, op_dot::<I>().then(word()), |expr, (op, field), span| {
                Spanned {
                    span,
                    inner: Expr::FieldAccess(Box::new(FieldAccess {
                        reciever: expr,
                        access: op,
                        field,
                    })),
                }
            }),
            pratt::postfix(6, call, |callee, ((lparen, args), rparen), span| Spanned {
                span,
                inner: Expr::Call(Box::new(Call {
                    callee,
                    lparen,
                    args,
                    rparen,
                })),
            }),
        ))
    })
    .labelled("Expr")
}

fn indent_increase<'i, I>(
) -> impl Parser<'i, I, usize, extra::Full<Rich<'i, Token, Span>, Vec<usize>, usize>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select(|x, e| match x {
        Token::Indent(level) if level > *e.ctx() => {
            if level > *e.ctx() {
                Some(level)
            } else {
                None
            }
        }
        _ => None,
    })
    .labelled("Indent increase")
}

fn proc_signature<'i, I>() -> impl RParser<'i, I, ProcSignature>
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

fn lbracket<'i, I>() -> impl RParser<'i, I, Punctuation>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::LBracket).map_with(|_, span| Spanned {
        span: span.span(),
        inner: Punctuation::LBracket,
    })
}
fn rbracket<'i, I>() -> impl RParser<'i, I, Punctuation>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::RBracket).map_with(|_, span| Spanned {
        span: span.span(),
        inner: Punctuation::RBracket,
    })
}

fn lparen<'i, I>() -> impl RParser<'i, I, Punctuation>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::LParen).map_with(|_, span| Spanned {
        span: span.span(),
        inner: Punctuation::RParen,
    })
}
fn rparen<'i, I>() -> impl RParser<'i, I, Punctuation>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::RParen).map_with(|_, span| Spanned {
        span: span.span(),
        inner: Punctuation::RParen,
    })
}

fn generics<'i, I>() -> impl RParser<'i, I, Generics>
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

fn body<'i: 'd, 'd, I>(
    expr: impl RParser<'i, I, Expr>,
) -> impl Parser<'i, I, Vec<Spanned<Expr>>, extra::Full<Rich<'i, Token, Span>, Vec<usize>, usize>> + Clone
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    expr.clone()
        .then(
            select(|x, extra| match x {
                Token::Indent(level) if level == *extra.ctx() => Some(()),
                _ => None,
            })
            .ignore_then(expr)
            .repeated()
            .collect::<Vec<_>>(),
        )
        .map(|(first, mut rest)| {
            rest.insert(0, first);
            rest
        })
}

fn proc<'i: 'd, 'd, I>() -> impl RParser<'i, I, TopLevel>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    kw_proc()
        .then(generics().or_not())
        .then(word())
        .then(proc_signature())
        .then(kw_do())
        .then(indent_increase().ignore_with_ctx(body(expr())))
        .map_with(
            |(((((proc, generics), name), signature), do_), body), span| Spanned {
                span: span.span(),
                inner: TopLevel::Proc(Proc {
                    proc,
                    generics,
                    name,
                    signature,
                    do_,
                    body,
                }),
            },
        )
}

fn const_signature<'i, I>() -> impl RParser<'i, I, ConstSignature>
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

fn const_<'i: 'd, 'd, I>() -> impl RParser<'i, I, TopLevel>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    kw_const()
        .then(word())
        .then(const_signature())
        .then(kw_do())
        .then(expr().repeated().collect())
        .map_with(|((((const_, name), signature), do_), body), span| Spanned {
            span: span.span(),
            inner: TopLevel::Const(Const {
                const_,
                name,
                signature,
                do_,
                body,
            }),
        })
}

fn name_type_pair<'i, I>() -> impl RParser<'i, I, NameTypePair>
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

fn struct_<'i, I>() -> impl RParser<'i, I, TopLevel>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    kw_struct()
        .then(generics().or_not())
        .then(word())
        .then(kw_do())
        .then(name_type_pair().repeated().collect())
        .map_with(|((((struct_, generics), name), do_), body), span| Spanned {
            span: span.span(),
            inner: TopLevel::Struct(Struct {
                struct_,
                generics,
                name,
                do_,
                body,
            }),
        })
}

fn module<'i, I>() -> impl RParser<'i, I, TopLevel>
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

fn use_<'i, I>() -> impl RParser<'i, I, TopLevel>
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

pub fn file<'i: 'd, 'd, I>(
) -> impl Parser<'i, I, Module, extra::Full<Rich<'i, Token, Span>, Vec<usize>, usize>> + 'd
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    choice((
        module(),
        proc(),
        const_(),
        static_toplevel(),
        struct_(),
        use_(),
    ))
    .padded_by(select!(Token::Indent(_) => ()).or_not())
    .repeated()
    .collect()
    .then_ignore(end())
    .or(end().to(vec![]))
    .map(|items| {
        let mut module = Module {
            procs: Vec::new(),
            consts: Vec::new(),
            vars: Vec::new(),
            structs: Vec::new(),
            modules: Vec::new(),
            uses: Vec::new(),
        };

        for item in items {
            match item.inner {
                TopLevel::Proc(p) => module.procs.push(Spanned {
                    span: item.span,
                    inner: p,
                }),
                TopLevel::Const(c) => module.consts.push(Spanned {
                    span: item.span,
                    inner: c,
                }),
                TopLevel::Var(v) => module.vars.push(Spanned {
                    span: item.span,
                    inner: v,
                }),
                TopLevel::Struct(s) => module.structs.push(Spanned {
                    span: item.span,
                    inner: s,
                }),
                TopLevel::Use(u) => module.uses.push(Spanned {
                    span: item.span,
                    inner: u,
                }),
                TopLevel::Module(m) => module.modules.push(Spanned {
                    span: item.span,
                    inner: m,
                }),
            }
        }

        module
    })
    .with_state(Vec::new())
}
