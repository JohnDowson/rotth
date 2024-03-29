use std::path::PathBuf;

use chumsky::{input::ValueInput, pratt, prelude::*};
use internment::Intern;
use itempath::ItemPathBuf;
use logos::Logos;
use spanner::{Span, Spanned};

use crate::lexer::Token;

use super::{
    ast::{
        Binary, Call, Cast, Cond, CondBranch, Const, ConstSignature, Else, Expr, FieldAccess,
        GenericParams, Generics, If, Keyword, Let, Literal, Module, ModuleDef, NameTypePair, Proc,
        ProcSignature, Punctuation, Read, Static, Struct, TopLevel, Unary, Use, While, Word,
    },
    types::{Custom, Primitive, Type},
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
        just(Token::OpAnd)
            .repeated()
            .collect::<Vec<_>>()
            .then(path().then(generic_params.or_not()))
            .map_with(|(ptr, (ty, params)), span| {
                let mut ty = if let Some(type_name) = ty.inner.only() {
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
    select! {
        Token::KwModule = span => Spanned { span: span.span(), inner: Keyword::Module },
    }
}

fn kw_use<'i, I>() -> impl RParser<'i, I, Keyword>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::KwUse= span => Spanned { span: span.span(), inner: Keyword::Use },
    }
}

fn kw_from<'i, I>() -> impl RParser<'i, I, Keyword>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::KwFrom = span => Spanned { span: span.span(), inner: Keyword::From },
    }
}

fn kw_let<'i, I>() -> impl RParser<'i, I, Keyword>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::KwLet = span => Spanned { span: span.span(), inner: Keyword::Let },
    }
}

fn kw_while<'i, I>() -> impl RParser<'i, I, Keyword>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::KwWhile = span => Spanned { span: span.span(), inner: Keyword::While },
    }
}
fn kw_cond<'i, I>() -> impl RParser<'i, I, Keyword>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::KwMatch = span => Spanned { span: span.span(), inner: Keyword::Match },
    }
}
fn kw_if<'i, I>() -> impl RParser<'i, I, Keyword>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::KwIf = span => Spanned { span: span.span(), inner: Keyword::If },
    }
}
fn kw_else<'i, I>() -> impl RParser<'i, I, Keyword>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::KwElse = span => Spanned { span: span.span(), inner: Keyword::Else },
    }
}
fn kw_do<'i, I>() -> impl RParser<'i, I, Keyword>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::KwDo = span => Spanned { span: span.span(), inner: Keyword::Do },
    }
}
fn kw_end<'i, I>() -> impl RParser<'i, I, Keyword>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::KwEnd = span => Spanned { span: span.span(), inner: Keyword::End },
    }
}
fn kw_ret<'i, I>() -> impl RParser<'i, I, Keyword>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::KwReturn = span => Spanned { span: span.span(), inner: Keyword::Return },
    }
}
fn kw_cast<'i, I>() -> impl RParser<'i, I, Keyword>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::KwCast = span => Spanned { span: span.span(), inner: Keyword::Cast },
    }
}
fn kw_proc<'i, I>() -> impl RParser<'i, I, Keyword>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::KwProc = span => Spanned { span: span.span(), inner: Keyword::Proc },
    }
}

fn kw_const<'i, I>() -> impl RParser<'i, I, Keyword>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::KwConst = span => Spanned { span: span.span(), inner: Keyword::Const },
    }
}

fn kw_static<'i, I>() -> impl RParser<'i, I, Keyword>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::KwStatic = span => Spanned { span: span.span(), inner: Keyword::Static },
    }
}

fn kw_struct<'i, I>() -> impl RParser<'i, I, Keyword>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::KwStruct = span => Spanned { span: span.span(), inner: Keyword::Struct },
    }
}

fn op_and<'i, I>() -> impl RParser<'i, I, Punctuation>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::OpAnd = span => Spanned { span: span.span(), inner: Punctuation::And },
    }
}

fn op_mul<'i, I>() -> impl RParser<'i, I, Punctuation>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::OpMul = span => Spanned { span: span.span(), inner: Punctuation::Mul },
    }
}

fn op_div<'i, I>() -> impl RParser<'i, I, Punctuation>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::OpDiv = span => Spanned { span: span.span(), inner: Punctuation::Div },
    }
}

fn op_plus<'i, I>() -> impl RParser<'i, I, Punctuation>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::OpPlus = span => Spanned { span: span.span(), inner: Punctuation::Plus },
    }
}

fn op_minus<'i, I>() -> impl RParser<'i, I, Punctuation>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::OpMinus = span => Spanned { span: span.span(), inner: Punctuation::Minus },
    }
}

fn op_assign<'i, I>() -> impl RParser<'i, I, Punctuation>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::OpAssign = span => Spanned { span: span.span(), inner: Punctuation::Assign },
    }
}

fn op_eq<'i, I>() -> impl RParser<'i, I, Punctuation>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::OpEq = span => Spanned { span: span.span(), inner: Punctuation::Eq },
    }
}

fn op_dot<'i, I>() -> impl RParser<'i, I, Punctuation>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::OpDot = span => Spanned { span: span.span(), inner: Punctuation::Dot },
    }
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
    select! {
        Token::Colon = span => Spanned { span: span.span(), inner: Punctuation::Colon },
    }
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
            cond,
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

fn proc<'i: 'd, 'd, I>() -> impl RParser<'i, I, TopLevel>
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

pub fn file<'i: 'd, 'd, I>() -> impl Parser<'i, I, Module, extra::Err<Rich<'i, Token, Span>>> + 'd
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
}
