use std::path::PathBuf;

use chumsky::{
    input::{MapExtra, ValueInput},
    pratt,
    prelude::*,
    primitive::select,
};
use internment::Intern;
use itempath::ItemPathBuf;
use logos::Logos;
use spanner::{Span, Spanned};

use crate::lexer::Token;

use super::{
    ast::{
        key, Ast, Binary, Call, Const, Constraint, Else, Expr, FieldAccess, FieldValuePair, Func,
        FuncDecl, FuncSignature, Generic, GenericParams, Generics, If, ImplLevel, InherentImpl,
        Keyword, Lambda, LambdaArg, Let, Literal, Match, MatchBranch, Module, ModuleDef,
        NameTypePair, Punctuation, Return, SignatureReturn, Static, Struct, StructLiteral,
        TopLevel, Trait, TraitImpl, TypeAscription, Unary, Use, While, Word,
    },
    types::{Custom, Primitive, Type},
};

pub trait NParser<'i, I, O, S: 'static>
where
    Self: Parser<'i, I, O, extra::Full<Rich<'i, Token, Span>, S, usize>> + Clone,
    I: ValueInput<'i, Token = Token, Span = Span>,
{
}
impl<'i, I, O, T, S: 'static> NParser<'i, I, O, S> for T
where
    T: Parser<'i, I, O, extra::Full<Rich<'i, Token, Span>, S, usize>> + Clone,
    I: ValueInput<'i, Token = Token, Span = Span>,
{
}

trait SParser<'i, I, O, S: 'static>
where
    Self: Parser<'i, I, Spanned<O>, extra::Full<Rich<'i, Token, Span>, S, usize>> + Clone,
    I: ValueInput<'i, Token = Token, Span = Span>,
{
}
impl<'i, I, O, T, S: 'static> SParser<'i, I, O, S> for T
where
    T: Parser<'i, I, Spanned<O>, extra::Full<Rich<'i, Token, Span>, S, usize>> + Clone,
    I: ValueInput<'i, Token = Token, Span = Span>,
{
}

fn ty<'i, I>() -> impl SParser<'i, I, Type, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    recursive(|ty| {
        let generic_params = lbracket()
            .then(ty.repeated().collect())
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
            .to(())
            .repeated()
            .collect::<Vec<()>>()
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

fn literal<'i, I>(expr: impl SParser<'i, I, Expr, Ast>) -> impl SParser<'i, I, Literal, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    choice((
        path()
            .then(
                lparen()
                    .ignore_then(
                        select! { Token::Indent(_) => () }
                            .or_not()
                            .ignore_then(field_value_pair(expr.clone()))
                            .separated_by(just(Token::Comma))
                            .allow_trailing()
                            .collect::<Vec<_>>(),
                    )
                    .then_ignore(
                        select! { Token::Indent(_) => () }
                            .or_not()
                            .ignore_then(rparen()),
                    ),
            )
            .map_with(|(ty, fields), extra| Spanned {
                span: extra.span(),
                inner: Literal::Struct(StructLiteral { ty, fields }),
            }),
        lparen()
            .ignore_then(
                select! { Token::Indent(_) => () }
                    .or_not()
                    .ignore_then(expr.clone())
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>(),
            )
            .then_ignore(
                select! { Token::Indent(_) => () }
                    .or_not()
                    .ignore_then(rparen()),
            )
            .map_with(|exprs, s| Spanned {
                span: s.span(),
                inner: Literal::Tuple(exprs),
            }),
        lbracket()
            .ignore_then(
                select! { Token::Indent(_) => () }
                    .or_not()
                    .ignore_then(expr)
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>(),
            )
            .then_ignore(
                select! { Token::Indent(_) => () }
                    .or_not()
                    .ignore_then(rbracket()),
            )
            .map_with(|exprs, s| Spanned {
                span: s.span(),
                inner: Literal::Array(exprs),
            }),
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
        },
    ))
    .labelled("Literal")
}

fn literal_expr<'i, I>(expr: impl SParser<'i, I, Expr, Ast>) -> impl SParser<'i, I, Expr, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    literal(expr).map(|Spanned { span, inner }| Spanned {
        span,
        inner: Expr::Literal(inner),
    })
}

fn kw_module<'i, I>() -> impl SParser<'i, I, Keyword, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwModule).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Keyword::Module,
    })
}

fn kw_use<'i, I>() -> impl SParser<'i, I, Keyword, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwUse).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Keyword::Use,
    })
}

fn kw_trait<'i, I>() -> impl SParser<'i, I, Keyword, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwTrait).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Keyword::Trait,
    })
}

fn kw_let<'i, I>() -> impl SParser<'i, I, Keyword, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwLet).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Keyword::Let,
    })
}

fn kw_lambda<'i, I>() -> impl SParser<'i, I, Keyword, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwLambda).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Keyword::Lambda,
    })
}

fn kw_while<'i, I>() -> impl SParser<'i, I, Keyword, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwWhile).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Keyword::While,
    })
}

fn kw_for<'i, I>() -> impl SParser<'i, I, Keyword, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwFor).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Keyword::For,
    })
}

fn kw_match<'i, I>() -> impl SParser<'i, I, Keyword, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwMatch).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Keyword::Match,
    })
}
fn kw_if<'i, I>() -> impl SParser<'i, I, Keyword, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwIf).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Keyword::If,
    })
}
fn kw_else<'i, I>() -> impl SParser<'i, I, Keyword, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwElse).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Keyword::Else,
    })
}

fn kw_ret<'i, I>() -> impl SParser<'i, I, Keyword, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwReturn).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Keyword::Return,
    })
}

fn kw_func<'i, I>() -> impl SParser<'i, I, Keyword, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwFunc).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Keyword::Func,
    })
}

fn kw_const<'i, I>() -> impl SParser<'i, I, Keyword, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwConst).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Keyword::Const,
    })
}

fn kw_static<'i, I>() -> impl SParser<'i, I, Keyword, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwStatic).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Keyword::Static,
    })
}

fn kw_struct<'i, I>() -> impl SParser<'i, I, Keyword, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwStruct).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Keyword::Struct,
    })
}

fn kw_impl<'i, I>() -> impl SParser<'i, I, Keyword, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwImpl).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Keyword::Impl,
    })
}

fn op_and<'i, I>() -> impl SParser<'i, I, Punctuation, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::OpAnd).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Punctuation::And,
    })
}

fn op_mul<'i, I>() -> impl SParser<'i, I, Punctuation, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::OpMul).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Punctuation::Mul,
    })
}

fn op_div<'i, I>() -> impl SParser<'i, I, Punctuation, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::OpDiv).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Punctuation::Div,
    })
}

fn op_plus<'i, I>() -> impl SParser<'i, I, Punctuation, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::OpPlus).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Punctuation::Plus,
    })
}

fn op_minus<'i, I>() -> impl SParser<'i, I, Punctuation, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::OpMinus).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Punctuation::Minus,
    })
}

fn op_assign<'i, I>() -> impl SParser<'i, I, Punctuation, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::OpAssign).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Punctuation::Assign,
    })
}

fn op_eq<'i, I>() -> impl SParser<'i, I, Punctuation, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::OpEq).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Punctuation::Eq,
    })
}

fn op_dot<'i, I>() -> impl SParser<'i, I, Punctuation, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::OpDot).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Punctuation::Dot,
    })
}

fn fat_arrow<'i, I>() -> impl SParser<'i, I, Punctuation, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::FatArrow).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Punctuation::FatArrow,
    })
}

fn word<'i, I>() -> impl SParser<'i, I, Word, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::Word(w) = span => Spanned { span: span.span(), inner: Word(w) },
    }
}

fn word_expr<'i, I>() -> impl SParser<'i, I, Expr, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::Word(w) = span => Spanned { span: span.span(), inner: Expr::Word(Word(w)) },
    }
}

fn path<'i, I>() -> impl SParser<'i, I, ItemPathBuf, Ast>
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

fn path_expr<'i, I>() -> impl SParser<'i, I, Expr, Ast>
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

fn separator<'i, I>() -> impl SParser<'i, I, Punctuation, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::Colon).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Punctuation::Colon,
    })
}

fn const_<'i: 'd, 'd, I>(expr: impl SParser<'i, I, Expr, Ast>) -> impl SParser<'i, I, Const, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    kw_const()
        .then(word())
        .then(type_ascription())
        .then(op_assign())
        .then(expr)
        .map_with(
            |((((const_, name), signature), assign), expr), span| Spanned {
                span: span.span(),
                inner: Const {
                    const_,
                    name,
                    signature,
                    assign,
                    expr,
                },
            },
        )
}

fn const_toplevel<'i, I>() -> impl SParser<'i, I, TopLevel, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    const_(expr()).map(|Spanned { span, inner }| Spanned {
        span,
        inner: TopLevel::Const(inner),
    })
}

fn const_expr<'i, I>(expr: impl SParser<'i, I, Expr, Ast>) -> impl SParser<'i, I, Expr, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    const_(expr).map(|Spanned { span, inner }| Spanned {
        span,
        inner: Expr::Const(Box::new(inner)),
    })
}

fn static_<'i, I>(expr: impl SParser<'i, I, Expr, Ast>) -> impl SParser<'i, I, Static, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    kw_static()
        .then(word())
        .then(type_ascription())
        .then(op_assign())
        .then(expr)
        .map_with(
            |((((static_, name), signature), assign), expr), span| Spanned {
                span: span.span(),
                inner: Static {
                    static_,
                    name,
                    signature,
                    assign,
                    expr,
                },
            },
        )
}

fn static_toplevel<'i, I>() -> impl SParser<'i, I, TopLevel, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    static_(expr()).map(|Spanned { span, inner }| Spanned {
        span,
        inner: TopLevel::Static(inner),
    })
}

fn static_expr<'i, I>(expr: impl SParser<'i, I, Expr, Ast>) -> impl SParser<'i, I, Expr, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    static_(expr).map(|Spanned { span, inner }| Spanned {
        span,
        inner: Expr::Static(Box::new(inner)),
    })
}

fn expr<'i: 'd, 'd, I>() -> impl SParser<'i, I, Expr, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    recursive(|expr| {
        let while_ = kw_while()
            .then(expr.clone())
            .then(body(expr.clone()))
            .map_with(|((while_, cond), body), span| Spanned {
                span: span.span(),
                inner: Expr::While(Box::new(While { while_, cond, body })),
            });

        let lie = indent_same()
            .ignore_then(kw_else())
            .then(body(expr.clone()))
            .map(|(else_, body)| Else { else_, body });

        let if_ = kw_if()
            .then(expr.clone())
            .then(body(expr.clone()))
            .then(lie.or_not())
            .map_with(|(((if_, cond), truth), lie), extra| Spanned {
                span: extra.span(),
                inner: Expr::If(Box::new(If {
                    if_,
                    cond,
                    truth,
                    lie,
                })),
            });

        let pat = choice((literal_expr(expr.clone()), word_expr(), path_expr()));
        let match_branch = pat
            .clone()
            .then(fat_arrow())
            .then(choice((expr.clone().map(|e| vec![e]), body(expr.clone()))))
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
            .then(body(match_branch))
            .map_with(|((match_, expr), branches), extra| Spanned {
                span: extra.span(),
                inner: Expr::Cond(Box::new(Match {
                    match_,
                    expr,
                    branches,
                })),
            });

        let ret_expr = kw_ret()
            .then(expr.clone().or_not())
            .map_with(|(return_, expr), extra| Spanned {
                span: extra.span(),
                inner: Expr::Return(Box::new(Return { return_, expr })),
            });

        let let_ = kw_let()
            .then(word())
            .then(type_ascription().or_not())
            .then(op_assign())
            .then(expr.clone())
            .map_with(
                |((((let_, pat), ascription), assign), body), extra| Spanned {
                    span: extra.span(),
                    inner: Expr::Let(Box::new(Let {
                        let_,
                        pat,
                        ascription,
                        assign,
                        body,
                    })),
                },
            );

        let lambda = kw_lambda()
            .then(
                word()
                    .then(type_ascription().or_not())
                    .map(|(name, ascription)| LambdaArg { name, ascription })
                    .separated_by(just(Token::Comma))
                    .collect::<Vec<_>>(),
            )
            .then(choice((expr.clone().map(|e| vec![e]), body(expr.clone()))))
            .map_with(|((lambda, arglist), body), extra| Spanned {
                span: extra.span(),
                inner: Expr::Lambda(Lambda {
                    lambda,
                    arglist,
                    body,
                }),
            });

        let call = lparen()
            .then(
                select! { Token::Indent(_) => () }
                    .or_not()
                    .ignore_then(expr.clone())
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>(),
            )
            .then(
                select! { Token::Indent(_) => () }
                    .or_not()
                    .ignore_then(rparen()),
            );

        choice((
            literal_expr(expr.clone()),
            static_expr(expr.clone()),
            const_expr(expr),
            path_expr(),
            word_expr(),
            let_,
            lambda,
            while_,
            if_,
            match_,
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
            pratt::postfix(
                6,
                select! { Token::Indent(_) => () }
                    .or_not()
                    .ignore_then(op_dot::<I>().then(word())),
                |expr, (op, field), span| Spanned {
                    span,
                    inner: Expr::FieldAccess(Box::new(FieldAccess {
                        reciever: expr,
                        access: op,
                        field,
                    })),
                },
            ),
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

fn indent_increase<'i, I>() -> impl NParser<'i, I, usize, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select(|x, e| match x {
        Token::Indent(level) if level > *e.ctx() => Some(level),
        _ => None,
    })
    .labelled("indent increase")
}

fn indent_same<'i, I>() -> impl NParser<'i, I, (), Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select(|x, e| match x {
        Token::Indent(level) if level == *e.ctx() => Some(()),
        _ => None,
    })
    .labelled("same indent")
}
fn func_signature<'i, I>() -> impl NParser<'i, I, FuncSignature, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    name_type_pair()
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>()
        .then(
            fat_arrow()
                .then(ty())
                .map(|(arrow, ty)| SignatureReturn { arrow, ty })
                .or_not(),
        )
        .map(|(ins, return_)| FuncSignature { ins, return_ })
}

fn lbracket<'i, I>() -> impl SParser<'i, I, Punctuation, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::LBracket).map_with(|_, span| Spanned {
        span: span.span(),
        inner: Punctuation::LBracket,
    })
}
fn rbracket<'i, I>() -> impl SParser<'i, I, Punctuation, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::RBracket).map_with(|_, span| Spanned {
        span: span.span(),
        inner: Punctuation::RBracket,
    })
}

fn lparen<'i, I>() -> impl SParser<'i, I, Punctuation, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::LParen).map_with(|_, span| Spanned {
        span: span.span(),
        inner: Punctuation::RParen,
    })
}
fn rparen<'i, I>() -> impl SParser<'i, I, Punctuation, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::RParen).map_with(|_, span| Spanned {
        span: span.span(),
        inner: Punctuation::RParen,
    })
}

fn generics<'i, I>() -> impl NParser<'i, I, Generics, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    lbracket()
        .then(
            word()
                .then(
                    separator()
                        .then(path().separated_by(just(Token::OpPlus)).collect())
                        .map(|(sep, constraint)| Constraint { sep, constraint })
                        .or_not(),
                )
                .map(|(ty, constraint)| Generic { ty, constraint })
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>(),
        )
        .then(rbracket())
        .map(|((left_bracket, tys), right_bracket)| Generics {
            left_bracket,
            tys,
            right_bracket,
        })
}

fn body<'i: 'd, 'd, I, O>(
    expr: impl NParser<'i, I, Spanned<O>, Ast>,
) -> impl NParser<'i, I, Vec<Spanned<O>>, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    indent_increase().ignore_with_ctx(expr.clone().separated_by(indent_same()).collect::<Vec<_>>())
}

fn body_unspanned<'i: 'd, 'd, I, O>(
    expr: impl NParser<'i, I, O, Ast>,
) -> impl NParser<'i, I, Vec<O>, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    indent_increase().ignore_with_ctx(expr.clone().separated_by(indent_same()).collect::<Vec<_>>())
}

fn func<'i: 'd, 'd, I>() -> impl SParser<'i, I, Func, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    func_decl()
        .then(body(expr()))
        .map_with(|(func_decl, body), span| Spanned {
            span: span.span(),
            inner: Func { func_decl, body },
        })
}

fn func_decl<'i: 'd, 'd, I>() -> impl NParser<'i, I, FuncDecl, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    kw_func()
        .then(generics().or_not())
        .then(word())
        .then(func_signature())
        .map(|(((func, generics), name), signature)| FuncDecl {
            func,
            generics,
            name,
            signature,
        })
}

fn type_ascription<'i, I>() -> impl NParser<'i, I, TypeAscription, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    separator()
        .then(ty())
        .map(|(sep, ty)| TypeAscription { sep, ty })
}

fn name_type_pair<'i, I>() -> impl NParser<'i, I, NameTypePair, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    word()
        .then(type_ascription())
        .map(|(name, TypeAscription { sep, ty })| NameTypePair { name, sep, ty })
}

fn field_value_pair<'i, I>(
    expr: impl SParser<'i, I, Expr, Ast>,
) -> impl NParser<'i, I, FieldValuePair, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    word()
        .then(separator())
        .then(expr)
        .map(|((name, sep), expr)| FieldValuePair { name, sep, expr })
}

fn struct_<'i, I>() -> impl SParser<'i, I, TopLevel, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    kw_struct()
        .then(generics().or_not())
        .then(word())
        .then(body_unspanned(name_type_pair()))
        .map_with(|(((struct_, generics), name), body), span| Spanned {
            span: span.span(),
            inner: TopLevel::Struct(Struct {
                struct_,
                generics,
                name,
                body,
            }),
        })
}

fn trait_<'i, I>() -> impl SParser<'i, I, TopLevel, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    kw_trait()
        .then(generics().or_not())
        .then(word())
        .then(body(func_decl().map_with(
            |f, e: &mut MapExtra<'_, '_, I, _>| e.span().spanned(f),
        )))
        .map_with(|(((trait_, generics), name), body), span| Spanned {
            span: span.span(),
            inner: TopLevel::Trait(Trait {
                trait_,
                generics,
                name,
                body,
            }),
        })
}

fn inherent_impl<'i, I>() -> impl SParser<'i, I, TopLevel, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    kw_impl()
        .then(generics().or_not())
        .then(ty())
        .then(body_unspanned(func().map(|f| f.map(ImplLevel::Func))))
        .map_with(|(((impl_, generics), ty), body), span| Spanned {
            span: span.span(),
            inner: TopLevel::InherentImpl(InherentImpl {
                impl_,
                generics,
                ty,
                body,
            }),
        })
}

fn trait_impl<'i, I>() -> impl SParser<'i, I, TopLevel, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    kw_impl()
        .then(generics().or_not())
        .then(path())
        .then(kw_for())
        .then(ty())
        .then(body_unspanned(func().map(|f| f.map(ImplLevel::Func))))
        .map_with(
            |(((((impl_, generics), trait_), for_), ty), body), span| Spanned {
                span: span.span(),
                inner: TopLevel::TraitImpl(TraitImpl {
                    impl_,
                    generics,
                    trait_,
                    for_,
                    ty,
                    body,
                }),
            },
        )
}

fn module<'i, I>() -> impl SParser<'i, I, TopLevel, Ast>
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

fn use_<'i, I>() -> impl SParser<'i, I, TopLevel, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    kw_use()
        .then(path())
        .map_with(|(use_, path), span| Spanned {
            span: span.span(),
            inner: TopLevel::Use(Use { use_, path }),
        })
}

pub fn file<'i, I>() -> impl NParser<'i, I, Module, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    choice((
        module(),
        func().map(|f| f.map(TopLevel::Func)),
        const_toplevel(),
        static_toplevel(),
        struct_(),
        inherent_impl(),
        trait_(),
        trait_impl(),
        use_(),
    ))
    .padded_by(select!(Token::Indent(_) => ()).or_not())
    .repeated()
    .collect()
    .then_ignore(end())
    .or(end().to(vec![]))
    .map_with(|items, extra| {
        let mut module = Module {
            funcs: Vec::new(),
            consts: Vec::new(),
            statics: Vec::new(),
            structs: Vec::new(),
            inherent_impls: Vec::new(),
            traits: Vec::new(),
            trait_impls: Vec::new(),
            modules: Vec::new(),
            uses: Vec::new(),
        };

        for item in items {
            match item.inner {
                TopLevel::Func(p) => module.funcs.push(Spanned {
                    span: item.span,
                    inner: p,
                }),
                TopLevel::Const(c) => module.consts.push(Spanned {
                    span: item.span,
                    inner: c,
                }),
                TopLevel::Static(v) => module.statics.push(Spanned {
                    span: item.span,
                    inner: v,
                }),
                TopLevel::Struct(s) => module.structs.push(Spanned {
                    span: item.span,
                    inner: s,
                }),
                TopLevel::InherentImpl(i) => module.inherent_impls.push(Spanned {
                    span: item.span,
                    inner: i,
                }),
                TopLevel::Trait(i) => module.traits.push(Spanned {
                    span: item.span,
                    inner: i,
                }),
                TopLevel::TraitImpl(i) => module.trait_impls.push(Spanned {
                    span: item.span,
                    inner: i,
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
