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

use super::flat_ast::{
    key, Ast, Binary, Call, Const, Custom, Expr, FieldAccess, Func, FuncDecl, FuncSignature,
    Generic, GenericParams, Generics, Let, Module, ModuleItem, NameTypePair, Operator, Primitive,
    Static, Type, Unary, Word,
};

pub trait NakedParser<'i, I, O, S: 'static>
where
    Self: Parser<'i, I, O, extra::Full<Rich<'i, Token, Span>, S, usize>> + Clone,
    I: ValueInput<'i, Token = Token, Span = Span>,
{
}
impl<'i, I, O, T, S: 'static> NakedParser<'i, I, O, S> for T
where
    T: Parser<'i, I, O, extra::Full<Rich<'i, Token, Span>, S, usize>> + Clone,
    I: ValueInput<'i, Token = Token, Span = Span>,
{
}

pub trait FlatParser<'i, I, O, S: 'static>
where
    Self: Parser<'i, I, Spanned<O>, extra::Full<Rich<'i, Token, Span>, S, usize>> + Clone,
    I: ValueInput<'i, Token = Token, Span = Span>,
{
}
impl<'i, I, O, T, S: 'static> FlatParser<'i, I, O, S> for T
where
    T: Parser<'i, I, Spanned<O>, extra::Full<Rich<'i, Token, Span>, S, usize>> + Clone,
    I: ValueInput<'i, Token = Token, Span = Span>,
{
}

fn ty<'i, I>() -> impl FlatParser<'i, I, Type, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    recursive(|ty| {
        let generic_params = lbracket()
            .ignore_then(ty.repeated().collect())
            .then_ignore(rbracket())
            .map_with(|tys, span| Spanned {
                span: span.span(),
                inner: GenericParams { tys },
            });
        just(Token::OpAnd)
            .ignored()
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

fn kw_module<'i, I>() -> impl NakedParser<'i, I, Span, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwModule).map_with(|_, extra| extra.span())
}

fn kw_use<'i, I>() -> impl NakedParser<'i, I, Span, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwUse).map_with(|_, extra| extra.span())
}

fn kw_trait<'i, I>() -> impl NakedParser<'i, I, Span, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwTrait).map_with(|_, extra| extra.span())
}

fn kw_let<'i, I>() -> impl NakedParser<'i, I, Span, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwLet).map_with(|_, extra| extra.span())
}

fn kw_lambda<'i, I>() -> impl NakedParser<'i, I, Span, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwLambda).map_with(|_, extra| extra.span())
}

fn kw_while<'i, I>() -> impl NakedParser<'i, I, Span, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwWhile).map_with(|_, extra| extra.span())
}

fn kw_for<'i, I>() -> impl NakedParser<'i, I, Span, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwFor).map_with(|_, extra| extra.span())
}

fn kw_match<'i, I>() -> impl NakedParser<'i, I, Span, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwMatch).map_with(|_, extra| extra.span())
}
fn kw_if<'i, I>() -> impl NakedParser<'i, I, Span, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwIf).map_with(|_, extra| extra.span())
}
fn kw_else<'i, I>() -> impl NakedParser<'i, I, Span, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwElse).map_with(|_, extra| extra.span())
}

fn kw_ret<'i, I>() -> impl NakedParser<'i, I, Span, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwReturn).map_with(|_, extra| extra.span())
}

fn kw_func<'i, I>() -> impl NakedParser<'i, I, Span, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwFunc).map_with(|_, extra| extra.span())
}

fn kw_const<'i, I>() -> impl NakedParser<'i, I, Span, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwConst).map_with(|_, extra| extra.span())
}

fn kw_static<'i, I>() -> impl NakedParser<'i, I, Span, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwStatic).map_with(|_, extra| extra.span())
}

fn kw_struct<'i, I>() -> impl NakedParser<'i, I, Span, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwStruct).map_with(|_, extra| extra.span())
}

fn kw_impl<'i, I>() -> impl NakedParser<'i, I, Span, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::KwImpl).map_with(|_, extra| extra.span())
}

fn op_and<'i, I>() -> impl FlatParser<'i, I, Operator, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::OpAnd).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Operator::And,
    })
}

fn op_mul<'i, I>() -> impl FlatParser<'i, I, Operator, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::OpMul).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Operator::Mul,
    })
}

fn op_div<'i, I>() -> impl FlatParser<'i, I, Operator, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::OpDiv).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Operator::Div,
    })
}

fn op_plus<'i, I>() -> impl FlatParser<'i, I, Operator, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::OpPlus).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Operator::Plus,
    })
}

fn op_minus<'i, I>() -> impl FlatParser<'i, I, Operator, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::OpMinus).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Operator::Minus,
    })
}

fn op_assign<'i, I>() -> impl FlatParser<'i, I, Operator, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::OpAssign).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Operator::Assign,
    })
}

fn op_eq<'i, I>() -> impl FlatParser<'i, I, Operator, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::OpEq).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Operator::Eq,
    })
}

fn op_dot<'i, I>() -> impl FlatParser<'i, I, Operator, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::OpDot).map_with(|_, extra| Spanned {
        span: extra.span(),
        inner: Operator::Dot,
    })
}

fn fat_arrow<'i, I>() -> impl NakedParser<'i, I, (), Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::FatArrow).ignored()
}

fn word<'i, I>() -> impl FlatParser<'i, I, Word, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::Word(w) = span => Spanned { span: span.span(), inner: Word(w) },
    }
}

fn word_expr<'i, I>() -> impl FlatParser<'i, I, key::Expr, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    {
        chumsky::primitive::select(
            |t, extra: &mut MapExtra<'i, '_, I, extra::Full<_, Ast, _>>| match t {
                Token::Word(w) => {
                    let expr = extra.span().spanned(Expr::Word(Word(w)));
                    Option::Some(extra.state().insert(expr))
                }
                _ => Option::None,
            },
        )
    }
}

fn path<'i, I>() -> impl FlatParser<'i, I, ItemPathBuf, Ast>
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

fn path_expr<'i, I>() -> impl FlatParser<'i, I, key::Expr, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select! {
        Token::Word(w) =>  w,
    }
    .separated_by(just(Token::DoubleColon))
    .at_least(1)
    .collect::<Vec<_>>()
    .map_with(
        |ws, extra: &mut MapExtra<'i, '_, I, extra::Full<_, Ast, _>>| {
            let expr = extra.span().spanned(Expr::Path(ItemPathBuf::from(ws)));
            extra.state().insert(expr)
        },
    )
}

fn separator<'i, I>() -> impl NakedParser<'i, I, (), Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::Colon).ignored()
}

fn const_<'i: 'd, 'd, I>(
    expr: impl FlatParser<'i, I, key::Expr, Ast>,
) -> impl FlatParser<'i, I, Const, Ast>
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

fn const_expr<'i, I>(
    expr: impl FlatParser<'i, I, key::Expr, Ast>,
) -> impl FlatParser<'i, I, key::Expr, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    const_(expr).map_with(|c, e| e.state().insert(c.map(Expr::Const)))
}

fn static_<'i, I>(
    expr: impl FlatParser<'i, I, key::Expr, Ast>,
) -> impl FlatParser<'i, I, Static, Ast>
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

fn static_expr<'i, I>(
    expr: impl FlatParser<'i, I, key::Expr, Ast>,
) -> impl FlatParser<'i, I, key::Expr, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    static_(expr).map_with(|c, e| e.state().insert(c.map(Expr::Static)))
}

fn expr<'i: 'd, 'd, I>() -> impl FlatParser<'i, I, key::Expr, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    recursive(|expr| {
        let let_ = kw_let()
            .then(word())
            .then(type_ascription().or_not())
            .then(op_assign())
            .then(expr.clone())
            .map_with(|((((let_, pat), ascription), assign), body), extra| {
                let expr = Spanned {
                    span: extra.span(),
                    inner: Expr::Let(Let {
                        let_,
                        pat,
                        ascription,
                        assign,
                        body,
                    }),
                };
                extra.state().insert(expr)
            });

        let call = lparen()
            .ignore_then(
                select! { Token::Indent(_) => () }
                    .or_not()
                    .ignore_then(expr.clone())
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<Spanned<key::Expr>>>(),
            )
            .then_ignore(
                select! { Token::Indent(_) => () }
                    .or_not()
                    .ignore_then(rparen()),
            );

        choice((
            static_expr(expr.clone()),
            const_expr(expr),
            path_expr(),
            word_expr(),
        ))
        .pratt((
            pratt::prefix(
                1,
                op_and::<I>(),
                |op, expr, extra: &mut MapExtra<'i, '_, I, extra::Full<_, Ast, _>>| {
                    let expr = extra.span().spanned(Expr::Ref(Unary { op, expr }));
                    extra.state().insert(expr)
                },
            ),
            pratt::infix(
                pratt::left(5),
                op_mul::<I>(),
                |left, op, right, extra: &mut MapExtra<'i, '_, I, extra::Full<_, Ast, _>>| {
                    let expr = extra.span().spanned(Expr::Mul(Binary { left, op, right }));
                    extra.state().insert(expr)
                },
            ),
            pratt::prefix(
                1,
                op_mul::<I>(),
                |op, expr, extra: &mut MapExtra<'i, '_, I, extra::Full<_, Ast, _>>| {
                    let expr = extra.span().spanned(Expr::Deref(Unary { op, expr }));
                    extra.state().insert(expr)
                },
            ),
            pratt::infix(
                pratt::left(5),
                op_div::<I>(),
                |left, op, right, extra: &mut MapExtra<'i, '_, I, extra::Full<_, Ast, _>>| {
                    let expr = extra.span().spanned(Expr::Div(Binary { left, op, right }));
                    extra.state().insert(expr)
                },
            ),
            pratt::infix(
                pratt::left(4),
                op_plus::<I>(),
                |left, op, right, extra: &mut MapExtra<'i, '_, I, extra::Full<_, Ast, _>>| {
                    let expr = extra.span().spanned(Expr::Add(Binary { left, op, right }));
                    extra.state().insert(expr)
                },
            ),
            pratt::infix(
                pratt::left(4),
                op_minus::<I>(),
                |left, op, right, extra: &mut MapExtra<'i, '_, I, extra::Full<_, Ast, _>>| {
                    let expr = extra.span().spanned(Expr::Sub(Binary { left, op, right }));
                    extra.state().insert(expr)
                },
            ),
            pratt::infix(
                pratt::left(3),
                op_eq::<I>(),
                |left, op, right, extra: &mut MapExtra<'i, '_, I, extra::Full<_, Ast, _>>| {
                    let expr = extra.span().spanned(Expr::Eq(Binary { left, op, right }));
                    extra.state().insert(expr)
                },
            ),
            pratt::infix(
                pratt::left(3),
                op_assign::<I>(),
                |left, op, right, extra: &mut MapExtra<'i, '_, I, extra::Full<_, Ast, _>>| {
                    let expr = extra
                        .span()
                        .spanned(Expr::Assign(Binary { left, op, right }));
                    extra.state().insert(expr)
                },
            ),
            pratt::postfix(
                6,
                select! { Token::Indent(_) => () }
                    .or_not()
                    .ignore_then(op_dot::<I>().ignore_then(word())),
                |reciever, field, extra: &mut MapExtra<'i, '_, I, extra::Full<_, Ast, _>>| {
                    let expr = extra
                        .span()
                        .spanned(Expr::FieldAccess(FieldAccess { reciever, field }));
                    extra.state().insert(expr)
                },
            ),
            pratt::postfix(
                6,
                call,
                |callee, args, extra: &mut MapExtra<'i, '_, I, extra::Full<_, Ast, _>>| {
                    let expr = extra.span().spanned(Expr::Call(Call { callee, args }));
                    extra.state().insert(expr)
                },
            ),
        ))
    })
    .labelled("Expr")
}

fn indent_increase<'i, I>() -> impl NakedParser<'i, I, usize, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select(|x, e| match x {
        Token::Indent(level) if level > *e.ctx() => Some(level),
        _ => None,
    })
    .labelled("indent increase")
}

fn indent_same<'i, I>() -> impl NakedParser<'i, I, (), Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    select(|x, e| match x {
        Token::Indent(level) if level == *e.ctx() => Some(()),
        _ => None,
    })
    .labelled("same indent")
}
fn func_signature<'i, I>() -> impl NakedParser<'i, I, FuncSignature, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    name_type_pair()
        .separated_by(just(Token::Comma))
        .collect::<Vec<_>>()
        .then(fat_arrow().ignore_then(ty()).or_not())
        .map(|(ins, return_)| FuncSignature { ins, return_ })
}

fn lbracket<'i, I>() -> impl NakedParser<'i, I, (), Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::LBracket).ignored()
}
fn rbracket<'i, I>() -> impl NakedParser<'i, I, (), Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::RBracket).ignored()
}

fn lparen<'i, I>() -> impl NakedParser<'i, I, (), Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::LParen).ignored()
}
fn rparen<'i, I>() -> impl NakedParser<'i, I, (), Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    just(Token::RParen).ignored()
}

fn generics<'i, I>() -> impl NakedParser<'i, I, Generics, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    lbracket()
        .ignore_then(
            word()
                .then(
                    separator()
                        .ignore_then(path().separated_by(just(Token::OpPlus)).collect())
                        .or_not(),
                )
                .map(|(ty, constraints)| Generic {
                    ty,
                    constraints: constraints.unwrap_or_default(),
                })
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>(),
        )
        .then_ignore(rbracket())
        .map(|tys| Generics { tys })
}

fn body<'i: 'd, 'd, I, O>(
    expr: impl NakedParser<'i, I, Spanned<O>, Ast>,
) -> impl NakedParser<'i, I, Vec<Spanned<O>>, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    indent_increase().ignore_with_ctx(expr.clone().separated_by(indent_same()).collect::<Vec<_>>())
}

fn body_unspanned<'i: 'd, 'd, I, O>(
    expr: impl NakedParser<'i, I, O, Ast>,
) -> impl NakedParser<'i, I, Vec<O>, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    indent_increase().ignore_with_ctx(expr.clone().separated_by(indent_same()).collect::<Vec<_>>())
}

fn func<'i: 'd, 'd, I>() -> impl FlatParser<'i, I, key::Func, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    func_decl().then(body(expr())).map_with(
        |(func_decl, body), extra: &mut MapExtra<'i, '_, I, extra::Full<_, Ast, _>>| {
            let func = extra.span().spanned(Func { func_decl, body });
            extra.state().insert(func)
        },
    )
}

fn func_decl<'i: 'd, 'd, I>() -> impl NakedParser<'i, I, FuncDecl, Ast>
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

fn type_ascription<'i, I>() -> impl FlatParser<'i, I, Type, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    separator().ignore_then(ty())
}

fn name_type_pair<'i, I>() -> impl NakedParser<'i, I, NameTypePair, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    word()
        .then(type_ascription())
        .map(|(name, ty)| NameTypePair { name, ty })
}

pub fn file<'i, I>() -> impl FlatParser<'i, I, key::Module, Ast>
where
    I: ValueInput<'i, Token = Token, Span = Span>,
{
    choice((func().map(|f| f.map(ModuleItem::Func)),))
        .padded_by(select!(Token::Indent(_) => ()).or_not())
        .repeated()
        .collect()
        .then_ignore(end())
        .or(end().to(vec![]))
        .map_with(|items, extra: &mut MapExtra<'i, '_, I, _>| {
            let module = extra.span().spanned(Module { items });
            extra.state().insert(module)
        })
}
