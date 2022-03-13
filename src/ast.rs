#[cfg(test)]
mod test;

use std::{
    collections::{hash_map::Entry, HashMap},
    path::{Path, PathBuf},
};

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

#[derive(Debug, Clone)]
pub enum TopLevel {
    Proc(Proc),
    Const(Const),
    Mem(Mem),
    Include(Include),
}

impl TopLevel {
    pub fn name(&self) -> Option<String> {
        let name_node = match self {
            TopLevel::Proc(i) => &i.name,
            TopLevel::Const(i) => &i.name,
            TopLevel::Mem(i) => &i.name,
            TopLevel::Include(_) => return None,
        };
        match &name_node.ast {
            AstKind::Word(n) => n.clone().some(),
            _ => unreachable!(),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            TopLevel::Proc(i) => &i.name,
            TopLevel::Const(i) => &i.name,
            TopLevel::Mem(i) => &i.name,
            TopLevel::Include(i) => &i.include,
        }
        .span
        .clone()
    }
}

#[derive(Debug, Clone)]
pub struct Proc {
    pub proc: AstNode,
    pub name: AstNode,
    pub signature: AstNode,
    pub do_: AstNode,
    pub body: AstNode,
    pub end: AstNode,
}

#[derive(Debug, Clone)]
pub struct Mem {
    pub mem: AstNode,
    pub name: AstNode,
    pub do_: AstNode,
    pub body: AstNode,
    pub end: AstNode,
}

#[derive(Debug, Clone)]
pub struct Const {
    pub const_: AstNode,
    pub name: AstNode,
    pub signature: AstNode,
    pub do_: AstNode,
    pub body: AstNode,
    pub end: AstNode,
}

#[derive(Debug, Clone)]
pub struct Include {
    pub include: AstNode,
    pub path: AstNode,
}

impl Include {
    pub fn path(&self) -> &Path {
        match &self.path.ast {
            AstKind::Path(p) => p,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AstNode {
    pub span: Span,
    pub ast: AstKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AstKind {
    KeyWord(KeyWord),
    Type(Type),
    Separator,

    Bind(Bind),
    Binding(Binding),

    While(While),

    If(If),
    Cond(Cond),

    Cast(Cast),

    Word(String),
    Path(PathBuf),
    Literal(IConst),
    Pattern(Box<AstNode>),

    ProcSignature(ProcSignature),
    ConstSignature(ConstSignature),

    Body(Vec<AstNode>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstSignature {
    pub sep: Box<AstNode>,
    pub tys: Vec<AstNode>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ProcSignature {
    pub ins: Vec<AstNode>,
    pub sep: Option<Box<AstNode>>,
    pub outs: Option<Vec<AstNode>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct While {
    pub while_: Box<AstNode>,
    pub cond: Box<AstNode>,
    pub do_: Box<AstNode>,
    pub body: Box<AstNode>,
    pub end: Box<AstNode>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Cast {
    pub cast: Box<AstNode>,
    pub ty: Box<AstNode>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct If {
    pub if_: Box<AstNode>,
    pub truth: Box<AstNode>,
    pub lie: Option<Else>,
    pub end: Box<AstNode>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Else {
    pub else_: Box<AstNode>,
    pub body: Box<AstNode>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Cond {
    pub cond: Box<AstNode>,
    pub pat: Box<AstNode>,
    pub do_: Box<AstNode>,
    pub body: Box<AstNode>,
    pub branches: Vec<CondBranch>,
    pub end: Box<AstNode>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CondBranch {
    pub else_: Box<AstNode>,
    pub pat: Box<AstNode>,
    pub do_: Box<AstNode>,
    pub body: Box<AstNode>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Bind {
    pub bind: Box<AstNode>,
    pub bindings: Vec<AstNode>,
    pub do_: Box<AstNode>,
    pub body: Box<AstNode>,
    pub end: Box<AstNode>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Binding {
    Ignore,
    Bind {
        name: Box<AstNode>,
        sep: Box<AstNode>,
        ty: Box<AstNode>,
    },
}

fn ty() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
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
    choice((value_type, ptr_type)).map_with_span(|ty, span| AstNode {
        span,
        ast: AstKind::Type(ty),
    })
}
fn literal() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! { span,
        Token::Bool(b) => AstNode { span, ast: AstKind::Literal(IConst::Bool(b)) },
        Token::Num(n) => AstNode { span, ast: AstKind::Literal(IConst::U64(n.parse().unwrap())) },
        Token::Str(s) => AstNode { span, ast: AstKind::Literal(IConst::Str(s)) },
        Token::Char(c) => AstNode { span, ast: AstKind::Literal(IConst::Char(c)) },
    }
}
fn include_path() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! { span,
        Token::Str(s) => AstNode { span, ast: AstKind::Path(PathBuf::from(s)) },
    }
}
fn kw_include() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! { span,
        Token::KeyWord(kw @ KeyWord::Include) => AstNode { span, ast: AstKind::KeyWord(kw) },
    }
}
fn kw_bind() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! { span,
        Token::KeyWord(kw @ KeyWord::Bind) => AstNode { span, ast: AstKind::KeyWord(kw) },
    }
}
fn kw_while() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! { span,
        Token::KeyWord(kw @ KeyWord::While) => AstNode { span, ast: AstKind::KeyWord(kw) },
    }
}
fn kw_cond() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! { span,
        Token::KeyWord(kw @ KeyWord::Cond) => AstNode { span, ast: AstKind::KeyWord(kw) },
    }
}
fn kw_if() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! { span,
        Token::KeyWord(kw @ KeyWord::If) => AstNode { span, ast: AstKind::KeyWord(kw) },
    }
}
fn kw_else() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! { span,
        Token::KeyWord(kw @ KeyWord::Else) => AstNode { span, ast: AstKind::KeyWord(kw) },
    }
}
fn kw_do() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! { span,
        Token::KeyWord(kw @ KeyWord::Do) => AstNode { span, ast: AstKind::KeyWord(kw) },
    }
}
fn kw_end() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! { span,
        Token::KeyWord(kw @ KeyWord::End) => AstNode { span, ast: AstKind::KeyWord(kw) },
    }
}
fn kw_ret() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! { span,
        Token::KeyWord(kw @ KeyWord::Return) => AstNode { span, ast: AstKind::KeyWord(kw) },
    }
}
fn kw_cast() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! { span,
        Token::KeyWord(kw @ KeyWord::Cast) => AstNode { span, ast: AstKind::KeyWord(kw) },
    }
}
fn kw_proc() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! { span,
        Token::KeyWord(kw @ KeyWord::Proc) => AstNode { span, ast: AstKind::KeyWord(kw) },
    }
}
fn kw_const() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! { span,
        Token::KeyWord(kw @ KeyWord::Const) => AstNode { span, ast: AstKind::KeyWord(kw) },
    }
}
fn kw_mem() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! { span,
        Token::KeyWord(kw @ KeyWord::Mem) => AstNode { span, ast: AstKind::KeyWord(kw) },
    }
}

fn word() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! { span,
        Token::Word(w) => AstNode { span, ast: AstKind::Word(w) },
    }
}
fn separator() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! { span,
        Token::SigSep => AstNode { span, ast: AstKind::Separator },
    }
}
fn ignore() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! { span,
        Token::Ignore => AstNode { span, ast: AstKind::Binding(Binding::Ignore) },
    }
}
fn binding() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    let name_type = word()
        .then(separator())
        .then(ty())
        .map_with_span(|((name, sep), ty), span| AstNode {
            span,
            ast: AstKind::Binding(Binding::Bind {
                name: name.boxed(),
                sep: sep.boxed(),
                ty: ty.boxed(),
            }),
        });

    choice((name_type, ignore()))
}

fn body() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> + Clone {
    recursive(|body: Recursive<'_, Token, AstNode, _>| {
        let bind = kw_bind()
            .then(binding().repeated().at_least(1))
            .then(kw_do())
            .then(body.clone())
            .then(kw_end())
            .map_with_span(|((((bind, bindings), do_), body), end), span| AstNode {
                ast: AstKind::Bind(Bind {
                    bind: bind.boxed(),
                    bindings,
                    do_: do_.boxed(),
                    body: body.boxed(),
                    end: end.boxed(),
                }),
                span,
            });

        let while_ = kw_while()
            .then(body.clone())
            .then(kw_do())
            .then(body.clone())
            .then(kw_end())
            .map_with_span(|((((while_, cond), do_), body), end), span| AstNode {
                ast: AstKind::While(While {
                    while_: box while_,
                    cond: box cond,
                    do_: box do_,
                    body: box body,
                    end: box end,
                }),
                span,
            });

        let lie = kw_else().then(body.clone()).map(|(else_, body)| Else {
            else_: box else_,
            body: box body,
        });
        let if_ = kw_if()
            .then(body.clone())
            .then(lie.or_not())
            .then(kw_end())
            .map_with_span(|(((if_, truth), lie), end), span| AstNode {
                span,
                ast: AstKind::If(If {
                    if_: box if_,
                    truth: box truth,
                    lie,
                    end: box end,
                }),
            });

        let cast = kw_cast()
            .then(ty())
            .map_with_span(|(cast, ty), span| AstNode {
                span,
                ast: AstKind::Cast(Cast {
                    cast: box cast,
                    ty: box ty,
                }),
            });

        let pat = choice((literal(), ignore(), word()));
        let cond_branch = kw_else().then(pat).then(kw_do()).then(body.clone()).map(
            |(((else_, pat), do_), body)| CondBranch {
                else_: box else_,
                pat: box pat,
                do_: box do_,
                body: box body,
            },
        );
        let pat = choice((literal(), ignore(), word()));
        let cond = kw_cond()
            .then(pat)
            .then(kw_do())
            .then(body.clone())
            .then(cond_branch.repeated())
            .then(kw_end())
            .map_with_span(
                |(((((cond, pat), do_), body), branches), end), span| AstNode {
                    span,
                    ast: AstKind::Cond(Cond {
                        cond: box cond,
                        pat: box pat,
                        do_: box do_,
                        body: box body,
                        branches,
                        end: box end,
                    }),
                },
            );

        choice((literal(), word(), bind, while_, if_, cond, cast, kw_ret()))
            .repeated()
            .map_with_span(|body, span| AstNode {
                span,
                ast: AstKind::Body(body),
            })
    })
}

fn proc_signature() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    ty().repeated()
        .then(separator().then(ty().repeated().at_least(1)).or_not())
        .map_with_span(|(ins, maybe_outs), span| {
            let (sep, outs) = if let Some((sep, outs)) = maybe_outs {
                (Some(box sep), Some(outs))
            } else {
                (None, None)
            };
            AstNode {
                span,
                ast: AstKind::ProcSignature(ProcSignature { ins, sep, outs }),
            }
        })
}

fn const_signature() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    separator()
        .then(ty().repeated().at_least(1))
        .map_with_span(|(sep, tys), span| AstNode {
            span,
            ast: AstKind::ConstSignature(ConstSignature { sep: box sep, tys }),
        })
}

fn proc() -> impl Parser<Token, TopLevel, Error = Simple<Token, Span>> {
    kw_proc()
        .then(word())
        .then(proc_signature())
        .then(kw_do())
        .then(body())
        .then(kw_end())
        .map(|(((((proc, name), signature), do_), body), end)| {
            TopLevel::Proc(Proc {
                proc,
                name,
                signature,
                do_,
                body,
                end,
            })
        })
}

fn const_() -> impl Parser<Token, TopLevel, Error = Simple<Token, Span>> {
    kw_const()
        .then(word())
        .then(const_signature())
        .then(kw_do())
        .then(body())
        .then(kw_end())
        .map(|(((((const_, name), signature), do_), body), end)| {
            TopLevel::Const(Const {
                const_,
                name,
                signature,
                do_,
                body,
                end,
            })
        })
}

fn mem() -> impl Parser<Token, TopLevel, Error = Simple<Token, Span>> {
    kw_mem()
        .then(word())
        .then(kw_do())
        .then(body())
        .then(kw_end())
        .map(|((((mem, name), do_), body), end)| {
            TopLevel::Mem(Mem {
                mem,
                name,
                do_,
                body,
                end,
            })
        })
}

fn include() -> impl Parser<Token, TopLevel, Error = Simple<Token, Span>> {
    kw_include()
        .then(include_path())
        .map(|(include, path)| TopLevel::Include(Include { include, path }))
}

fn toplevel() -> impl Parser<Token, Vec<TopLevel>, Error = Simple<Token, Span>> {
    choice((include(), proc(), const_(), mem()))
        .repeated()
        .then_ignore(end())
}

pub fn parse_no_include(tokens: Vec<(Token, Span)>) -> Result<Vec<TopLevel>, Error> {
    toplevel()
        .parse(Stream::from_iter(
            tokens.last().unwrap().1.clone(),
            tokens.into_iter(),
        ))
        .map_err(Error::Parser)
}

pub fn parse(tokens: Vec<(Token, Span)>) -> Result<HashMap<String, TopLevel>, Error> {
    let items = match toplevel().parse(Stream::from_iter(
        tokens.last().unwrap().1.clone(),
        tokens.into_iter(),
    )) {
        Ok(items) => items,
        Err(es) => return Error::Parser(es).error(),
    };

    let (includes, mut items) = items
        .into_iter()
        .partition::<Vec<_>, _>(|item| matches!(item, TopLevel::Include(_)));

    for include in includes {
        if let TopLevel::Include(include) = include {
            resolve_include(&include.path.span.file, include.path(), &mut items)?;
        } else {
            unreachable!();
        }
    }

    let mut res = HashMap::new();
    let mut errors = Vec::new();

    for item in items {
        match res.entry(item.name().unwrap()) {
            Entry::Occupied(it) => {
                let redefined: &TopLevel = it.get();
                errors.push(RedefinitionError {
                    redefining_item: item.span(),
                    redefined_item: redefined.span(),
                });
            }
            Entry::Vacant(v) => {
                v.insert(item);
            }
        }
    }

    if errors.is_empty() {
        res.okay()
    } else {
        Error::Redefinition(errors).error()
    }
}
