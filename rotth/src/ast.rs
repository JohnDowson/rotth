#[cfg(test)]
mod test;

use std::{
    collections::hash_map::Entry,
    path::{Path, PathBuf},
};

use crate::{
    iconst::IConst,
    lexer::{KeyWord, Token},
    resolver::resolve_include,
    span::Span,
    types::{self, Primitive, StructIndex, ValueType},
    Error, RedefinitionError,
};
use chumsky::{prelude::*, Stream};
use fnv::FnvHashMap;
use somok::Somok;

#[derive(Debug, Clone)]
pub enum TopLevel {
    Proc(Proc),
    Const(Const),
    Mem(Mem),
    Var(ToplevelVar),
    Struct(Struct),
    Include(Include),
}

impl TopLevel {
    pub fn name(&self) -> Option<String> {
        let name_node = match self {
            TopLevel::Proc(i) => &i.name,
            TopLevel::Const(i) => &i.name,
            TopLevel::Mem(i) => &i.name,
            TopLevel::Var(i) => &i.name,
            TopLevel::Struct(i) => &i.name,
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
            TopLevel::Var(i) => &i.name,
            TopLevel::Struct(i) => &i.name,
            TopLevel::Include(i) => &i.include,
        }
        .span
        .clone()
    }
}

#[derive(Debug, Clone)]
pub struct Proc {
    pub proc: AstNode,
    pub generics: Option<AstNode>,
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
pub struct ToplevelVar {
    pub var: AstNode,
    pub name: AstNode,
    pub sep: AstNode,
    pub ty: AstNode,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub struct_: AstNode,
    pub name: AstNode,
    pub do_: AstNode,
    pub body: Vec<AstNode>,
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
    Accessor,

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

    LBracket,
    RBracket,
    Generics(Generics),
    ProcSignature(ProcSignature),
    ConstSignature(ConstSignature),

    Body(Vec<AstNode>),
    StructField(StructField),
    Var(Box<Var>),
    FieldAccess(Box<FieldAccess>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Generics {
    pub left_bracket: Box<AstNode>,
    pub tys: Vec<AstNode>,
    pub right_bracket: Box<AstNode>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FieldAccess {
    pub access: AstNode,
    pub field: AstNode,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Var {
    pub var: AstNode,
    pub ret: Option<AstNode>,
    pub name: AstNode,
    pub sep: AstNode,
    pub ty: AstNode,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type {
    pub ptr_count: usize,
    pub type_name: String,
}

impl Type {
    pub fn to_primitive_type(self) -> types::Type {
        let primitive = match &*self.type_name {
            "bool" => Primitive::Bool,
            "char" => Primitive::Char,

            "u64" => Primitive::U64,
            "u32" => Primitive::U32,
            "u16" => Primitive::U16,
            "u8" => Primitive::U8,

            "i64" => Primitive::I64,
            "i32" => Primitive::I32,
            "i16" => Primitive::I16,
            "i8" => Primitive::I8,
            t => todo!(
                "Can only parse primitive types at this time! Type: {} is not primitive",
                t
            ),
        };
        let value_type = ValueType::Primitive(primitive);
        let ptr_depth = self.ptr_count;
        types::Type {
            ptr_depth,
            value_type,
        }
    }

    pub fn to_type(self, structs: &StructIndex) -> Option<types::Type> {
        let value_type = match &*self.type_name {
            "bool" => ValueType::Primitive(Primitive::Bool),
            "char" => ValueType::Primitive(Primitive::Char),

            "u64" => ValueType::Primitive(Primitive::U64),
            "u32" => ValueType::Primitive(Primitive::U32),
            "u16" => ValueType::Primitive(Primitive::U16),
            "u8" => ValueType::Primitive(Primitive::U8),

            "i64" => ValueType::Primitive(Primitive::I64),
            "i32" => ValueType::Primitive(Primitive::I32),
            "i16" => ValueType::Primitive(Primitive::I16),
            "i8" => ValueType::Primitive(Primitive::I8),
            n => ValueType::Struct(structs.name_to_id(n)?),
        };
        let ptr_depth = self.ptr_count;
        types::Type {
            ptr_depth,
            value_type,
        }
        .some()
    }
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
pub struct StructField {
    pub name: Box<AstNode>,
    pub sep: Box<AstNode>,
    pub ty: Box<AstNode>,
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
    just(Token::Ptr)
        .repeated()
        .then(word())
        .map_with_span(|(ptr, ty), span| AstNode {
            span,
            ast: AstKind::Type(Type {
                ptr_count: ptr.len(),
                type_name: coerce_ast!(ty => Word || unreachable!()),
            }),
        })
}
fn literal() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! {
        Token::Bool(b), span => AstNode { span, ast: AstKind::Literal(IConst::Bool(b)) },
        Token::Num(n), span => AstNode { span, ast: AstKind::Literal(IConst::U64(n.parse().unwrap())) },
        Token::Str(s), span => AstNode { span, ast: AstKind::Literal(IConst::Str(s)) },
        Token::Char(c), span => AstNode { span, ast: AstKind::Literal(IConst::Char(c)) },
    }
}
fn include_path() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! {
        Token::Str(s), span => AstNode { span, ast: AstKind::Path(PathBuf::from(s)) },
    }
}
fn kw_include() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! {
        Token::KeyWord(kw @ KeyWord::Include), span=> AstNode { span, ast: AstKind::KeyWord(kw) },
    }
}
fn kw_bind() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! {
        Token::KeyWord(kw @ KeyWord::Bind), span => AstNode { span, ast: AstKind::KeyWord(kw) },
    }
}
fn kw_while() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! {
        Token::KeyWord(kw @ KeyWord::While), span => AstNode { span, ast: AstKind::KeyWord(kw) },
    }
}
fn kw_cond() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! {
        Token::KeyWord(kw @ KeyWord::Cond), span => AstNode { span, ast: AstKind::KeyWord(kw) },
    }
}
fn kw_if() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! {
        Token::KeyWord(kw @ KeyWord::If), span => AstNode { span, ast: AstKind::KeyWord(kw) },
    }
}
fn kw_else() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! {
        Token::KeyWord(kw @ KeyWord::Else), span => AstNode { span, ast: AstKind::KeyWord(kw) },
    }
}
fn kw_do() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! {
        Token::KeyWord(kw @ KeyWord::Do), span => AstNode { span, ast: AstKind::KeyWord(kw) },
    }
}
fn kw_end() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! {
        Token::KeyWord(kw @ KeyWord::End), span => AstNode { span, ast: AstKind::KeyWord(kw) },
    }
}
fn kw_ret() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! {
        Token::KeyWord(kw @ KeyWord::Return), span => AstNode { span, ast: AstKind::KeyWord(kw) },
    }
}
fn kw_cast() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! {
        Token::KeyWord(kw @ KeyWord::Cast), span => AstNode { span, ast: AstKind::KeyWord(kw) },
    }
}
fn kw_proc() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! {
        Token::KeyWord(kw @ KeyWord::Proc), span => AstNode { span, ast: AstKind::KeyWord(kw) },
    }
}
fn kw_const() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! {
        Token::KeyWord(kw @ KeyWord::Const), span => AstNode { span, ast: AstKind::KeyWord(kw) },
    }
}
fn kw_mem() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! {
        Token::KeyWord(kw @ KeyWord::Mem), span => AstNode { span, ast: AstKind::KeyWord(kw) },
    }
}
fn kw_var() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! {
        Token::KeyWord(kw @ KeyWord::Var), span => AstNode { span, ast: AstKind::KeyWord(kw) },
    }
}
fn kw_struct() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! {
        Token::KeyWord(kw @ KeyWord::Struct), span => AstNode { span, ast: AstKind::KeyWord(kw) },
    }
}

fn word() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! {
        Token::Word(w), span => AstNode { span, ast: AstKind::Word(w) },
    }
}
fn separator() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! {
        Token::SigSep, span => AstNode { span, ast: AstKind::Separator },
    }
}
fn ignore() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    select! {
        Token::Ignore, span => AstNode { span, ast: AstKind::Binding(Binding::Ignore) },
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

fn var() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    kw_var()
        .then(kw_ret().or_not())
        .then(word())
        .then(separator())
        .then(ty())
        .map_with_span(|((((var, ret), name), sep), ty), span| AstNode {
            span,
            ast: AstKind::Var(box Var {
                var,
                ret,
                name,
                sep,
                ty,
            }),
        })
}
fn accessor() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    just(Token::FieldAccess).map_with_span(|_, span| AstNode {
        span,
        ast: AstKind::Accessor,
    })
}

fn field_access() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    accessor()
        .then(word())
        .map_with_span(|(access, field), span| AstNode {
            span,
            ast: AstKind::FieldAccess(box FieldAccess { access, field }),
        })
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

        choice((
            field_access(),
            literal(),
            var(),
            word(),
            bind,
            while_,
            if_,
            cond,
            cast,
            kw_ret(),
        ))
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

fn lbracket() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    just(Token::LBracket).map_with_span(|_, span| AstNode {
        span,
        ast: AstKind::LBracket,
    })
}
fn rbracket() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    just(Token::RBracket).map_with_span(|_, span| AstNode {
        span,
        ast: AstKind::RBracket,
    })
}

fn generics() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    lbracket()
        .then(word().repeated().at_least(1))
        .then(rbracket())
        .map_with_span(|((left_bracket, tys), right_bracket), span| AstNode {
            span,
            ast: AstKind::Generics(Generics {
                left_bracket: box left_bracket,
                tys,
                right_bracket: box right_bracket,
            }),
        })
}

fn proc() -> impl Parser<Token, TopLevel, Error = Simple<Token, Span>> {
    kw_proc()
        .then(generics().or_not())
        .then(word())
        .then(proc_signature())
        .then(kw_do())
        .then(body())
        .then(kw_end())
        .map(
            |((((((proc, generics), name), signature), do_), body), end)| {
                TopLevel::Proc(Proc {
                    proc,
                    generics,
                    name,
                    signature,
                    do_,
                    body,
                    end,
                })
            },
        )
}

fn const_signature() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    separator()
        .then(ty().repeated().at_least(1))
        .map_with_span(|(sep, tys), span| AstNode {
            span,
            ast: AstKind::ConstSignature(ConstSignature { sep: box sep, tys }),
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

fn toplevel_var() -> impl Parser<Token, TopLevel, Error = Simple<Token, Span>> {
    kw_var()
        .then(word())
        .then(separator())
        .then(ty())
        .map(|(((var, name), sep), ty)| TopLevel::Var(ToplevelVar { var, name, sep, ty }))
}

fn struct_field() -> impl Parser<Token, AstNode, Error = Simple<Token, Span>> {
    word()
        .then(separator())
        .then(ty())
        .map_with_span(|((name, sep), ty), span| AstNode {
            span,
            ast: AstKind::StructField(StructField {
                name: box name,
                sep: box sep,
                ty: box ty,
            }),
        })
}
fn struct_() -> impl Parser<Token, TopLevel, Error = Simple<Token, Span>> {
    kw_struct()
        .then(word())
        .then(kw_do())
        .then(struct_field().repeated())
        .then(kw_end())
        .map(|((((struct_, name), do_), body), end)| {
            TopLevel::Struct(Struct {
                struct_,
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
    choice((
        include(),
        proc(),
        const_(),
        mem(),
        toplevel_var(),
        struct_(),
    ))
    .repeated()
    .then_ignore(end())
    .or(end().to(vec![]))
}

pub fn parse_no_include(tokens: Vec<(Token, Span)>) -> Result<Vec<TopLevel>, Error> {
    toplevel()
        .parse(Stream::from_iter(
            tokens
                .last()
                .map(|(_, s)| s.clone())
                .unwrap_or_else(|| Span::point("", 0)),
            tokens.into_iter(),
        ))
        .map_err(Error::Parser)
}

pub fn parse(tokens: Vec<(Token, Span)>) -> Result<FnvHashMap<String, TopLevel>, Error> {
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

    let mut res = FnvHashMap::default();
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
