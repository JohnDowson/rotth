mod parsers;

use chumsky::{input::Stream, prelude::Input, Parser};
use fnv::FnvHashMap;
use internment::Intern;
use itempath::{path, ItemPath, ItemPathBuf};
use rotth_lexer::{lex, Token};
use smol_str::SmolStr;
use somok::Either;
use spanner::{Span, Spanned};
use std::{fmt::Debug, rc::Rc};

use crate::{types::Type, Error, ParserError, Redefinition};

#[derive(Debug, Clone)]
pub struct File(pub Vec<Spanned<TopLevel>>);

// #[derive(Debug, Clone)]
// pub struct Module {
//     procs: Vec<Spanned<Proc>>,
//     consts: Vec<Spanned<Const>>,
//     vars: Vec<Spanned<Var>>,
//     structs: Vec<Spanned<Struct>>,
//     modules: Vec<Spanned<ModuleDef>>,
//     uses: Vec<Spanned<Use>>,
// }

#[derive(Debug, Clone)]
pub struct Word(pub SmolStr);

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Punctuation {
    LBracket,
    RBracket,
    Colon,
    RArrow,
}

#[derive(Clone)]
pub enum TopLevel {
    Proc(Proc),
    Const(Const),
    Var(Var),
    Struct(Struct),
    Use(Use),
    Module(ModuleDef),
}

impl TopLevel {
    pub fn name(&self) -> SmolStr {
        match self {
            TopLevel::Proc(Proc {
                name:
                    Spanned {
                        span: _,
                        inner: Word(name),
                    },
                ..
            }) => name.clone(),
            TopLevel::Const(Const {
                name:
                    Spanned {
                        span: _,
                        inner: Word(name),
                    },
                ..
            }) => name.clone(),
            TopLevel::Var(Var {
                name:
                    Spanned {
                        span: _,
                        inner: Word(name),
                    },
                ..
            }) => name.clone(),
            TopLevel::Struct(Struct {
                name:
                    Spanned {
                        span: _,
                        inner: Word(name),
                    },
                ..
            }) => name.clone(),
            TopLevel::Use(Use {
                use_: _,
                name:
                    Spanned {
                        span: _,
                        inner: Word(name),
                    },
                from: _,
                path: _,
            }) => name.clone(),
            TopLevel::Module(ModuleDef {
                module: _,
                name:
                    Spanned {
                        span: _,
                        inner: Word(name),
                    },
            }) => name.clone(),
        }
    }
}

impl Debug for TopLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TopLevel::Proc(arg0) => arg0.fmt(f),
            TopLevel::Const(arg0) => arg0.fmt(f),
            TopLevel::Var(arg0) => arg0.fmt(f),
            TopLevel::Struct(arg0) => arg0.fmt(f),
            TopLevel::Use(arg0) => arg0.fmt(f),
            TopLevel::Module(arg0) => arg0.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Proc {
    pub proc: Spanned<Keyword>,
    pub generics: Option<Spanned<Generics>>,
    pub name: Spanned<Word>,
    pub signature: Spanned<ProcSignature>,
    pub do_: Spanned<Keyword>,
    pub body: Vec<Spanned<Expr>>,
    pub end: Spanned<Keyword>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Keyword {
    Module,
    Use,
    From,
    Return,
    Cond,
    If,
    Else,
    Proc,
    While,
    Do,
    Bind,
    Const,
    Mem,
    Var,
    Struct,
    Cast,
    End,
}

impl std::fmt::Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

#[derive(Debug, Clone)]
pub struct Mem {
    pub mem: Spanned<Keyword>,
    pub name: Spanned<Word>,
    pub do_: Spanned<Keyword>,
    pub body: Vec<Spanned<Expr>>,
    pub end: Spanned<Keyword>,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub struct_: Spanned<Keyword>,
    pub generics: Option<Spanned<Generics>>,
    pub name: Spanned<Word>,
    pub do_: Spanned<Keyword>,
    pub body: Vec<Spanned<NameTypePair>>,
    pub end: Spanned<Keyword>,
}

#[derive(Debug, Clone)]
pub struct Const {
    pub const_: Spanned<Keyword>,
    pub name: Spanned<Word>,
    pub signature: Spanned<ConstSignature>,
    pub do_: Spanned<Keyword>,
    pub body: Vec<Spanned<Expr>>,
    pub end: Spanned<Keyword>,
}

#[derive(Debug, Clone)]
pub struct ModuleDef {
    pub module: Spanned<Keyword>,
    pub name: Spanned<Word>,
}

#[derive(Debug, Clone)]
pub struct Use {
    pub use_: Spanned<Keyword>,
    pub name: Spanned<Word>,
    pub from: Spanned<Keyword>,
    pub path: Spanned<ItemPathBuf>,
}

#[derive(Debug, Clone)]
pub struct Qualifiers {
    pub items: Vec<Spanned<ItemPathBuf>>,
    pub from: Spanned<Keyword>,
}

#[derive(Debug, Clone)]
pub struct Read {
    pub read: Spanned<Token>,
    pub ty: Spanned<Type>,
}

#[derive(Debug, Clone)]
pub struct Write {
    pub write: Spanned<Token>,
    pub ty: Spanned<Type>,
}

#[derive(Clone)]
pub enum Expr {
    CompStop,
    Keyword(Keyword),
    Type(Type),

    Bind(Bind),

    While(While),

    If(If),
    Cond(Box<Cond>),

    Cast(Cast),
    Read(Read),
    Write(Write),

    Word(Word),
    Path(ItemPathBuf),

    Literal(Literal),
    FieldAccess(Box<FieldAccess>),

    Var(Var),
}

impl Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::CompStop => write!(f, "&?&"),
            Expr::Read(r) => write!(f, "@{:?}", r.ty),
            Expr::Write(w) => write!(f, "@{:?}", w.ty),
            Expr::Keyword(arg0) => arg0.fmt(f),
            Expr::Type(arg0) => arg0.fmt(f),
            Expr::Bind(arg0) => arg0.fmt(f),
            Expr::While(arg0) => arg0.fmt(f),
            Expr::If(arg0) => arg0.fmt(f),
            Expr::Cond(arg0) => arg0.fmt(f),
            Expr::Cast(arg0) => arg0.fmt(f),
            Expr::Word(arg0) => arg0.fmt(f),
            Expr::Path(arg0) => arg0.fmt(f),
            Expr::Literal(arg0) => arg0.fmt(f),
            Expr::FieldAccess(arg0) => arg0.fmt(f),
            Expr::Var(arg0) => arg0.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Generics {
    pub left_bracket: Spanned<Punctuation>,
    pub tys: Vec<Spanned<Word>>,
    pub right_bracket: Spanned<Punctuation>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct GenericParams {
    pub left_bracket: Spanned<Punctuation>,
    pub tys: Vec<Spanned<Type>>,
    pub right_bracket: Spanned<Punctuation>,
}

#[derive(Debug, Clone)]
pub struct FieldAccess {
    pub access: Spanned<Punctuation>,
    pub field: Spanned<Word>,
}

#[derive(Debug, Clone)]
pub struct Var {
    pub var: Spanned<Keyword>,
    pub name: Spanned<Word>,
    pub sep: Spanned<Punctuation>,
    pub ty: Spanned<Type>,
}

#[derive(Debug, Clone)]
pub struct ConstSignature {
    pub sep: Spanned<Punctuation>,
    pub tys: Vec<Spanned<Type>>,
}

#[derive(Debug, Clone)]
pub struct ProcSignature {
    pub ins: Vec<Spanned<Type>>,
    pub sep: Option<Spanned<Punctuation>>,
    pub outs: Option<Vec<Spanned<Type>>>,
}

#[derive(Debug, Clone)]
pub struct NameTypePair {
    pub name: Spanned<Word>,
    pub sep: Spanned<Punctuation>,
    pub ty: Spanned<Type>,
}

#[derive(Debug, Clone)]
pub struct While {
    pub while_: Spanned<Keyword>,
    pub cond: Vec<Spanned<Expr>>,
    pub do_: Spanned<Keyword>,
    pub body: Vec<Spanned<Expr>>,
    pub end: Spanned<Keyword>,
}

#[derive(Debug, Clone)]
pub struct Cast {
    pub cast: Spanned<Keyword>,
    pub ty: Spanned<Type>,
}

#[derive(Debug, Clone)]
pub struct If {
    pub if_: Spanned<Keyword>,
    pub truth: Vec<Spanned<Expr>>,
    pub lie: Option<Else>,
    pub end: Spanned<Keyword>,
}

#[derive(Debug, Clone)]
pub struct Else {
    pub else_: Spanned<Keyword>,
    pub body: Vec<Spanned<Expr>>,
}

#[derive(Debug, Clone)]
pub struct Cond {
    pub cond: Spanned<Keyword>,
    pub pat: Spanned<Expr>,
    pub do_: Spanned<Keyword>,
    pub body: Vec<Spanned<Expr>>,
    pub branches: Vec<Spanned<CondBranch>>,
    pub end: Spanned<Keyword>,
}

#[derive(Debug, Clone)]
pub struct CondBranch {
    pub else_: Spanned<Keyword>,
    pub pat: Spanned<Expr>,
    pub do_: Spanned<Keyword>,
    pub body: Vec<Spanned<Expr>>,
}

#[derive(Debug, Clone)]
pub struct Bind {
    pub bind: Spanned<Keyword>,
    pub bindings: Vec<Spanned<Either<Word, NameTypePair>>>,
    pub do_: Spanned<Keyword>,
    pub body: Vec<Spanned<Expr>>,
    pub end: Spanned<Keyword>,
}

#[derive(Clone)]
pub enum Literal {
    Bool(bool),
    Int(i64),
    UInt(u64),
    String(SmolStr),
    Char(char),
}

impl Debug for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(arg0) => arg0.fmt(f),
            Self::Int(arg0) => arg0.fmt(f),
            Self::UInt(arg0) => arg0.fmt(f),
            Self::String(arg0) => arg0.fmt(f),
            Self::Char(arg0) => arg0.fmt(f),
        }
    }
}

pub fn parse(tokens: Vec<(Token, Span)>) -> Result<File, ParserError<'static>> {
    let len = tokens.len();
    let path = tokens.first().unwrap().1.file;
    parsers::file()
        .parse(Stream::from_iter(tokens).spanned(Span::point(path, len)))
        .into_result()
        .map_err(|e| e.into())
}

#[derive(Clone)]
pub enum ResolvedItem {
    Ref(Rc<ItemPathBuf>),
    Proc(Rc<Proc>),
    Const(Rc<Const>),
    Mem(Rc<Mem>),
    Var(Rc<Var>),
    Struct(Rc<ResolvedStruct>),
    Module(Rc<ResolvedFile>),
}

impl Debug for ResolvedItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ref(arg0) => f.debug_tuple("Ref").field(arg0).finish(),
            Self::Proc(arg0) => arg0.fmt(f),
            Self::Const(arg0) => arg0.fmt(f),
            Self::Mem(arg0) => arg0.fmt(f),
            Self::Var(arg0) => arg0.fmt(f),
            Self::Struct(arg0) => arg0.fmt(f),
            Self::Module(arg0) => arg0.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ResolvedFile {
    pub path: ItemPathBuf,
    pub ast: FnvHashMap<SmolStr, Spanned<ResolvedItem>>,
}

impl ResolvedFile {
    pub fn find(&self, path: &ItemPath) -> Option<Spanned<ResolvedItem>> {
        let mut segments = path.iter();
        let segment = segments.next();
        let item = segment.and_then(|s| self.ast.get(s));
        if let Some(path) = path.drop_first() {
            match item {
                Some(i) => match &**i {
                    ResolvedItem::Ref(_) => return None,
                    ResolvedItem::Proc(_) => return None,
                    ResolvedItem::Const(_) => return None,
                    ResolvedItem::Mem(_) => return None,
                    ResolvedItem::Var(_) => return None,
                    ResolvedItem::Struct(_) => return None,
                    ResolvedItem::Module(f) => return f.find(path),
                },
                None => return None,
            }
        }
        item.cloned()
    }
}

#[derive(Debug, Clone)]
pub struct ResolvedStruct {
    pub name: Spanned<Word>,
    pub generics: Vec<Spanned<SmolStr>>,
    pub fields: FnvHashMap<SmolStr, Spanned<NameTypePair>>,
}

pub fn resolve_includes(root: File) -> Result<ResolvedFile, ParserError<'static>> {
    let path = path!();
    resolve_includes_from(path, root)
}

fn resolve_includes_from(
    path: ItemPathBuf,
    root: File,
) -> Result<ResolvedFile, ParserError<'static>> {
    let mut ast: FnvHashMap<SmolStr, Spanned<ResolvedItem>> = Default::default();
    let mut errors = Vec::new();
    for item in root.0 {
        let src_file = item.span.file;
        let name = item.inner.name();
        let resolved_item = match item.inner {
            TopLevel::Proc(p) => ResolvedItem::Proc(Rc::new(p)),
            TopLevel::Const(c) => ResolvedItem::Const(Rc::new(c)),
            TopLevel::Var(v) => ResolvedItem::Var(Rc::new(v)),
            TopLevel::Struct(s) => match make_struct(s) {
                Ok(s) => ResolvedItem::Struct(Rc::new(s)),
                Err(mut es) => {
                    errors.append(&mut es);
                    continue;
                }
            },
            TopLevel::Use(Use {
                use_: _,
                name: _,
                from: _,
                path:
                    Spanned {
                        span: _,
                        inner: from,
                    },
            }) => {
                todo!()
            }
            TopLevel::Module(ModuleDef { module: _, name: _ }) => {
                let mut module_file = (*src_file).to_owned();
                module_file.set_extension("");
                module_file.push(&*name);
                module_file.set_extension("rh");

                let module_src = std::fs::read_to_string(dbg! { &module_file }).unwrap();
                let tokens = lex(&module_src, Intern::new(module_file));

                let mut module_path = path.clone();
                module_path.push(&*name);

                let module_ast = parse(tokens)?;
                let resolved = resolve_includes_from(module_path, module_ast)?;

                ResolvedItem::Module(Rc::new(resolved))
            }
        };

        if let Some(redefined) = ast.get(&name) {
            errors.push(Error::Redefinition(Redefinition {
                redefining_item: item.span,
                redefined_item: redefined.span,
            }))
        } else {
            ast.insert(
                name,
                Spanned {
                    span: item.span,
                    inner: resolved_item,
                },
            );
        }
    }

    if errors.is_empty() {
        Ok(ResolvedFile { path, ast })
    } else {
        Err(ParserError(errors))
    }
}

fn make_struct(s: Struct) -> Result<ResolvedStruct, Vec<Error<'static>>> {
    let mut errors = Vec::default();
    let mut fields: FnvHashMap<SmolStr, Spanned<NameTypePair>> = Default::default();
    for field in s.body {
        let Word(name) = &*field.name;

        if let Some(redefined) = fields.get(name) {
            errors.push(Error::Redefinition(Redefinition {
                redefining_item: field.span,
                redefined_item: redefined.span,
            }))
        } else {
            fields.insert(name.clone(), field);
        }
    }
    let mut generics = Vec::new();
    if let Some(Spanned {
        span: _,
        inner:
            Generics {
                left_bracket: _,
                tys,
                right_bracket: _,
            },
    }) = s.generics
    {
        for ty in tys {
            generics.push(ty.map(|Word(ty)| ty))
        }
    }
    if errors.is_empty() {
        Ok(ResolvedStruct {
            name: s.name,
            generics,
            fields,
        })
    } else {
        Err(errors)
    }
}
