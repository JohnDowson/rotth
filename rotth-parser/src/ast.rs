mod parsers;

use chumsky::{
    input::Stream,
    prelude::{Input, Rich},
    Parser,
};
use fnv::FnvHashMap;
use rotth_lexer::{lex, Token};
use smol_str::SmolStr;
use somok::{Either, Leaksome};
use spanner::{Span, Spanned};
use std::{
    borrow::Borrow,
    fmt::Debug,
    ops::{Deref, DerefMut},
    path::PathBuf,
    rc::Rc,
};

use crate::{types::Type, Error, ParserError, Redefinition};

#[derive(Debug, Clone)]
pub struct File(pub Vec<Spanned<TopLevel>>);

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
    Mem(Mem),
    Var(Var),
    Struct(Struct),
    Include(Include),
}

impl TopLevel {
    pub fn name(&self) -> Result<SmolStr, Rich<'static, Token, Span>> {
        match self {
            TopLevel::Proc(Proc {
                name:
                    Spanned {
                        span: _,
                        inner: Word(name),
                    },
                ..
            }) => Ok(name.clone()),
            TopLevel::Const(Const {
                name:
                    Spanned {
                        span: _,
                        inner: Word(name),
                    },
                ..
            }) => Ok(name.clone()),
            TopLevel::Mem(Mem {
                name:
                    Spanned {
                        span: _,
                        inner: Word(name),
                    },
                ..
            }) => Ok(name.clone()),
            TopLevel::Var(Var {
                name:
                    Spanned {
                        span: _,
                        inner: Word(name),
                    },
                ..
            }) => Ok(name.clone()),
            TopLevel::Struct(Struct {
                name:
                    Spanned {
                        span: _,
                        inner: Word(name),
                    },
                ..
            }) => Ok(name.clone()),
            TopLevel::Include(Include {
                include: _,
                qualifiers: _,
                path: Spanned { span, inner: name },
            }) => name
                .file_prefix()
                .and_then(|n| n.to_str())
                .map(Into::into)
                .ok_or_else(|| Rich::custom(*span, "Invalid include path: not a file")),
        }
    }
}

impl Debug for TopLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Proc(arg0) => arg0.fmt(f),
            Self::Const(arg0) => arg0.fmt(f),
            Self::Mem(arg0) => arg0.fmt(f),
            Self::Var(arg0) => arg0.fmt(f),
            Self::Struct(arg0) => arg0.fmt(f),
            Self::Include(arg0) => arg0.fmt(f),
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

// (T . (c . (T . ())))
// pub struct SeparatedList<V, S> {
//     val: V,
//     next: Option<Box<SeparatedList<S, V>>>,
// }

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Keyword {
    Include,
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
pub struct Include {
    pub include: Spanned<Keyword>,
    pub qualifiers: Option<Qualifiers>,
    pub path: Spanned<PathBuf>,
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

#[derive(PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ItemPath {
    segments: [SmolStr],
}

impl PartialEq<&ItemPath> for ItemPathBuf {
    fn eq(&self, other: &&ItemPath) -> bool {
        self.segments == other.segments
    }
}

impl PartialEq<ItemPath> for ItemPathBuf {
    fn eq(&self, other: &ItemPath) -> bool {
        self.segments == other.segments
    }
}

impl PartialEq<ItemPathBuf> for ItemPath {
    fn eq(&self, other: &ItemPathBuf) -> bool {
        self.segments == other.segments
    }
}

impl ItemPath {
    pub fn iter(&'_ self) -> impl Iterator<Item = &'_ SmolStr> {
        self.segments.iter()
    }

    pub fn only(&self) -> Option<&str> {
        self.segments.first().map(SmolStr::as_str)
    }

    pub fn last(&self) -> Option<SmolStr> {
        self.segments.last().cloned()
    }

    pub fn segment_mut(&mut self, n: usize) -> Option<&mut SmolStr> {
        self.segments.get_mut(n)
    }

    pub fn drop_first(&self) -> Option<&Self> {
        self.segments
            .split_first()
            .and_then(|(_, b)| if b.is_empty() { None } else { Some(b) })
            .map(|s| unsafe { std::mem::transmute::<&[SmolStr], &ItemPath>(s) })
    }

    pub fn parent(&self) -> Option<&ItemPath> {
        if self.segments.is_empty() {
            None
        } else {
            let segments = &self.segments[..self.segments.len() - 1];
            let parent = unsafe { std::mem::transmute::<&[SmolStr], &ItemPath>(segments) };
            Some(parent)
        }
    }

    pub fn join(&self, other: &Self) -> ItemPathBuf {
        let mut segments = self.segments.to_owned();
        segments.extend_from_slice(&other.segments);
        ItemPathBuf { segments }
    }

    pub fn child(&self, segment: impl Into<SmolStr>) -> ItemPathBuf {
        let mut segments = self.segments.to_owned();
        segments.push(segment.into());
        ItemPathBuf { segments }
    }
}

#[macro_export]
macro_rules! path {
    ( $( $s:tt )::+ ) => {{
        let mut path = ItemPathBuf::new();
        $(path.push(stringify!($s));)*
        path
    }};
    () => {{
        ItemPathBuf::new()
    }};
}

#[derive(PartialEq, Eq, Hash, Default, Clone)]
#[repr(transparent)]
pub struct ItemPathBuf {
    segments: Vec<SmolStr>,
}

impl Deref for ItemPathBuf {
    type Target = ItemPath;

    fn deref(&self) -> &Self::Target {
        unsafe { std::mem::transmute::<&[SmolStr], &ItemPath>(&*self.segments) }
    }
}

impl DerefMut for ItemPathBuf {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { std::mem::transmute::<&mut [SmolStr], &mut ItemPath>(&mut *self.segments) }
    }
}

impl<T: Into<SmolStr>> From<Vec<T>> for ItemPathBuf {
    fn from(segments: Vec<T>) -> Self {
        Self {
            segments: segments.into_iter().map(Into::into).collect(),
        }
    }
}

impl From<SmolStr> for ItemPathBuf {
    fn from(segment: SmolStr) -> Self {
        Self {
            segments: vec![segment],
        }
    }
}

impl Borrow<ItemPath> for ItemPathBuf {
    fn borrow(&self) -> &ItemPath {
        self.deref()
    }
}

impl ToOwned for ItemPath {
    type Owned = ItemPathBuf;

    fn to_owned(&self) -> Self::Owned {
        let segments = self.segments.to_owned();
        Self::Owned { segments }
    }
}

impl ItemPathBuf {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, segment: impl Into<SmolStr>) {
        self.segments.push(segment.into())
    }
}

impl Debug for ItemPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let separator = SmolStr::from("::");
        let segments = self.segments.iter();
        let iter = segments.intersperse(&separator);
        for s in iter {
            write!(f, "{s}")?
        }
        Ok(())
    }
}

impl Debug for ItemPathBuf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.deref().fmt(f)
    }
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
    let path = Default::default();
    resolve_includes_from(path, root)
}

fn resolve_includes_from(
    path: ItemPathBuf,
    root: File,
) -> Result<ResolvedFile, ParserError<'static>> {
    let mut ast: FnvHashMap<SmolStr, Spanned<ResolvedItem>> = Default::default();
    let mut errors = Vec::new();
    for item in root.0 {
        let name = match item.name().map_err(Error::Parser) {
            Ok(name) => name,
            Err(e) => {
                errors.push(e);
                continue;
            }
        };
        let resolved_item = match item.inner {
            TopLevel::Proc(p) => ResolvedItem::Proc(Rc::new(p)),
            TopLevel::Const(c) => ResolvedItem::Const(Rc::new(c)),
            TopLevel::Mem(m) => ResolvedItem::Mem(Rc::new(m)),
            TopLevel::Var(v) => ResolvedItem::Var(Rc::new(v)),
            TopLevel::Struct(s) => match make_struct(s) {
                Ok(s) => ResolvedItem::Struct(Rc::new(s)),
                Err(mut es) => {
                    errors.append(&mut es);
                    continue;
                }
            },
            TopLevel::Include(Include {
                include: _,
                qualifiers,
                path: file_path,
            }) => {
                let file_span = file_path.span;
                let file_name = match file_path
                    .file_prefix()
                    .and_then(|f| f.to_str())
                    .map(SmolStr::from)
                    .ok_or(Error::UnresolvedInclude(file_span))
                {
                    Ok(n) => n,
                    Err(e) => {
                        errors.push(e);
                        continue;
                    }
                };
                let parent = file_path.span.file;
                let file_path = parent.parent().unwrap().join(file_path.inner);
                let file_path = &*file_path.into_boxed_path().leak();
                let src = match std::fs::read_to_string(file_path)
                    .map_err(|_| Error::UnresolvedInclude(file_span))
                {
                    Ok(s) => s,
                    Err(e) => {
                        errors.push(e);
                        continue;
                    }
                };
                let tokens = lex(&src, file_path);

                let module_ast = parse(tokens)?;
                let resolved = resolve_includes_from(path.child(file_name), module_ast)?;
                if let Some(Qualifiers { items, from: _ }) = qualifiers {
                    for qualifier in items {
                        let span = qualifier.span;
                        let item = qualifier.inner;
                        let item_path = resolved.path.join(&item);

                        let i = resolved.find(&item);
                        match i {
                            Some(Spanned { span: _, inner: _ }) => {
                                ast.insert(
                                    item.last().unwrap(),
                                    Spanned {
                                        span,
                                        inner: ResolvedItem::Ref(Rc::new(item_path)),
                                    },
                                );
                            }
                            None => {
                                todo!("Reject importing nonexistant items")
                            }
                        }
                    }
                }
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
