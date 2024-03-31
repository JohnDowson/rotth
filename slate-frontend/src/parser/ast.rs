use std::fmt::Debug;

use internment::Intern;
use itempath::ItemPathBuf;
use spanner::Spanned;

use crate::lexer::Token;

use super::types::Type;

#[derive(Debug, Clone)]
pub struct Module {
    pub procs: Vec<Spanned<Proc>>,
    pub consts: Vec<Spanned<Const>>,
    pub vars: Vec<Spanned<Static>>,
    pub structs: Vec<Spanned<Struct>>,
    pub modules: Vec<Spanned<ModuleDef>>,
    pub uses: Vec<Spanned<Use>>,
}

#[derive(Clone)]
pub struct Word(pub Intern<String>);

impl Debug for Word {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Word({})", self.0)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Punctuation {
    LBracket,
    RBracket,
    LParen,
    RParen,
    And,
    Mul,
    Div,
    Plus,
    Minus,
    Assign,
    Eq,
    Semicolon,
    Colon,
    DoubleColon,
    Dot,
    FatArrow,
}

#[derive(Clone)]
pub enum TopLevel {
    Proc(Proc),
    Const(Const),
    Var(Static),
    Struct(Struct),
    Use(Use),
    Module(ModuleDef),
}

impl TopLevel {
    pub fn name(&self) -> Intern<String> {
        match self {
            TopLevel::Proc(Proc {
                name:
                    Spanned {
                        span: _,
                        inner: Word(name),
                    },
                ..
            }) => *name,
            TopLevel::Const(Const {
                name:
                    Spanned {
                        span: _,
                        inner: Word(name),
                    },
                ..
            }) => *name,
            TopLevel::Var(Static {
                name:
                    Spanned {
                        span: _,
                        inner: Word(name),
                    },
                ..
            }) => *name,
            TopLevel::Struct(Struct {
                name:
                    Spanned {
                        span: _,
                        inner: Word(name),
                    },
                ..
            }) => *name,
            TopLevel::Use(Use {
                use_: _,
                name:
                    Spanned {
                        span: _,
                        inner: Word(name),
                    },
                from: _,
                path: _,
            }) => *name,
            TopLevel::Module(ModuleDef {
                module: _,
                name:
                    Spanned {
                        span: _,
                        inner: Word(name),
                    },
            }) => *name,
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
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Keyword {
    Module,
    Use,
    From,
    Return,
    Match,
    If,
    Else,
    Proc,
    While,
    Do,
    Let,
    Const,
    Static,
    Struct,
    Cast,
}

impl std::fmt::Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub struct_: Spanned<Keyword>,
    pub generics: Option<Spanned<Generics>>,
    pub name: Spanned<Word>,
    pub do_: Spanned<Keyword>,
    pub body: Vec<Spanned<NameTypePair>>,
}

#[derive(Debug, Clone)]
pub struct Const {
    pub const_: Spanned<Keyword>,
    pub name: Spanned<Word>,
    pub signature: Spanned<ConstSignature>,
    pub do_: Spanned<Keyword>,
    pub body: Vec<Spanned<Expr>>,
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
    Keyword(Keyword),
    Type(Type),

    Let(Box<Let>),

    While(Box<While>),

    If(If),
    Cond(Box<Match>),

    Cast(Cast),
    Read(Read),
    Write(Write),
    Ref(Box<Unary>),
    Deref(Box<Unary>),

    Word(Word),
    Path(ItemPathBuf),

    Literal(Literal),
    FieldAccess(Box<FieldAccess>),
    Add(Box<Binary>),
    Sub(Box<Binary>),
    Mul(Box<Binary>),
    Div(Box<Binary>),
    Eq(Box<Binary>),
    Assign(Box<Binary>),
    Call(Box<Call>),

    Static(Static),
}

impl Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Read(r) => write!(f, "@{:?}", r.ty),
            Expr::Write(w) => write!(f, "@{:?}", w.ty),
            Expr::Keyword(arg0) => arg0.fmt(f),
            Expr::Type(arg0) => arg0.fmt(f),
            Expr::Let(arg0) => arg0.fmt(f),
            Expr::While(arg0) => arg0.fmt(f),
            Expr::If(arg0) => arg0.fmt(f),
            Expr::Cond(arg0) => arg0.fmt(f),
            Expr::Cast(arg0) => arg0.fmt(f),
            Expr::Word(arg0) => arg0.fmt(f),
            Expr::Path(arg0) => arg0.fmt(f),
            Expr::Literal(arg0) => arg0.fmt(f),
            Expr::FieldAccess(arg0) => arg0.fmt(f),
            Expr::Add(arg0) => arg0.fmt(f),
            Expr::Sub(arg0) => arg0.fmt(f),
            Expr::Mul(arg0) => arg0.fmt(f),
            Expr::Div(arg0) => arg0.fmt(f),
            Expr::Eq(arg0) => arg0.fmt(f),
            Expr::Assign(arg0) => arg0.fmt(f),
            Expr::Static(arg0) => arg0.fmt(f),
            Expr::Deref(arg0) => arg0.fmt(f),
            Expr::Ref(arg0) => arg0.fmt(f),
            Expr::Call(arg0) => arg0.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Call {
    pub callee: Spanned<Expr>,
    pub lparen: Spanned<Punctuation>,
    pub args: Vec<Spanned<Expr>>,
    pub rparen: Spanned<Punctuation>,
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub left: Spanned<Expr>,
    pub op: Spanned<Punctuation>,
    pub right: Spanned<Expr>,
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub op: Spanned<Punctuation>,
    pub expr: Spanned<Expr>,
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
    pub reciever: Spanned<Expr>,
    pub access: Spanned<Punctuation>,
    pub field: Spanned<Word>,
}

#[derive(Debug, Clone)]
pub struct Static {
    pub static_: Spanned<Keyword>,
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
    pub cond: Spanned<Expr>,
    pub do_: Spanned<Keyword>,
    pub body: Vec<Spanned<Expr>>,
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
}

#[derive(Debug, Clone)]
pub struct Else {
    pub else_: Spanned<Keyword>,
    pub body: Vec<Spanned<Expr>>,
}

#[derive(Debug, Clone)]
pub struct Match {
    pub match_: Spanned<Keyword>,
    pub expr: Spanned<Expr>,
    pub do_: Spanned<Keyword>,
    pub branches: Vec<Spanned<MatchBranch>>,
}

#[derive(Debug, Clone)]
pub struct MatchBranch {
    pub pat: Spanned<Expr>,
    pub fat_arrow: Spanned<Punctuation>,
    pub body: Vec<Spanned<Expr>>,
}

#[derive(Debug, Clone)]
pub struct Let {
    pub let_: Spanned<Keyword>,
    pub pat: Spanned<Word>,
    pub assign: Spanned<Punctuation>,
    pub body: Spanned<Expr>,
}
#[derive(Clone)]
pub enum Literal {
    Bool(bool),
    Int(i64),
    UInt(u64),
    String(Intern<String>),
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
