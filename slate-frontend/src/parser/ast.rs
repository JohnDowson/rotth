use std::fmt::Debug;

use super::types::Type;
use internment::Intern;
use itempath::ItemPathBuf;
use spanner::Spanned;

#[derive(Debug, Clone)]
pub struct ResolvedModule {
    pub funcs: Vec<Spanned<Func>>,
    pub consts: Vec<Spanned<Const>>,
    pub statics: Vec<Spanned<Static>>,
    pub structs: Vec<Spanned<Struct>>,
    pub inherent_impls: Vec<Spanned<InherentImpl>>,
    pub traits: Vec<Spanned<Trait>>,
    pub trait_impls: Vec<Spanned<TraitImpl>>,
    pub modules: Vec<Spanned<ResolvedModule>>,
    pub uses: Vec<Spanned<Use>>,
}

#[derive(Debug, Clone)]
pub struct Module {
    pub funcs: Vec<Spanned<Func>>,
    pub consts: Vec<Spanned<Const>>,
    pub statics: Vec<Spanned<Static>>,
    pub structs: Vec<Spanned<Struct>>,
    pub inherent_impls: Vec<Spanned<InherentImpl>>,
    pub traits: Vec<Spanned<Trait>>,
    pub trait_impls: Vec<Spanned<TraitImpl>>,
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
    Func(Func),
    Const(Const),
    Static(Static),
    Struct(Struct),
    Trait(Trait),
    InherentImpl(InherentImpl),
    TraitImpl(TraitImpl),
    Use(Use),
    Module(ModuleDef),
}

impl Debug for TopLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TopLevel::Func(arg0) => arg0.fmt(f),
            TopLevel::Const(arg0) => arg0.fmt(f),
            TopLevel::Static(arg0) => arg0.fmt(f),
            TopLevel::Struct(arg0) => arg0.fmt(f),
            TopLevel::Trait(arg0) => arg0.fmt(f),
            TopLevel::InherentImpl(arg0) => arg0.fmt(f),
            TopLevel::TraitImpl(arg0) => arg0.fmt(f),
            TopLevel::Use(arg0) => arg0.fmt(f),
            TopLevel::Module(arg0) => arg0.fmt(f),
        }
    }
}

#[derive(Clone)]
pub enum ImplLevel {
    Func(Func),
    Const(Const),
    Static(Static),
}

impl Debug for ImplLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ImplLevel::Func(arg0) => arg0.fmt(f),
            ImplLevel::Const(arg0) => arg0.fmt(f),
            ImplLevel::Static(arg0) => arg0.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Func {
    pub func_decl: FuncDecl,
    pub body: Vec<Spanned<Expr>>,
}

#[derive(Debug, Clone)]
pub struct FuncDecl {
    pub func: Spanned<Keyword>,
    pub generics: Option<Generics>,
    pub name: Spanned<Word>,
    pub signature: FuncSignature,
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
    Func,
    While,
    For,
    Let,
    Const,
    Static,
    Struct,
    Trait,
    Impl,
    Cast,
    Lambda,
}

impl std::fmt::Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub struct_: Spanned<Keyword>,
    pub generics: Option<Generics>,
    pub name: Spanned<Word>,
    pub body: Vec<NameTypePair>,
}

#[derive(Debug, Clone)]
pub struct Trait {
    pub trait_: Spanned<Keyword>,
    pub generics: Option<Generics>,
    pub name: Spanned<Word>,
    pub body: Vec<Spanned<FuncDecl>>,
}

#[derive(Debug, Clone)]
pub struct StructLiteral {
    pub ty: Spanned<ItemPathBuf>,
    pub fields: Vec<FieldValuePair>,
}

#[derive(Debug, Clone)]
pub struct InherentImpl {
    pub impl_: Spanned<Keyword>,
    pub generics: Option<Generics>,
    pub ty: Spanned<Type>,
    pub body: Vec<Spanned<ImplLevel>>,
}

#[derive(Debug, Clone)]
pub struct TraitImpl {
    pub impl_: Spanned<Keyword>,
    pub generics: Option<Generics>,
    pub trait_: Spanned<ItemPathBuf>,
    pub for_: Spanned<Keyword>,
    pub ty: Spanned<Type>,
    pub body: Vec<Spanned<ImplLevel>>,
}

#[derive(Debug, Clone)]
pub struct Const {
    pub const_: Spanned<Keyword>,
    pub name: Spanned<Word>,
    pub signature: TypeAscription,
    pub assign: Spanned<Punctuation>,
    pub expr: Spanned<Expr>,
}

#[derive(Debug, Clone)]
pub struct ModuleDef {
    pub module: Spanned<Keyword>,
    pub name: Spanned<Word>,
}

#[derive(Debug, Clone)]
pub struct Use {
    pub use_: Spanned<Keyword>,
    pub path: Spanned<ItemPathBuf>,
}

#[derive(Clone, Debug)]
pub struct Return {
    pub return_: Spanned<Keyword>,
    pub expr: Option<Spanned<Expr>>,
}

#[derive(Clone, Debug)]
pub struct Lambda {
    pub lambda: Spanned<Keyword>,
    pub arglist: Vec<LambdaArg>,
    pub body: Vec<Spanned<Expr>>,
}

#[derive(Clone, Debug)]
pub struct LambdaArg {
    pub name: Spanned<Word>,
    pub ascription: Option<TypeAscription>,
}

#[derive(Clone)]
pub enum Expr {
    Return(Box<Return>),

    Let(Box<Let>),
    Lambda(Lambda),

    While(Box<While>),

    If(Box<If>),
    Cond(Box<Match>),

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

    Static(Box<Static>),
    Const(Box<Const>),
}

impl Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Return(arg0) => arg0.fmt(f),
            Expr::Let(arg0) => arg0.fmt(f),
            Expr::Lambda(arg0) => arg0.fmt(f),
            Expr::While(arg0) => arg0.fmt(f),
            Expr::If(arg0) => arg0.fmt(f),
            Expr::Cond(arg0) => arg0.fmt(f),
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
            Expr::Deref(arg0) => arg0.fmt(f),
            Expr::Ref(arg0) => arg0.fmt(f),
            Expr::Call(arg0) => arg0.fmt(f),
            Expr::Static(arg0) => arg0.fmt(f),
            Expr::Const(arg0) => arg0.fmt(f),
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
    pub tys: Vec<Generic>,
    pub right_bracket: Spanned<Punctuation>,
}

#[derive(Debug, Clone)]
pub struct Generic {
    pub ty: Spanned<Word>,
    pub constraint: Option<Constraint>,
}

#[derive(Debug, Clone)]
pub struct Constraint {
    pub sep: Spanned<Punctuation>,
    pub constraint: Vec<Spanned<ItemPathBuf>>,
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
    pub signature: TypeAscription,
    pub assign: Spanned<Punctuation>,
    pub expr: Spanned<Expr>,
}

#[derive(Debug, Clone)]
pub struct TypeAscription {
    pub sep: Spanned<Punctuation>,
    pub ty: Spanned<Type>,
}

#[derive(Debug, Clone)]
pub struct FuncSignature {
    pub ins: Vec<NameTypePair>,
    pub return_: Option<SignatureReturn>,
}

#[derive(Debug, Clone)]
pub struct SignatureReturn {
    pub arrow: Spanned<Punctuation>,
    pub ty: Spanned<Type>,
}

#[derive(Debug, Clone)]
pub struct NameTypePair {
    pub name: Spanned<Word>,
    pub sep: Spanned<Punctuation>,
    pub ty: Spanned<Type>,
}

#[derive(Debug, Clone)]
pub struct FieldValuePair {
    pub name: Spanned<Word>,
    pub sep: Spanned<Punctuation>,
    pub expr: Spanned<Expr>,
}

#[derive(Debug, Clone)]
pub struct While {
    pub while_: Spanned<Keyword>,
    pub cond: Spanned<Expr>,
    pub body: Vec<Spanned<Expr>>,
}

#[derive(Debug, Clone)]
pub struct If {
    pub if_: Spanned<Keyword>,
    pub cond: Spanned<Expr>,
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
    pub ascription: Option<TypeAscription>,
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
    Struct(StructLiteral),
    Tuple(Vec<Spanned<Expr>>),
    Array(Vec<Spanned<Expr>>),
}

impl Debug for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Bool(arg0) => arg0.fmt(f),
            Literal::Int(arg0) => arg0.fmt(f),
            Literal::UInt(arg0) => arg0.fmt(f),
            Literal::String(arg0) => arg0.fmt(f),
            Literal::Char(arg0) => arg0.fmt(f),
            Literal::Struct(arg0) => arg0.fmt(f),
            Literal::Tuple(arg0) => write!(f, "({:?})", arg0),
            Literal::Array(arg0) => write!(f, "[{:?}]", arg0),
        }
    }
}
