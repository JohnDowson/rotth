use internment::Intern;
use itempath::{path, ItemPathBuf};
use slotmap::SlotMap;
use spanner::{Span, Spanned};

pub mod key {
    slotmap::new_key_type! {
        pub struct Entity;
        pub struct Module;
        pub struct Func;
        pub struct Expr;
    }
}

#[derive(Debug, Default)]
pub struct Ast {
    pub modules: SlotMap<key::Module, Spanned<Module>>,
    pub funcs: SlotMap<key::Func, Spanned<Func>>,
    pub exprs: SlotMap<key::Expr, Spanned<Expr>>,
}

impl Ast {
    pub fn insert<T: AstEntity>(&mut self, ent: T) -> Spanned<T::Key> {
        ent.emit(self)
    }
}

pub trait AstEntity {
    type Key: slotmap::Key;

    fn emit(self, ast: &mut Ast) -> Spanned<Self::Key>;
}

impl AstEntity for Spanned<Module> {
    type Key = key::Module;

    fn emit(self, ast: &mut Ast) -> Spanned<Self::Key> {
        self.span.spanned(ast.modules.insert(self))
    }
}

impl AstEntity for Spanned<Expr> {
    type Key = key::Expr;

    fn emit(self, ast: &mut Ast) -> Spanned<Self::Key> {
        self.span.spanned(ast.exprs.insert(self))
    }
}

impl AstEntity for Spanned<Func> {
    type Key = key::Func;

    fn emit(self, ast: &mut Ast) -> Spanned<Self::Key> {
        self.span.spanned(ast.funcs.insert(self))
    }
}

#[derive(Debug, Clone)]
pub struct Module {
    pub items: Vec<Spanned<ModuleItem>>,
}

#[derive(Debug, Clone)]
pub enum ModuleItem {
    Func(key::Func),
}

#[derive(Debug, Clone)]
pub struct Func {
    pub func_decl: FuncDecl,
    pub body: Vec<Spanned<key::Expr>>,
}

#[derive(Debug, Clone)]
pub struct FuncDecl {
    pub func: Span,
    pub generics: Option<Generics>,
    pub name: Spanned<Word>,
    pub signature: FuncSignature,
}

#[derive(Clone)]
pub struct Word(pub Intern<String>);

impl std::fmt::Debug for Word {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Word({})", self.0)
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Operator {
    And,
    Mul,
    Div,
    Plus,
    Minus,
    Assign,
    Eq,
    Dot,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub struct_: Span,
    pub generics: Option<Generics>,
    pub name: Spanned<Word>,
    pub body: Vec<NameTypePair>,
}

#[derive(Debug, Clone)]
pub struct Trait {
    pub trait_: Span,
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
pub struct Const {
    pub const_: Span,
    pub name: Spanned<Word>,
    pub signature: Spanned<Type>,
    pub assign: Spanned<Operator>,
    pub expr: Spanned<key::Expr>,
}

#[derive(Debug, Clone)]
pub struct ModuleDef {
    pub module: Span,
    pub name: Spanned<Word>,
}

#[derive(Debug, Clone)]
pub struct Use {
    pub use_: Span,
    pub path: Spanned<ItemPathBuf>,
}

#[derive(Clone, Debug)]
pub struct Return {
    pub return_: Span,
    pub expr: Option<key::Expr>,
}

#[derive(Clone, Debug)]
pub struct Lambda {
    pub lambda: Span,
    pub arglist: Vec<LambdaArg>,
    pub body: Vec<Spanned<key::Expr>>,
}

#[derive(Clone, Debug)]
pub struct LambdaArg {
    pub name: Spanned<Word>,
    pub ascription: Option<Spanned<Type>>,
}

#[derive(Clone)]
pub enum Expr {
    Return(Return),

    Let(Let),
    Lambda(Lambda),

    While(While),

    If(If),
    Cond(Match),

    Ref(Unary),
    Deref(Unary),

    Word(Word),
    Path(ItemPathBuf),

    Literal(Literal),
    FieldAccess(FieldAccess),
    Add(Binary),
    Sub(Binary),
    Mul(Binary),
    Div(Binary),
    Eq(Binary),
    Assign(Binary),
    Call(Call),

    Static(Static),
    Const(Const),
}

impl std::fmt::Debug for Expr {
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
    pub callee: Spanned<key::Expr>,
    pub args: Vec<Spanned<key::Expr>>,
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub left: Spanned<key::Expr>,
    pub op: Spanned<Operator>,
    pub right: Spanned<key::Expr>,
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub op: Spanned<Operator>,
    pub expr: Spanned<key::Expr>,
}

#[derive(Debug, Clone)]
pub struct Generics {
    pub tys: Vec<Generic>,
}

#[derive(Debug, Clone)]
pub struct Generic {
    pub ty: Spanned<Word>,
    pub constraints: Vec<Spanned<ItemPathBuf>>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct GenericParams {
    pub tys: Vec<Spanned<Type>>,
}

#[derive(Debug, Clone)]
pub struct FieldAccess {
    pub reciever: Spanned<key::Expr>,
    pub field: Spanned<Word>,
}

#[derive(Debug, Clone)]
pub struct Static {
    pub static_: Span,
    pub name: Spanned<Word>,
    pub signature: Spanned<Type>,
    pub assign: Spanned<Operator>,
    pub expr: Spanned<key::Expr>,
}

#[derive(Debug, Clone)]
pub struct FuncSignature {
    pub ins: Vec<NameTypePair>,
    pub return_: Option<Spanned<Type>>,
}

#[derive(Debug, Clone)]
pub struct NameTypePair {
    pub name: Spanned<Word>,
    pub ty: Spanned<Type>,
}

#[derive(Debug, Clone)]
pub struct FieldValuePair {
    pub name: Spanned<Word>,
    pub sep: Spanned<Operator>,
    pub expr: Spanned<key::Expr>,
}

#[derive(Debug, Clone)]
pub struct While {
    pub while_: Span,
    pub cond: Spanned<key::Expr>,
    pub body: Vec<Spanned<key::Expr>>,
}

#[derive(Debug, Clone)]
pub struct If {
    pub if_: Span,
    pub cond: Spanned<key::Expr>,
    pub truth: Vec<Spanned<key::Expr>>,
    pub lie: Option<Else>,
}

#[derive(Debug, Clone)]
pub struct Else {
    pub else_: Span,
    pub body: Vec<Spanned<key::Expr>>,
}

#[derive(Debug, Clone)]
pub struct Match {
    pub match_: Span,
    pub expr: Spanned<key::Expr>,
    pub branches: Vec<Spanned<MatchBranch>>,
}

#[derive(Debug, Clone)]
pub struct MatchBranch {
    pub pat: Spanned<key::Expr>,
    pub fat_arrow: Spanned<Operator>,
    pub body: Vec<Spanned<key::Expr>>,
}

#[derive(Debug, Clone)]
pub struct Let {
    pub let_: Span,
    pub pat: Spanned<Word>,
    pub ascription: Option<Spanned<Type>>,
    pub assign: Spanned<Operator>,
    pub body: Spanned<key::Expr>,
}
#[derive(Clone)]
pub enum Literal {
    Bool(bool),
    Int(i64),
    UInt(u64),
    String(Intern<String>),
    Char(char),
    Struct(StructLiteral),
    Tuple(Vec<Spanned<key::Expr>>),
    Array(Vec<Spanned<key::Expr>>),
}

impl std::fmt::Debug for Literal {
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

#[derive(Clone, Eq, PartialEq)]
pub enum Type {
    Ptr(Box<Self>),
    Primitive(Primitive),
    Custom(Custom),
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.type_name().fmt(f)
    }
}

impl Type {
    pub fn type_name(&self) -> ItemPathBuf {
        use std::fmt::Write;
        match self {
            Type::Ptr(box ty) => {
                let mut name = ty.type_name();
                if let Some(s) = name.segment_mut(0) {
                    *s = format!("&>{s}").into();
                }
                name
            }
            Type::Primitive(p) => match p {
                Primitive::Void => path!(void),
                Primitive::Bool => path!(bool),
                Primitive::Char => path!(char),
                Primitive::U64 => path!(u64),
                Primitive::U32 => path!(u32),
                Primitive::U16 => path!(u16),
                Primitive::U8 => path!(u8),
                Primitive::I64 => path!(i64),
                Primitive::I32 => path!(i32),
                Primitive::I16 => path!(i16),
                Primitive::I8 => path!(i8),
            },
            Type::Custom(Custom { name, params }) => {
                let mut base = name.clone();
                if let Some(params) = params {
                    let mut paramstr = String::from("[");
                    for (i, param) in params.inner.tys.iter().enumerate() {
                        if i == 0 {
                            write!(paramstr, "{:?}", param.inner.type_name()).unwrap();
                        } else {
                            write!(paramstr, " {:?}", param.inner.type_name()).unwrap();
                        }
                    }
                    write!(paramstr, "]").unwrap();
                    base.push(paramstr.into());
                }
                base
            }
        }
    }
}

#[derive(Clone, Eq, PartialEq)]
pub struct Custom {
    pub name: ItemPathBuf,
    pub params: Option<Spanned<GenericParams>>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Primitive {
    Void,

    Bool,
    Char,

    U64,
    U32,
    U16,
    U8,

    I64,
    I32,
    I16,
    I8,
}

impl Primitive {
    pub fn size(&self) -> usize {
        match self {
            Primitive::Void => 0,
            Primitive::Bool => 1,
            Primitive::Char => 1,
            Primitive::U64 => 8,
            Primitive::U32 => 4,
            Primitive::U16 => 2,
            Primitive::U8 => 1,
            Primitive::I64 => 8,
            Primitive::I32 => 4,
            Primitive::I16 => 2,
            Primitive::I8 => 1,
        }
    }
}
