use crate::{
    ast::Keyword,
    ast::{
        self, Cast, Expr, ItemPathBuf, Literal, ProcSignature, ResolvedFile, ResolvedItem,
        ResolvedStruct, Word,
    },
    types::{StructIndex, Type},
};
use fnv::FnvHashMap;
use smol_str::SmolStr;
use somok::{Either, PartitionThree, Somok, Ternary};
use spanner::{Span, Spanned};

#[derive(Debug, Clone)]
pub enum TopLevel {
    Ref(ItemPathBuf),
    Proc(Proc),
    Const(Const),
    Mem(Mem),
    Var(Var),
}
impl TopLevel {
    pub fn as_proc(&self) -> Option<&Proc> {
        if let Self::Proc(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_const(&self) -> Option<&Const> {
        if let Self::Const(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_var(&self) -> Option<&Var> {
        if let Self::Var(v) = self {
            Some(v)
        } else {
            None
        }
    }
    pub fn span(&self) -> Span {
        match self {
            TopLevel::Ref(_) => todo!(),
            TopLevel::Proc(p) => p.span,
            TopLevel::Const(c) => c.span,
            TopLevel::Mem(m) => m.span,
            TopLevel::Var(v) => v.span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Var {
    pub ty: Spanned<Type>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Proc {
    pub generics: Vec<SmolStr>,
    pub ins: Vec<Spanned<Type>>,
    pub outs: Vec<Spanned<Type>>,
    pub body: Vec<Spanned<Hir>>,
    pub span: Span,
    pub vars: FnvHashMap<ItemPathBuf, Var>,
}

#[derive(Debug, Clone)]
pub struct Const {
    pub outs: Vec<Spanned<Type>>,
    pub body: Vec<Spanned<Hir>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Mem {
    pub body: Vec<Spanned<Hir>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Hir {
    Path(ItemPathBuf),
    Intrinsic(Intrinsic),
    Bind(Bind),
    While(While),
    If(If),
    Cond(Cond),
    Literal(Literal),
    IgnorePattern,
    Return,
    FieldAccess(FieldAccess),
}
#[derive(Debug, Clone)]
pub struct FieldAccess {
    pub field: SmolStr,
}
#[derive(Clone)]
pub struct If {
    pub truth: Vec<Spanned<Hir>>,
    pub lie: Option<Vec<Spanned<Hir>>>,
}

impl std::fmt::Debug for If {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            writeln!(f, "If {{")?;
            writeln!(f, "\t{:#?}", &self.truth)?;
            if let Some(lie) = &self.lie {
                writeln!(f, "\t{:#?}", lie)?;
            }
            writeln!(f, "}}")
        } else {
            write!(f, "If {{")?;
            write!(f, "\t{:?}", &self.truth)?;
            if let Some(lie) = &self.lie {
                write!(f, "\t{:?}", lie)?;
            }
            write!(f, "}}")
        }
    }
}

#[derive(Debug, Clone)]
pub struct Cond {
    pub branches: Vec<CondBranch>,
}

#[derive(Debug, Clone)]
pub struct CondBranch {
    pub pattern: Spanned<Hir>,
    pub body: Vec<Spanned<Hir>>,
}

#[derive(Debug, Clone)]
pub struct Bind {
    pub bindings: Vec<Spanned<Binding>>,
    pub body: Vec<Spanned<Hir>>,
}
#[derive(Debug, Clone)]
pub struct While {
    pub cond: Vec<Spanned<Hir>>,
    pub body: Vec<Spanned<Hir>>,
}

#[derive(Debug, Clone)]
pub enum Binding {
    Ignore,
    Bind { name: ItemPathBuf, ty: Option<Type> },
}

#[derive(Debug, Clone)]
pub enum Intrinsic {
    Drop,
    Dup,
    Swap,
    Over,

    Cast(Spanned<Type>),

    ReadU64,
    ReadU8,
    WriteU64,
    WriteU8,

    CompStop,
    Dump,
    Print,

    Syscall0,
    Syscall1,
    Syscall2,
    Syscall3,
    Syscall4,
    Syscall5,
    Syscall6,

    Argc,
    Argv,

    Add,
    Sub,
    Divmod,
    Mul,

    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Default)]
pub struct Walker {
    proc_vars: FnvHashMap<ItemPathBuf, Var>,
    current_path: ItemPathBuf,
    items: FnvHashMap<ItemPathBuf, TopLevel>,
    types: StructIndex,
}

impl Walker {
    pub fn new() -> Self {
        Self {
            proc_vars: Default::default(),
            current_path: Default::default(),
            items: Default::default(),
            types: Default::default(),
        }
    }

    fn intrinsic(&mut self, ast: &Spanned<Expr>) -> Option<Spanned<Hir>> {
        let word = match &ast.inner {
            Expr::Cast(Cast {
                cast: _,
                ty: Spanned { span, inner: ty },
            }) => {
                return Spanned {
                    span: ast.span,
                    inner: Hir::Intrinsic(Intrinsic::Cast(Spanned {
                        span: *span,
                        inner: ty.clone(),
                    })),
                }
                .some()
            }
            Expr::CompStop => {
                return Spanned {
                    span: ast.span,
                    inner: Hir::Intrinsic(Intrinsic::CompStop),
                }
                .some()
            }
            Expr::Path(p) => p.only()?,
            Expr::Word(Word(w)) => w.as_str(),
            _ => return None,
        };
        let intrinsic = match word {
            "drop" => Intrinsic::Drop,
            "dup" => Intrinsic::Dup,
            "swap" => Intrinsic::Swap,
            "over" => Intrinsic::Over,

            "@u64" => Intrinsic::ReadU64,
            "@u8" => Intrinsic::ReadU8,
            "!u64" => Intrinsic::WriteU64,
            "!u8" => Intrinsic::WriteU8,

            "&?" => Intrinsic::Dump,
            "print" => Intrinsic::Print,

            "syscall0" => Intrinsic::Syscall0,
            "syscall1" => Intrinsic::Syscall1,
            "syscall2" => Intrinsic::Syscall2,
            "syscall3" => Intrinsic::Syscall3,
            "syscall4" => Intrinsic::Syscall4,
            "syscall5" => Intrinsic::Syscall5,
            "syscall6" => Intrinsic::Syscall6,

            "argc" => Intrinsic::Argc,
            "argv" => Intrinsic::Argv,

            "+" => Intrinsic::Add,
            "-" => Intrinsic::Sub,
            "*" => Intrinsic::Mul,
            "divmod" => Intrinsic::Divmod,

            "=" => Intrinsic::Eq,
            "!=" => Intrinsic::Ne,
            "<" => Intrinsic::Lt,
            "<=" => Intrinsic::Le,
            ">" => Intrinsic::Gt,
            ">=" => Intrinsic::Ge,
            _ => return None,
        };
        Spanned {
            span: ast.span,
            inner: Hir::Intrinsic(intrinsic),
        }
        .some()
    }

    pub fn walk_ast(
        mut self,
        ast: ResolvedFile,
    ) -> (FnvHashMap<ItemPathBuf, TopLevel>, StructIndex) {
        self.walk_module(ast);
        (self.items, self.types)
    }

    fn walk_module(&mut self, module: ResolvedFile) {
        let (items, structs, modules) = module
            .ast
            .into_iter()
            .partition_three::<FnvHashMap<_, _>, _>(|(_, item)| match &**item {
                ResolvedItem::Ref(_) => Ternary::First,
                ResolvedItem::Proc(_) => Ternary::First,
                ResolvedItem::Const(_) => Ternary::First,
                ResolvedItem::Mem(_) => Ternary::First,
                ResolvedItem::Var(_) => Ternary::First,
                ResolvedItem::Struct(_) => Ternary::Second,
                ResolvedItem::Module(_) => Ternary::Third,
            });
        let modules = modules
            .into_iter()
            .map(|(n, m)| {
                (
                    n,
                    match m.inner {
                        ResolvedItem::Module(box m) => m,
                        _ => unreachable!(),
                    },
                )
            })
            .collect::<FnvHashMap<_, _>>();
        let structs = structs
            .into_iter()
            .map(|(n, m)| {
                (
                    n,
                    match m.inner {
                        ResolvedItem::Struct(s) => s,
                        _ => unreachable!(),
                    },
                )
            })
            .collect::<FnvHashMap<_, _>>();
        for (_, module) in modules {
            self.walk_module(module);
        }
        self.current_path = module.path;
        for (_, struct_) in structs {
            self.walk_struct(struct_);
        }
        for (name, item) in items {
            let item = self.walk_toplevel(item.inner);
            let name = self.current_path.child(name);
            self.items.insert(name, item);
        }
    }

    fn walk_struct(&mut self, struct_: ResolvedStruct) {
        let ResolvedStruct { name, fields } = struct_;
        let Word(name) = name.inner;
        let name = self.current_path.child(name);
        let mut builder = self.types.new_struct(name);
        for (name, ty) in fields {
            let span = ty.ty.span;
            let ty = ty.inner.ty.inner;
            let ty = match ty {
                ty @ Type::Primitive(_) | ty @ Type::Ptr(_) => ty,
                Type::Custom(type_name) => {
                    let type_name = self.current_path.join(&type_name);
                    Type::Custom(type_name)
                }
            };
            let ty = Spanned { span, inner: ty };
            builder.field(name, ty);
        }
        builder.finish();
    }

    fn walk_toplevel(&mut self, item: ResolvedItem) -> TopLevel {
        match item {
            ResolvedItem::Ref(p) => TopLevel::Ref(p),
            ResolvedItem::Proc(p) => TopLevel::Proc(self.walk_proc(p)),
            ResolvedItem::Const(c) => TopLevel::Const(self.walk_const(c)),
            ResolvedItem::Mem(m) => TopLevel::Mem(self.walk_mem(m)),
            ResolvedItem::Var(v) => TopLevel::Var(Var {
                ty: v.ty,
                span: v.name.span,
            }),
            ResolvedItem::Struct(_) => unreachable!(),
            ResolvedItem::Module(_) => unreachable!(),
        }
    }

    fn walk_mem(&mut self, mem: ast::Mem) -> Mem {
        let body = mem
            .body
            .into_iter()
            .map(|ast| self.walk_node(ast).unwrap())
            .collect::<Vec<_>>();
        Mem {
            body,
            span: mem.mem.span.merge(mem.end.span),
        }
    }

    fn walk_const(&mut self, const_: ast::Const) -> Const {
        let outs = const_.signature.inner.tys;
        let body = const_
            .body
            .into_iter()
            .map(|ast| self.walk_node(ast).unwrap())
            .collect::<Vec<_>>();
        Const {
            outs,
            body,
            span: const_.const_.span.merge(const_.end.span),
        }
    }

    fn walk_proc(&mut self, proc: ast::Proc) -> Proc {
        let generics = proc
            .generics
            .map(|g| {
                g.inner.tys.into_iter().map(|ty| {
                    let Word(ty) = ty.inner;
                    ty
                })
            })
            .into_iter()
            .flatten()
            .collect::<Vec<_>>();

        let ProcSignature { ins, sep: _, outs } = proc.signature.inner;
        let ins = ins.into_iter().collect();
        let outs = outs
            .map(|i| i.into_iter().collect())
            .unwrap_or_else(Vec::new);

        let body = self.walk_body(proc.body);
        let mut vars = Default::default();
        std::mem::swap(&mut vars, &mut self.proc_vars);

        Proc {
            ins,
            outs,
            generics,
            body,
            vars,
            span: proc.proc.span.merge(proc.end.span),
        }
    }

    fn walk_body(&mut self, body: Vec<Spanned<Expr>>) -> Vec<Spanned<Hir>> {
        body.into_iter()
            .filter_map(|ast| self.walk_node(ast))
            .collect::<Vec<_>>()
    }

    fn walk_node(&mut self, node: Spanned<Expr>) -> Option<Spanned<Hir>> {
        if let Some(node) = self.intrinsic(&node) {
            return node.some();
        }
        let hir = match node.inner {
            Expr::Bind(bind) => Hir::Bind(self.walk_bind(bind)),
            Expr::While(while_) => Hir::While(self.walk_while(while_)),
            Expr::If(if_) => Hir::If(self.walk_if(if_)),
            Expr::Cond(box cond) => Hir::Cond(self.walk_cond(cond)),
            Expr::Cast(_) => unreachable!(),
            Expr::Word(Word(w)) => Hir::Path(self.current_path.child(w)),
            Expr::Literal(l) => Hir::Literal(l),
            Expr::Keyword(Keyword::Return) => Hir::Return,
            Expr::Var(var) => {
                self.walk_var(var);
                return None;
            }
            Expr::FieldAccess(box access) => {
                let Word(field) = access.field.inner;
                let access = FieldAccess { field };
                Hir::FieldAccess(access)
            }
            Expr::Path(p) => Hir::Path(self.current_path.join(&p)),
            expr => unreachable!("{:?}", expr),
        };
        Spanned {
            span: node.span,
            inner: hir,
        }
        .some()
    }

    fn walk_var(&mut self, var: ast::Var) {
        let name = var.name.map(|Word(w)| self.current_path.child(w));
        let ty = var.ty;
        let var = Var {
            ty,
            span: name.span,
        };
        self.proc_vars.insert(name.inner, var);
    }

    fn walk_bind(&mut self, bind: ast::Bind) -> Bind {
        let bindings = bind
            .bindings
            .into_iter()
            .map(|b| {
                b.map(|b| match b {
                    Either::Left(Word(w)) if w == "_" => Binding::Ignore,
                    Either::Left(Word(w)) => Binding::Bind {
                        name: self.current_path.child(w),
                        ty: None,
                    },
                    Either::Right(r) => Binding::Bind {
                        name: r.name.map(|Word(w)| self.current_path.child(w)).inner,
                        ty: Some(r.ty.inner),
                    },
                })
            })
            .collect();
        let body = bind
            .body
            .into_iter()
            .filter_map(|node| self.walk_node(node))
            .collect();
        Bind { bindings, body }
    }

    fn walk_cond(&mut self, cond: ast::Cond) -> Cond {
        let branches = cond
            .branches
            .into_iter()
            .map(|b| b.map(|b| self.walk_cond_branch(b)).inner)
            .collect();
        Cond { branches }
    }

    fn walk_cond_branch(&mut self, branch: ast::CondBranch) -> CondBranch {
        let pattern = match branch.pat.inner {
            Expr::Word(Word(w)) if w == "_" => Spanned {
                span: branch.pat.span,
                inner: Hir::IgnorePattern,
            },
            Expr::Path(p) => {
                let path = self.current_path.join(&p);
                Spanned {
                    span: branch.pat.span,
                    inner: Hir::Path(path),
                }
            }
            Expr::Word(Word(w)) => {
                let path = self.current_path.child(w);
                Spanned {
                    span: branch.pat.span,
                    inner: Hir::Path(path),
                }
            }
            Expr::Literal(l) => Spanned {
                span: branch.pat.span,
                inner: Hir::Literal(l),
            },
            e => unreachable!("{:?}", e),
        };
        let body = branch
            .body
            .into_iter()
            .filter_map(|node| self.walk_node(node))
            .collect();
        CondBranch { pattern, body }
    }

    fn walk_while(&mut self, while_: ast::While) -> While {
        let cond = while_
            .cond
            .into_iter()
            .filter_map(|node| self.walk_node(node))
            .collect();
        let body = while_
            .body
            .into_iter()
            .filter_map(|node| self.walk_node(node))
            .collect();
        While { cond, body }
    }

    fn walk_if(&mut self, if_: ast::If) -> If {
        let truth = if_
            .truth
            .into_iter()
            .filter_map(|node| self.walk_node(node))
            .collect();
        let lie = if_.lie.map(|lie| {
            lie.body
                .into_iter()
                .filter_map(|node| self.walk_node(node))
                .collect()
        });

        If { truth, lie }
    }
}
