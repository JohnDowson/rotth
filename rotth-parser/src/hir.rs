use std::rc::Rc;

use crate::{
    ast::Keyword,
    ast::{
        self, Cast, Expr, Literal, ProcSignature, ResolvedFile, ResolvedItem, ResolvedStruct, Word,
    },
    types::{Custom, StructIndex, Type},
};
use fnv::FnvHashMap;
use internment::Intern;
use itempath::{ItemPath, ItemPathBuf};
use smol_str::SmolStr;
use somok::{Either, Somok};
use spanner::{Span, Spanned};

#[derive(Debug, Clone)]
pub enum TopLevel {
    Ref(ItemPathBuf),
    Proc(Proc),
    Const(Const),
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
    pub generics: Vec<Intern<String>>,
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
    Intrinsic(Intrinsic<Type>),
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
    pub field: Intern<String>,
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
                writeln!(f, "\t{lie:#?}")?;
            }
            writeln!(f, "}}")
        } else {
            write!(f, "If {{")?;
            write!(f, "\t{:?}", &self.truth)?;
            if let Some(lie) = &self.lie {
                write!(f, "\t{lie:?}")?;
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
pub enum Intrinsic<T> {
    Drop,
    Dup,
    Swap,
    Over,

    Cast(Spanned<T>),

    Read(Spanned<T>),
    Write(Spanned<T>),

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

pub struct Walker {
    proc_vars: FnvHashMap<ItemPathBuf, Var>,
    current_path: ItemPathBuf,
    ast: Rc<ResolvedFile>,
    items: FnvHashMap<ItemPathBuf, TopLevel>,
    types: StructIndex,
}

impl Walker {
    fn new(ast: Rc<ResolvedFile>) -> Self {
        Self {
            proc_vars: Default::default(),
            current_path: Default::default(),
            ast,
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
            Expr::Path(p) => p.first()?,
            Expr::Word(Word(w)) => *w,

            Expr::Read(i) => {
                return Spanned {
                    span: ast.span,
                    inner: Hir::Intrinsic(Intrinsic::Read(i.ty.clone())),
                }
                .some()
            }
            Expr::Write(i) => {
                return Spanned {
                    span: ast.span,
                    inner: Hir::Intrinsic(Intrinsic::Write(i.ty.clone())),
                }
                .some()
            }
            _ => return None,
        };
        let intrinsic = match &**word {
            "drop" => Intrinsic::Drop,
            "dup" => Intrinsic::Dup,
            "swap" => Intrinsic::Swap,
            "over" => Intrinsic::Over,

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

    pub fn walk_ast(ast: ResolvedFile) -> (FnvHashMap<ItemPathBuf, TopLevel>, StructIndex) {
        let ast = Rc::new(ast);
        let mut this = Self::new(ast.clone());
        this.walk_module(ast);
        (this.items, this.types)
    }

    fn walk_module(&mut self, module: Rc<ResolvedFile>) {
        let mp = self.current_path.clone();
        self.current_path = module.path.clone();
        for (
            name,
            Spanned {
                span: _,
                inner: item,
            },
        ) in &module.ast
        {
            self.walk_toplevel(name.clone(), item.clone());
        }
        self.current_path = mp;
    }

    fn walk_struct(&mut self, name: Intern<String>) {
        let path = self.current_path.child(name);
        let struct_ = if let Some(Spanned {
            span: _,
            inner: ResolvedItem::Struct(s),
        }) = self.ast.find(&path)
        {
            s
        } else {
            unreachable!()
        };
        let ResolvedStruct {
            name,
            fields,
            generics,
        } = &*struct_;
        let Word(name) = name.inner.clone();
        let name = self.current_path.child(name);
        let generics = generics.iter().map(|t| t.map_ref(|t| t.clone())).collect();
        let mut builder = self.types.new_struct(name, generics);
        for (name, ty) in fields {
            let span = ty.inner.ty.span;
            let ty = ty.inner.ty.inner.clone();

            fn resolve_field(current_path: &ItemPath, ty: Type) -> Type {
                match ty {
                    ty @ Type::Primitive(_) | ty @ Type::Ptr(_) => ty,
                    Type::Custom(Custom { name, params }) => {
                        let type_name = current_path.join(&name);
                        Type::Custom(Custom {
                            name: type_name,
                            params,
                        })
                    }
                }
            }

            let ty = resolve_field(&self.current_path, ty);
            let ty = Spanned { span, inner: ty };
            builder.field(name.clone(), ty);
        }
        builder.finish();
    }

    fn walk_ref(&mut self, name: Intern<String>, referee: &ItemPath) {
        let path = self.current_path.child(name);
        match self.ast.find(referee) {
            Some(Spanned {
                span: _,
                inner: item,
            }) => match item {
                ResolvedItem::Ref(_) => todo!(),
                ResolvedItem::Module(m) => {
                    for name in m.ast.keys() {
                        let referee = referee.child(name.clone());
                        self.items
                            .insert(path.child(name.clone()), TopLevel::Ref(referee));
                    }
                }
                _ => {
                    self.items.insert(path, TopLevel::Ref(referee.to_owned()));
                }
            },
            None => unreachable!(),
        }
    }

    fn walk_toplevel(&mut self, name: Intern<String>, item: ResolvedItem) {
        match item {
            ResolvedItem::Ref(path) => self.walk_ref(name, &path),
            ResolvedItem::Proc(_) => self.walk_proc(name),
            ResolvedItem::Const(_) => self.walk_const(name),
            ResolvedItem::Var(v) => {
                self.items.insert(
                    self.current_path.child(name),
                    TopLevel::Var(Var {
                        ty: v.ty.clone(),
                        span: v.name.span,
                    }),
                );
            }
            ResolvedItem::Struct(_) => self.walk_struct(name),
            ResolvedItem::Module(module) => self.walk_module(module),
        }
    }

    fn walk_const(&mut self, name: Intern<String>) {
        let path = self.current_path.child(name.clone());
        let const_ = if let Some(Spanned {
            span: _,
            inner: ResolvedItem::Const(c),
        }) = self.ast.find(&path)
        {
            c
        } else {
            unreachable!()
        };
        let outs = const_.signature.inner.tys.clone();
        let body = const_
            .body
            .iter()
            .map(|ast| self.walk_node(ast).unwrap())
            .collect::<Vec<_>>();
        self.items.insert(
            self.current_path.child(name),
            TopLevel::Const(Const {
                outs,
                body,
                span: const_.const_.span.merge(const_.end.span),
            }),
        );
    }

    fn walk_proc(&mut self, name: Intern<String>) {
        let path = self.current_path.child(name);
        let proc = if let Some(Spanned {
            span: _,
            inner: ResolvedItem::Proc(p),
        }) = self.ast.find(&path)
        {
            p
        } else {
            unreachable!()
        };

        let generics = proc
            .generics
            .as_ref()
            .map(|g| {
                g.inner.tys.iter().map(|ty| {
                    let Word(ty) = &ty.inner;
                    ty.clone()
                })
            })
            .into_iter()
            .flatten()
            .collect::<Vec<_>>();

        let ProcSignature { ins, sep: _, outs } = proc.signature.inner.clone();
        let ins = ins.into_iter().collect();
        let outs = outs
            .map(|i| i.into_iter().collect())
            .unwrap_or_else(Vec::new);

        let body = self.walk_body(&proc.body);
        let mut vars = Default::default();
        std::mem::swap(&mut vars, &mut self.proc_vars);

        self.items.insert(
            self.current_path.child(name),
            TopLevel::Proc(Proc {
                ins,
                outs,
                generics,
                body,
                vars,
                span: proc.proc.span.merge(proc.end.span),
            }),
        );
    }

    fn walk_body(&mut self, body: &[Spanned<Expr>]) -> Vec<Spanned<Hir>> {
        body.iter()
            .filter_map(|ast| self.walk_node(ast))
            .collect::<Vec<_>>()
    }

    fn walk_node(&mut self, node: &Spanned<Expr>) -> Option<Spanned<Hir>> {
        if let Some(node) = self.intrinsic(node) {
            return node.some();
        }
        let hir = match &node.inner {
            Expr::Bind(bind) => Hir::Bind(self.walk_bind(bind)),
            Expr::While(while_) => Hir::While(self.walk_while(while_)),
            Expr::If(if_) => Hir::If(self.walk_if(if_)),
            Expr::Cond(box cond) => Hir::Cond(self.walk_cond(cond)),
            Expr::Cast(_) => unreachable!(),
            Expr::Word(Word(w)) => Hir::Path(self.current_path.child(w.clone())),
            Expr::Literal(l) => Hir::Literal(l.clone()),
            Expr::Keyword(Keyword::Return) => Hir::Return,
            Expr::Var(var) => {
                self.walk_var(var);
                return None;
            }
            Expr::FieldAccess(box access) => {
                let Word(field) = access.field.inner.clone();
                let access = FieldAccess { field };
                Hir::FieldAccess(access)
            }
            Expr::Path(p) => Hir::Path(self.current_path.join(p)),
            expr => unreachable!("{:?}", expr),
        };
        Spanned {
            span: node.span,
            inner: hir,
        }
        .some()
    }

    fn walk_var(&mut self, var: &ast::Var) {
        let name = var
            .name
            .map_ref(|Word(w)| self.current_path.child(w.clone()));
        let ty = var.ty.clone();
        let var = Var {
            ty,
            span: name.span,
        };
        self.proc_vars.insert(name.inner, var);
    }

    fn walk_bind(&mut self, bind: &ast::Bind) -> Bind {
        let bindings = bind
            .bindings
            .iter()
            .map(|b| {
                b.map_ref(|b| match b {
                    Either::Left(Word(w)) if **w == "_" => Binding::Ignore,
                    Either::Left(Word(w)) => Binding::Bind {
                        name: self.current_path.child(w.clone()),
                        ty: None,
                    },
                    Either::Right(r) => Binding::Bind {
                        name: r
                            .name
                            .map_ref(|Word(w)| self.current_path.child(w.clone()))
                            .inner,
                        ty: Some(r.ty.inner.clone()),
                    },
                })
            })
            .collect();
        let body = bind
            .body
            .iter()
            .filter_map(|node| self.walk_node(node))
            .collect();
        Bind { bindings, body }
    }

    fn walk_cond(&mut self, cond: &ast::Cond) -> Cond {
        let branches = cond
            .branches
            .iter()
            .map(|b| b.map_ref(|b| self.walk_cond_branch(b)).inner)
            .collect();
        Cond { branches }
    }

    fn walk_cond_branch(&mut self, branch: &ast::CondBranch) -> CondBranch {
        let pattern = match &branch.pat.inner {
            Expr::Word(Word(w)) if **w == "_" => Spanned {
                span: branch.pat.span,
                inner: Hir::IgnorePattern,
            },
            Expr::Path(p) => {
                let path = self.current_path.join(p);
                Spanned {
                    span: branch.pat.span,
                    inner: Hir::Path(path),
                }
            }
            Expr::Word(Word(w)) => {
                let path = self.current_path.child(w.clone());
                Spanned {
                    span: branch.pat.span,
                    inner: Hir::Path(path),
                }
            }
            Expr::Literal(l) => Spanned {
                span: branch.pat.span,
                inner: Hir::Literal(l.clone()),
            },
            e => unreachable!("{:?}", e),
        };
        let body = branch
            .body
            .iter()
            .filter_map(|node| self.walk_node(node))
            .collect();
        CondBranch { pattern, body }
    }

    fn walk_while(&mut self, while_: &ast::While) -> While {
        let cond = while_
            .cond
            .iter()
            .filter_map(|node| self.walk_node(node))
            .collect();
        let body = while_
            .body
            .iter()
            .filter_map(|node| self.walk_node(node))
            .collect();
        While { cond, body }
    }

    fn walk_if(&mut self, if_: &ast::If) -> If {
        let truth = if_
            .truth
            .iter()
            .filter_map(|node| self.walk_node(node))
            .collect();
        let lie = if_.lie.as_ref().map(|lie| {
            lie.body
                .iter()
                .filter_map(|node| self.walk_node(node))
                .collect()
        });

        If { truth, lie }
    }
}
