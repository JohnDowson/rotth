use crate::{
    ast::{self, AstKind, AstNode, Cast},
    iconst::IConst,
    lexer::KeyWord,
    span::Span,
    types::{self, StructId, StructIndex, Type},
};
use fnv::FnvHashMap;
use somok::Somok;

#[derive(Debug, Clone)]
pub enum TopLevel {
    Proc(Proc),
    Const(Const),
    Mem(Mem),
    Var(TopLevelVar),
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

    pub fn as_var(&self) -> Option<&TopLevelVar> {
        if let Self::Var(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub struct TopLevelVar {
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Proc {
    pub ins: Vec<Type>,
    pub outs: Vec<Type>,
    pub body: Vec<HirNode>,
    pub span: Span,
    pub vars: FnvHashMap<String, Var>,
}

#[derive(Debug, Clone)]
pub struct Const {
    pub outs: Vec<Type>,
    pub body: Vec<HirNode>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Mem {
    pub body: Vec<HirNode>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct HirNode {
    pub span: Span,
    pub hir: HirKind,
}

#[derive(Debug, Clone)]
pub enum HirKind {
    Word(String),
    Intrinsic(Intrinsic),
    Bind(Bind),
    While(While),
    If(If),
    Cond(Cond),
    Literal(IConst),
    IgnorePattern,
    Return,
    FieldAccess(FieldAccess),
}
#[derive(Debug, Clone)]
pub struct FieldAccess {
    pub ty: Option<StructId>,
    pub field: String,
}
#[derive(Debug, Clone)]
pub struct If {
    pub truth: Vec<HirNode>,
    pub lie: Option<Vec<HirNode>>,
}

#[derive(Debug, Clone)]
pub struct Cond {
    pub branches: Vec<CondBranch>,
}

#[derive(Debug, Clone)]
pub struct CondBranch {
    pub pattern: HirNode,
    pub body: Vec<HirNode>,
}

#[derive(Debug, Clone)]
pub struct Bind {
    pub bindings: Vec<Binding>,
    pub body: Vec<HirNode>,
}
#[derive(Debug, Clone)]
pub struct While {
    pub cond: Vec<HirNode>,
    pub body: Vec<HirNode>,
}

#[derive(Debug, Clone)]
pub enum Binding {
    Ignore,
    Bind { name: String, ty: Type },
}

#[derive(Debug, Clone)]
pub enum Intrinsic {
    Drop,
    Dup,
    Swap,
    Over,

    Cast(Type),

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

#[derive(Debug, Clone)]
pub struct Var {
    pub ty: types::Type,
    pub escaping: bool,
}

pub struct Walker<'s> {
    structs: &'s StructIndex,
    proc_vars: FnvHashMap<String, Var>,
}

impl<'s> Walker<'s> {
    pub fn new(structs: &'s StructIndex) -> Self {
        Self {
            structs,
            proc_vars: Default::default(),
        }
    }

    fn intrinsic(&mut self, ast: &AstNode) -> Option<HirNode> {
        let intrinsic = match &ast.ast {
            AstKind::Cast(Cast {
                cast: _,
                ty:
                    box AstNode {
                        span: _,
                        ast: AstKind::Type(ty),
                    },
            }) => {
                let ty = if let Some(struct_) = ty.clone().to_type(self.structs) {
                    struct_
                } else {
                    todo!()
                };
                Intrinsic::Cast(ty)
            }
            AstKind::Word(ref w) => match w.as_str() {
                "drop" => Intrinsic::Drop,
                "dup" => Intrinsic::Dup,
                "swap" => Intrinsic::Swap,
                "over" => Intrinsic::Over,

                "@u64" => Intrinsic::ReadU64,
                "@u8" => Intrinsic::ReadU8,
                "!u64" => Intrinsic::WriteU64,
                "!u8" => Intrinsic::WriteU8,

                "&?&" => Intrinsic::CompStop,
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
            },
            _ => return None,
        };
        HirNode {
            span: ast.span.clone(),
            hir: HirKind::Intrinsic(intrinsic),
        }
        .some()
    }

    fn hir_bindings(&mut self, bindings: Vec<AstNode>) -> Vec<Binding> {
        let mut res = Vec::with_capacity(bindings.len());
        for binding in bindings {
            if let AstKind::Binding(binding) = binding.ast {
                match binding {
                    ast::Binding::Ignore => res.push(Binding::Ignore),
                    ast::Binding::Bind {
                        name:
                            box AstNode {
                                span: _,
                                ast: AstKind::Word(name),
                            },
                        sep: _,
                        ty:
                            box AstNode {
                                span: _,
                                ast: AstKind::Type(ty),
                            },
                    } => res.push(Binding::Bind {
                        name,
                        ty: ty.to_type(self.structs).unwrap(),
                    }),
                    _ => unreachable!(),
                }
            } else {
                unreachable!()
            }
        }
        res
    }

    pub fn walk_ast(
        &mut self,
        ast: FnvHashMap<String, ast::TopLevel>,
    ) -> FnvHashMap<String, TopLevel> {
        ast.into_iter()
            .map(|(name, item)| (name, self.walk_toplevel(item)))
            .collect()
    }

    fn walk_toplevel(&mut self, item: ast::TopLevel) -> TopLevel {
        match item {
            ast::TopLevel::Proc(p) => TopLevel::Proc(self.walk_proc(p)),
            ast::TopLevel::Const(c) => TopLevel::Const(self.walk_const(c)),
            ast::TopLevel::Mem(m) => TopLevel::Mem(self.walk_mem(m)),
            ast::TopLevel::Var(v) => {
                let ty = coerce_ast!(v.ty => Type || unreachable!())
                    .to_type(self.structs)
                    .unwrap();
                TopLevel::Var(TopLevelVar {
                    ty,
                    span: v.name.span,
                })
            }
            _ => unreachable!(),
        }
    }

    fn walk_mem(&mut self, mem: ast::Mem) -> Mem {
        let body = coerce_ast!(mem.body => Body || unreachable!())
            .into_iter()
            .map(|ast| self.walk_node(ast).unwrap())
            .collect::<Vec<_>>();
        Mem {
            body,
            span: mem.mem.span.merge(mem.end.span),
        }
    }

    fn walk_const(&mut self, const_: ast::Const) -> Const {
        let outs = coerce_ast!(const_.signature => ConstSignature || unreachable!())
            .tys
            .into_iter()
            .map(|ty| {
                coerce_ast!(ty => Type || unreachable!())
                    .to_type(self.structs)
                    .unwrap()
            })
            .collect();
        let body = coerce_ast!(const_.body => Body || unreachable!())
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
                coerce_ast!(g => Generics || unreachable!())
                    .tys
                    .into_iter()
                    .map(|ty| coerce_ast!(ty => Type || unreachable!()))
            })
            .into_iter()
            .flatten()
            .collect::<Vec<_>>();

        let (ins, outs) = match proc.signature.ast {
            AstKind::ProcSignature(signature) => self.walk_proc_signature(signature),
            _ => unreachable!(),
        };

        let body = self.try_walk_body(proc.body);
        let mut vars = Default::default();
        std::mem::swap(&mut vars, &mut self.proc_vars);

        Proc {
            ins,
            outs,
            body: body.unwrap(),
            vars,
            span: proc.proc.span.merge(proc.end.span),
        }
    }

    fn try_walk_body(&mut self, node: AstNode) -> Option<Vec<HirNode>> {
        let body = coerce_ast!(node => Body || None)?;
        body.into_iter()
            .filter_map(|ast| self.walk_node(ast))
            .collect::<Vec<_>>()
            .some()
    }

    fn walk_node(&mut self, node: AstNode) -> Option<HirNode> {
        if let Some(node) = self.intrinsic(&node) {
            return node.some();
        }
        let hir = match node.ast {
            AstKind::Bind(bind) => HirKind::Bind(self.walk_bind(bind)),
            AstKind::While(while_) => HirKind::While(self.walk_while(while_)),
            AstKind::If(if_) => HirKind::If(self.walk_if(if_)),
            AstKind::Cond(cond) => HirKind::Cond(self.walk_cond(cond)),
            AstKind::Cast(_) => unreachable!(),
            AstKind::Word(w) => HirKind::Word(w),
            AstKind::Literal(l) => HirKind::Literal(l),
            AstKind::KeyWord(KeyWord::Return) => HirKind::Return,
            AstKind::Var(box var) => {
                self.walk_var(var);
                return None;
            }
            AstKind::FieldAccess(box access) => {
                let access = FieldAccess {
                    ty: None,
                    field: coerce_ast!(access.field => Word || unreachable!()),
                };
                HirKind::FieldAccess(access)
            }
            shouldnt_happen => todo!("{:?}", shouldnt_happen),
        };
        HirNode {
            span: node.span,
            hir,
        }
        .some()
    }

    fn walk_var(&mut self, var: ast::Var) {
        let name = coerce_ast!(var.name => Word || unreachable!());
        let escaping = var.ret.is_some();
        let ty = coerce_ast!(var.ty => Type || unreachable!())
            .to_type(self.structs)
            .unwrap();
        let var = Var { ty, escaping };
        self.proc_vars.insert(name, var);
    }

    fn walk_bind(&mut self, bind: ast::Bind) -> Bind {
        let bindings = self.hir_bindings(bind.bindings);
        let body = coerce_ast!(bind.body => Body || unreachable!())
            .into_iter()
            .filter_map(|node| self.walk_node(node))
            .collect();
        Bind { bindings, body }
    }

    fn walk_cond(&mut self, cond: ast::Cond) -> Cond {
        let branches = cond
            .branches
            .into_iter()
            .map(|b| self.walk_cond_branch(b))
            .collect();
        Cond { branches }
    }

    fn walk_cond_branch(&mut self, branch: ast::CondBranch) -> CondBranch {
        let pattern = match branch.pat.ast {
            AstKind::Binding(ast::Binding::Ignore) => HirNode {
                span: branch.pat.span,
                hir: HirKind::IgnorePattern,
            },
            AstKind::Literal(l) => HirNode {
                span: branch.pat.span,
                hir: HirKind::Literal(l),
            },
            _ => unreachable!(),
        };
        let body = coerce_ast!(branch.body => Body || unreachable!())
            .into_iter()
            .filter_map(|node| self.walk_node(node))
            .collect();
        CondBranch { pattern, body }
    }

    fn walk_while(&mut self, while_: ast::While) -> While {
        let cond = coerce_ast!(while_.cond => Body || unreachable!())
            .into_iter()
            .filter_map(|node| self.walk_node(node))
            .collect();
        let body = coerce_ast!(while_.body => Body || unreachable!())
            .into_iter()
            .filter_map(|node| self.walk_node(node))
            .collect();
        While { cond, body }
    }

    fn walk_if(&mut self, if_: ast::If) -> If {
        let truth = coerce_ast!(if_.truth => Body || unreachable!())
            .into_iter()
            .filter_map(|node| self.walk_node(node))
            .collect();
        let lie = if_.lie.map(|lie| {
            coerce_ast!(lie.body => Body || unreachable!())
                .into_iter()
                .filter_map(|node| self.walk_node(node))
                .collect()
        });

        If { truth, lie }
    }

    fn walk_proc_signature(&mut self, signature: ast::ProcSignature) -> (Vec<Type>, Vec<Type>) {
        let mut ins = Vec::with_capacity(signature.ins.len());
        for ty in signature.ins {
            if let AstKind::Type(ty) = ty.ast {
                if let Some(struct_) = ty.to_type(self.structs) {
                    ins.push(struct_);
                } else {
                    todo!()
                }
            } else {
                unreachable!();
            }
        }
        let outs = if let Some(outs) = signature.outs {
            let mut proc_outs = Vec::with_capacity(outs.len());
            for ty in outs {
                if let AstKind::Type(ty) = ty.ast {
                    if let Some(struct_) = ty.to_type(self.structs) {
                        proc_outs.push(struct_);
                    } else {
                        todo!()
                    }
                } else {
                    unreachable!();
                }
            }
            proc_outs
        } else {
            Vec::new()
        };

        (ins, outs)
    }
}
