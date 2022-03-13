use crate::{
    ast::{self, AstKind, AstNode, Cast},
    iconst::IConst,
    lexer::KeyWord,
    span::Span,
    types::Type,
};
use somok::Somok;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum TopLevel {
    Proc(Proc),
    Const(Const),
    Mem(Mem),
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
}

#[derive(Debug, Clone)]
pub struct Proc {
    pub ins: Vec<Type>,
    pub outs: Vec<Type>,
    pub body: Vec<HirNode>,
    pub span: Span,
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

fn intrinsic(ast: &AstNode) -> Option<HirNode> {
    let intrinsic = match &ast.ast {
        AstKind::Cast(Cast {
            cast: _,
            ty:
                box AstNode {
                    span: _,
                    ast: AstKind::Type(ty),
                },
        }) => Intrinsic::Cast(*ty),
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

fn hir_bindings(bindings: Vec<AstNode>) -> Vec<Binding> {
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
                } => res.push(Binding::Bind { name, ty }),
                _ => unreachable!(),
            }
        } else {
            unreachable!()
        }
    }
    res
}

pub fn hir_for_ast(ast: HashMap<String, ast::TopLevel>) -> HashMap<String, TopLevel> {
    ast.into_iter()
        .map(|(name, item)| (name, item.into()))
        .collect()
}

impl From<ast::TopLevel> for TopLevel {
    fn from(item: ast::TopLevel) -> Self {
        match item {
            ast::TopLevel::Proc(p) => TopLevel::Proc(p.into()),
            ast::TopLevel::Const(c) => TopLevel::Const(c.into()),
            ast::TopLevel::Mem(m) => TopLevel::Mem(m.into()),
            _ => unreachable!(),
        }
    }
}

impl From<ast::Mem> for Mem {
    fn from(mem: ast::Mem) -> Self {
        let body = coerce_ast!(mem.body => Body || unreachable!())
            .into_iter()
            .map(|ast| Option::<HirNode>::from(ast).unwrap())
            .collect::<Vec<_>>();
        Mem {
            body,
            span: mem.mem.span.merge(mem.end.span),
        }
    }
}

impl From<ast::Const> for Const {
    fn from(const_: ast::Const) -> Self {
        let outs = coerce_ast!(const_.signature => ConstSignature || unreachable!())
            .tys
            .into_iter()
            .map(|ty| coerce_ast!(ty => Type || unreachable!()))
            .collect();
        let body = coerce_ast!(const_.body => Body || unreachable!())
            .into_iter()
            .map(|ast| Option::<HirNode>::from(ast).unwrap())
            .collect::<Vec<_>>();
        Const {
            outs,
            body,
            span: const_.const_.span.merge(const_.end.span),
        }
    }
}

impl From<ast::Proc> for Proc {
    fn from(proc: ast::Proc) -> Self {
        let (ins, outs) = match proc.signature.ast {
            AstKind::ProcSignature(signature) => signature.into(),
            _ => unreachable!(),
        };

        let body: Option<_> = proc.body.into();

        Proc {
            ins,
            outs,
            body: body.unwrap(),
            span: proc.proc.span.merge(proc.end.span),
        }
    }
}

impl From<AstNode> for Option<Vec<HirNode>> {
    fn from(node: AstNode) -> Self {
        let body = coerce_ast!(node => Body || None)?;
        body.into_iter()
            .map(|ast| Option::<HirNode>::from(ast).unwrap())
            .collect::<Vec<_>>()
            .some()
    }
}

impl From<AstNode> for Option<HirNode> {
    fn from(node: AstNode) -> Self {
        if let Some(node) = intrinsic(&node) {
            return node.some();
        }
        let hir = match node.ast {
            AstKind::Bind(bind) => HirKind::Bind(bind.into()),
            AstKind::While(while_) => HirKind::While(while_.into()),
            AstKind::If(if_) => HirKind::If(if_.into()),
            AstKind::Cond(cond) => HirKind::Cond(cond.into()),
            AstKind::Cast(_) => unreachable!(),
            AstKind::Word(w) => HirKind::Word(w),
            AstKind::Literal(l) => HirKind::Literal(l),
            AstKind::KeyWord(KeyWord::Return) => HirKind::Return,
            shouldnt_happen => {
                eprintln!("{:?}", shouldnt_happen);
                return None;
            }
        };
        HirNode {
            span: node.span,
            hir,
        }
        .some()
    }
}

impl From<ast::Bind> for Bind {
    fn from(bind: ast::Bind) -> Self {
        let bindings = hir_bindings(bind.bindings);
        let body = coerce_ast!(bind.body => Body || unreachable!())
            .into_iter()
            .map(|node| Option::<HirNode>::from(node).unwrap())
            .collect();
        Bind { bindings, body }
    }
}

impl From<ast::Cond> for Cond {
    fn from(cond: ast::Cond) -> Self {
        let branches = cond.branches.into_iter().map(|b| b.into()).collect();
        Cond { branches }
    }
}

impl From<ast::CondBranch> for CondBranch {
    fn from(branch: ast::CondBranch) -> Self {
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
            .map(|node| Option::<HirNode>::from(node).unwrap())
            .collect();
        CondBranch { pattern, body }
    }
}

impl From<ast::While> for While {
    fn from(while_: ast::While) -> Self {
        let cond = coerce_ast!(while_.cond => Body || unreachable!())
            .into_iter()
            .map(|node| Option::<HirNode>::from(node).unwrap())
            .collect();
        let body = coerce_ast!(while_.body => Body || unreachable!())
            .into_iter()
            .map(|node| Option::<HirNode>::from(node).unwrap())
            .collect();
        While { cond, body }
    }
}

impl From<ast::If> for If {
    fn from(if_: ast::If) -> Self {
        let truth = coerce_ast!(if_.truth => Body || unreachable!())
            .into_iter()
            .map(|node| Option::<HirNode>::from(node).unwrap())
            .collect();
        let lie = if_.lie.map(|lie| {
            coerce_ast!(lie.body => Body || unreachable!())
                .into_iter()
                .map(|node| Option::<HirNode>::from(node).unwrap())
                .collect()
        });

        If { truth, lie }
    }
}

impl From<ast::ProcSignature> for (Vec<Type>, Vec<Type>) {
    fn from(signature: ast::ProcSignature) -> Self {
        let mut ins = Vec::with_capacity(signature.ins.len());
        for ty in signature.ins {
            if let AstKind::Type(ty) = ty.ast {
                ins.push(ty);
            } else {
                unreachable!();
            }
        }
        let outs = if let Some(outs) = signature.outs {
            let mut proc_outs = Vec::with_capacity(outs.len());
            for ty in outs {
                if let AstKind::Type(ty) = ty.ast {
                    proc_outs.push(ty);
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
