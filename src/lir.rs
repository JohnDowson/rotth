use std::collections::HashMap;

use crate::{
    eval::eval,
    hir::{AstKind, AstNode, Const, IConst, If, Intrinsic, Proc, TopLevel, While},
    span::Span,
};

#[derive(Debug)]
pub enum Op {
    Push(IConst),
    Drop,
    Dup,
    Swap,
    Over,

    Dump,
    Print,

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

    Proc(String),
    ProcEnd,
    Label(String),
    Jump(String),
    JumpF(String),
    JumpT(String),
    Call(String),
    Return,
    Exit,
}
use somok::Somok;
use Op::*;

#[derive(Clone)]
enum ComConst {
    Compiled(IConst),
    NotCompiled(Const),
}

pub struct Compiler {
    label: usize,
    current_name: String,
    result: Vec<Op>,
    consts: HashMap<String, ComConst>,
}

impl Compiler {
    pub fn compile(mut self, items: HashMap<String, (TopLevel, Span, bool)>) -> Vec<Op> {
        let (procs, consts) = items
            .into_iter()
            .partition::<Vec<_>, _>(|(_, (it, _, _))| matches!(it, TopLevel::Proc(_)));
        let procs = procs.into_iter().filter_map(|(name, (proc, _, needed))| {
            if let TopLevel::Proc(proc) = proc {
                if needed {
                    (name, proc).some()
                } else {
                    None
                }
            } else {
                unreachable!()
            }
        });
        let consts = consts
            .into_iter()
            .filter_map(|(name, (const_, _, needed))| {
                if let TopLevel::Const(const_) = const_ {
                    if needed {
                        (name, ComConst::NotCompiled(const_)).some()
                    } else {
                        None
                    }
                } else {
                    unreachable!()
                }
            })
            .collect::<HashMap<_, _>>();
        self.consts = consts;

        self.emit(Call("main".to_string()));

        self.emit(Exit);

        for (name, proc) in procs {
            self.compile_proc(name, proc)
        }

        self.result
    }

    fn compile_proc(&mut self, name: String, proc: Proc) {
        self.label = 0;
        self.current_name = name.clone();
        let label = name;
        self.emit(Proc(label));

        self.compile_body(proc.body);

        self.emit(ProcEnd);
        self.emit(Return);
    }

    fn compile_const(&mut self, name: String) -> IConst {
        let const_ = match self.consts.get(&name) {
            Some(ComConst::Compiled(i)) => return *i,
            Some(ComConst::NotCompiled(c)) => c.clone(),
            None => unreachable!(),
        };
        let Const { body, ty } = const_;
        let mut com = Self::with_consts(self.consts.clone());
        com.compile_body(body.clone());
        com.emit(Exit);
        let ops = com.result;
        let const_ = match eval(ops) {
            Ok(bytes) => IConst::from_ty_bytes(ty, bytes),
            Err(req) => {
                self.compile_const(req);
                let mut com = Self::with_consts(com.consts);
                com.compile_body(body);
                com.emit(Exit);
                let ops = com.result;
                self.consts = com.consts;
                match eval(ops) {
                    Ok(bytes) => IConst::from_ty_bytes(ty, bytes),
                    Err(_) => unreachable!(),
                }
            }
        };

        self.consts.insert(name, ComConst::Compiled(const_));
        const_
    }

    fn compile_body(&mut self, body: Vec<AstNode>) {
        for node in body {
            match node.ast {
                AstKind::Literal(c) => self.emit(Push(c)),
                AstKind::Word(w) if self.is_const(&w) => {
                    let c = self.compile_const(w);
                    self.emit(Push(c))
                }
                AstKind::Word(w) => self.emit(Call(w)),
                AstKind::Intrinsic(i) => match i {
                    Intrinsic::Drop => self.emit(Drop),
                    Intrinsic::Dup => self.emit(Dup),
                    Intrinsic::Swap => self.emit(Swap),
                    Intrinsic::Over => self.emit(Over),

                    Intrinsic::Add => self.emit(Add),
                    Intrinsic::Sub => self.emit(Sub),
                    Intrinsic::Divmod => self.emit(Divmod),
                    Intrinsic::Mul => self.emit(Mul),

                    Intrinsic::Eq => self.emit(Eq),
                    Intrinsic::Ne => self.emit(Ne),
                    Intrinsic::Lt => self.emit(Lt),
                    Intrinsic::Le => self.emit(Le),
                    Intrinsic::Gt => self.emit(Gt),
                    Intrinsic::Ge => self.emit(Ge),

                    Intrinsic::Dump => self.emit(Dump),
                    Intrinsic::Print => self.emit(Print),
                    Intrinsic::CompStop => return,
                },
                AstKind::If(cond) => self.compile_cond(cond),
                AstKind::While(while_) => self.compile_while(while_),
                AstKind::Bind(_) => todo!(),
            }
        }
    }

    fn compile_while(&mut self, while_: While) {
        let cond_label = self.gen_label();
        let end_label = self.gen_label();
        self.emit(Label(cond_label.clone()));
        self.compile_body(while_.cond);
        self.emit(JumpF(end_label.clone()));
        self.compile_body(while_.body);
        self.emit(Jump(cond_label));
        self.emit(Label(end_label))
    }

    fn compile_cond(&mut self, cond: If) {
        let lie_label = self.gen_label();
        let mut end_label = None;
        self.emit(JumpF(lie_label.clone()));

        self.compile_body(cond.truth);
        if cond.lie.is_some() {
            end_label = self.gen_label().some();
            self.emit(Jump(end_label.clone().unwrap()))
        }

        self.emit(Label(lie_label));

        if let Some(lie) = cond.lie {
            self.compile_body(lie);
            self.emit(Label(end_label.unwrap()))
        }
    }

    fn emit(&mut self, op: Op) {
        self.result.push(op)
    }

    fn gen_label(&mut self) -> String {
        let res = format!(".{}{}", self.current_name, self.label);
        self.label += 1;
        res
    }

    pub fn new() -> Self {
        Self {
            label: 0,
            current_name: "".to_string(),
            result: Default::default(),
            consts: Default::default(),
        }
    }
    fn with_consts(consts: HashMap<String, ComConst>) -> Self {
        Self {
            label: 0,
            current_name: "".to_string(),
            result: Default::default(),
            consts,
        }
    }

    fn is_const(&self, w: &str) -> bool {
        self.consts.contains_key(w)
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}
