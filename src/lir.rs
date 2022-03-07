use std::collections::HashMap;

use crate::{
    hir::{AstKind, AstNode, Const, If, Proc, While},
    span::Span,
};

#[derive(Debug)]
pub enum Op {
    Push(Const),
    Pop,
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
    Neq,
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

pub struct Compiler {
    label: usize,
    current_name: String,
    result: Vec<Op>,
}

impl Compiler {
    pub fn compile(mut self, procs: HashMap<String, (Proc, Span, bool)>) -> Vec<Op> {
        self.emit(Call("main".to_string()));

        self.emit(Exit);

        for (name, (proc, _, needed)) in procs {
            if needed {
                self.compile_proc(name, proc);
            }
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

    fn compile_body(&mut self, body: Vec<AstNode>) {
        for node in body {
            match node.ast {
                AstKind::Literal(c) => self.emit(Push(c)),
                AstKind::Word(w) => match w.as_str() {
                    "pop" => self.emit(Pop),
                    "dup" => self.emit(Dup),
                    "swap" => self.emit(Swap),
                    "over" => self.emit(Over),

                    "+" => self.emit(Add),
                    "-" => self.emit(Sub),
                    "%/" => self.emit(Divmod),
                    "*" => self.emit(Sub),

                    "=" => self.emit(Eq),
                    "!=" => self.emit(Neq),
                    "<" => self.emit(Lt),
                    "<=" => self.emit(Le),
                    ">" => self.emit(Gt),
                    ">=" => self.emit(Ge),

                    "&?" => self.emit(Dump),
                    "print" => self.emit(Print),

                    _ => self.emit(Call(w)),
                },
                AstKind::If(cond) => self.compile_cond(cond),
                AstKind::While(while_) => self.compile_while(while_),
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
        let res = format!("{}{}", self.current_name, self.label);
        self.label += 1;
        res
    }

    pub fn new() -> Self {
        Self {
            label: 0,
            current_name: "".to_string(),
            result: Vec::new(),
        }
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}
