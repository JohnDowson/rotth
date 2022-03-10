use std::collections::HashMap;

use crate::{
    eval::eval,
    hir::{
        AstKind, AstNode, Bind, Binding, Const, IConst, If, Intrinsic, Proc, TopLevel, Type, While,
    },
    span::Span,
};

#[derive(Debug)]
pub enum Op {
    Push(IConst),
    PushStr(usize),
    Drop,
    Dup,
    Swap,
    Over,

    Bind,
    UseBinding(usize),
    Unbind,

    ReadU8,
    WriteU8,

    Dump,
    Print,
    Syscall3,

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
    Label(String),
    Jump(String),
    JumpF(String),
    JumpT(String),
    Call(String),
    Return,
    Exit,
}
use somok::{Either, Somok};
use Op::*;

#[derive(Clone)]
enum ComConst {
    Compiled(Vec<IConst>),
    NotCompiled(Const),
}

pub struct Compiler {
    label: usize,
    mangle_table: HashMap<String, String>,
    proc_id: usize,
    current_name: String,
    result: Vec<Op>,
    consts: HashMap<String, ComConst>,
    strings: Vec<String>,
    bindings: Vec<Vec<String>>,
}

impl Compiler {
    pub fn compile(
        mut self,
        items: HashMap<String, (TopLevel, Span, bool)>,
    ) -> (Vec<Op>, Vec<String>, HashMap<String, String>) {
        let (procs, consts) = items
            .into_iter()
            .partition::<Vec<_>, _>(|(_, (it, _, _))| matches!(it, TopLevel::Proc(_)));
        let procs = procs
            .into_iter()
            .filter_map(|(name, (proc, _, needed))| {
                if let TopLevel::Proc(proc) = proc {
                    if needed {
                        let mangled = self.mangle_name(name);
                        (mangled, proc).some()
                    } else {
                        None
                    }
                } else {
                    unreachable!()
                }
            })
            .collect::<Vec<_>>();

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

        (self.result, self.strings, self.mangle_table)
    }

    fn compile_proc(&mut self, name: String, proc: Proc) {
        self.label = 0;
        self.current_name = name.clone();
        let label = name;
        self.emit(Proc(label));

        self.compile_body(proc.body);

        self.emit(Return);
    }

    fn compile_const(&mut self, name: String) -> Vec<IConst> {
        let const_ = match self.consts.get(&name) {
            Some(ComConst::Compiled(i)) => return i.clone(),
            Some(ComConst::NotCompiled(c)) => c.clone(),
            None => unreachable!(),
        };
        let Const { body, types } = const_;
        let mut com = Self::with_consts_and_strings(self.consts.clone(), self.strings.clone());
        com.compile_body(body.clone());
        self.consts = com.consts;
        self.strings = com.strings;
        let ops = com.result;
        let mut const_ = Vec::new();
        match eval(ops, &self.strings) {
            Ok(Either::Right(bytes)) => {
                for (ty, bytes) in types.iter().zip(bytes) {
                    match ty {
                        Type::Bool => const_.push(IConst::Bool(bytes == 1)),
                        Type::U64 => const_.push(IConst::U64(bytes)),
                        Type::I64 => const_.push(IConst::I64(bytes as i64)),
                        Type::Ptr => todo!("Figure out a way to support const string literals"),
                    }
                }
            }
            Err(req) => {
                self.compile_const(req);
                let mut com =
                    Self::with_consts_and_strings(self.consts.clone(), self.strings.clone());
                com.compile_body(body);
                com.emit(Exit);
                let ops = com.result;
                self.consts = com.consts;
                self.strings = com.strings;
                match eval(ops, &self.strings) {
                    Ok(Either::Right(bytes)) => {
                        for (ty, bytes) in types.iter().zip(bytes) {
                            match ty {
                                Type::Bool => const_.push(IConst::Bool(bytes == 1)),
                                Type::U64 => const_.push(IConst::U64(bytes)),
                                Type::I64 => const_.push(IConst::I64(bytes as i64)),
                                Type::Ptr => {
                                    todo!("Figure out a way to support const string literals")
                                }
                            }
                        }
                    }
                    _ => unreachable!(),
                }
            }
            Ok(Either::Left(_)) => unreachable!(),
        };

        self.consts.insert(name, ComConst::Compiled(const_.clone()));
        const_
    }

    fn compile_body(&mut self, body: Vec<AstNode>) {
        for node in body {
            match node.ast {
                AstKind::Literal(c) => match c {
                    IConst::Str(s) => {
                        let i = self.strings.len();
                        self.strings.push(s);
                        self.emit(PushStr(i));
                    }
                    _ => self.emit(Push(c)),
                },
                AstKind::Word(w) if self.is_const(&w) => {
                    let c = self.compile_const(w);
                    for c in c {
                        self.emit(Push(c))
                    }
                }
                AstKind::Word(w) if self.is_binding(&w) => {
                    let offset = self
                        .bindings
                        .iter()
                        .flatten()
                        .rev()
                        .position(|s| s == &w)
                        .unwrap();
                    self.emit(UseBinding(offset))
                }
                AstKind::Word(w) => {
                    let mangled = self.mangle_table.get(&w).unwrap().clone();
                    self.emit(Call(mangled))
                }
                AstKind::Intrinsic(i) => match i {
                    Intrinsic::Drop => self.emit(Drop),
                    Intrinsic::Dup => self.emit(Dup),
                    Intrinsic::Swap => self.emit(Swap),
                    Intrinsic::Over => self.emit(Over),

                    Intrinsic::ReadU8 => self.emit(ReadU8),
                    Intrinsic::WriteU8 => self.emit(WriteU8),
                    Intrinsic::PtrAdd => self.emit(Add),
                    Intrinsic::PtrSub => self.emit(Sub),

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
                    Intrinsic::Syscall3 => self.emit(Syscall3),
                    Intrinsic::CompStop => return,
                },
                AstKind::If(cond) => self.compile_cond(cond),
                AstKind::While(while_) => self.compile_while(while_),
                AstKind::Bind(bind) => self.compile_bind(bind),
            }
        }
    }

    fn compile_bind(&mut self, bind: Bind) {
        let mut new_bindings = Vec::new();
        for binding in bind.bindings.iter().rev() {
            match binding {
                Binding::Ignore => self.emit(Drop),
                Binding::Bind { name, ty: _ } => {
                    new_bindings.push(name.clone());
                    self.emit(Bind)
                }
            }
        }
        self.bindings.push(new_bindings);
        self.compile_body(bind.body);
        for binding in bind.bindings.into_iter().rev() {
            match binding {
                Binding::Ignore => (),
                Binding::Bind { name: _, ty: _ } => self.emit(Unbind),
            }
        }
        self.bindings.pop();
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
        let res = format!(".{}_{}", self.current_name, self.label);
        self.label += 1;
        res
    }

    pub fn new() -> Self {
        Self {
            label: 0,
            mangle_table: Default::default(),
            proc_id: 0,
            current_name: "".to_string(),
            result: Default::default(),
            consts: Default::default(),
            strings: Default::default(),
            bindings: Default::default(),
        }
    }
    fn with_consts_and_strings(consts: HashMap<String, ComConst>, strings: Vec<String>) -> Self {
        Self {
            label: 0,
            mangle_table: Default::default(),
            proc_id: 0,
            current_name: "".to_string(),
            result: Default::default(),
            consts,
            strings,
            bindings: Default::default(),
        }
    }

    fn mangle_name(&mut self, name: String) -> String {
        let name_mangled = if name != "main" {
            format!(
                "proc{}_{}",
                self.proc_id,
                name.replace(|p: char| !p.is_ascii_alphabetic(), "")
            )
        } else {
            name.clone()
        };
        self.mangle_table.insert(name, name_mangled.clone());
        self.proc_id += 1;
        name_mangled
    }

    fn is_const(&self, w: &str) -> bool {
        self.consts.contains_key(w)
    }
    fn is_binding(&self, w: &str) -> bool {
        self.bindings.iter().flatten().any(|n| n == w)
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}
