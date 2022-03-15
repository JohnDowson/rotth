use fnv::FnvHashMap;
use simplearena::{Heap, Ref};
use somok::Somok;
use std::collections::VecDeque;

use crate::{
    hir::{Binding, CondBranch, HirKind, HirNode, If, Intrinsic, TopLevel},
    iconst::IConst,
    span::Span,
    types::{StructIndex, Type, ValueType},
    Error,
};

#[derive(Debug)]
pub struct TypecheckError {
    pub span: Span,
    pub kind: ErrorKind,
    pub message: String,
}
impl TypecheckError {
    fn new(span: Span, kind: ErrorKind, message: impl ToString) -> TypecheckError {
        TypecheckError {
            span,
            kind,
            message: message.to_string(),
        }
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    TypeMismatch {
        expected: Vec<Type>,
        actual: Vec<Type>,
    },
    NotEnoughData,
    Undefined(String),
    InvalidMain,
    InvalidWhile,
    CompStop,
    Unexpected,
    CallInConst,
}
use ErrorKind::*;
fn error<T>(span: Span, kind: ErrorKind, message: impl ToString) -> Result<T> {
    Error::Typecheck(TypecheckError::new(span, kind, message)).error()
}

pub type Result<T> = std::result::Result<T, Error>;

pub struct Typechecker<'s> {
    items: FnvHashMap<String, (TopLevel, bool)>,
    structs: &'s StructIndex,
    heap: THeap,
}

impl<'s> Typechecker<'s> {
    pub fn typecheck_program(
        items: FnvHashMap<String, TopLevel>,
        structs: &'s StructIndex,
    ) -> Result<FnvHashMap<String, (TopLevel, bool)>> {
        let items = items
            .into_iter()
            .map(|(name, item)| (name, (item, false)))
            .collect();

        let heap = THeap::default();

        let mut this = Self {
            items,
            structs,
            heap,
        };
        this.typecheck_proc("main")?;
        this.items.okay()
    }

    fn typecheck_proc(&mut self, name: &str) -> Result<()> {
        let (proc, typechecked) = self.items.get(name).ok_or_else(|| {
            TypecheckError::new(
                Span::point("".to_string(), 0),
                Undefined(name.to_string()),
                format!("Proc `{}` does not exist", name),
            )
        })?;
        let proc = match proc {
            TopLevel::Proc(p) => p,
            _ => unreachable!("This can't not be proc"),
        };
        if *typechecked {
            return ().okay();
        }
        if name == "main" && (!proc.ins.is_empty() || !(proc.outs[..] == [Type::U64])) {
            return error(
                proc.span.clone(),
                InvalidMain,
                "Main must have no inputs and a single uint output",
            );
        }

        let mut actual = TypeStack::default();
        let mut expected = TypeStack::default();
        for ty in &proc.ins {
            actual.push(&mut self.heap, *ty)
        }
        for ty in &proc.outs {
            expected.push(&mut self.heap, *ty)
        }
        let mut bindings = Vec::new();
        self.typecheck_body(name, &mut proc.body, &mut actual, false, &mut bindings)?;

        if !actual.eq(&expected, &self.heap) {
            error(
                proc.span.clone(),
                TypeMismatch {
                    actual: actual.into_vec(&self.heap),
                    expected: expected.into_vec(&self.heap),
                },
                "Type mismatch: proc body does not equal proc outputs",
            )
        } else {
            let (_, typechecked) = self.items.get_mut(name).unwrap();
            *typechecked = true;
            ().okay()
        }
    }

    fn typecheck_cond(
        &mut self,
        name: &str,
        node: &mut HirNode,
        stack: &mut TypeStack,
        in_const: bool,
        bindings: &mut Vec<Vec<(String, Type)>>,
    ) -> Result<()> {
        let ty = stack.pop(&mut self.heap).ok_or_else(|| {
            TypecheckError::new(node.span.clone(), NotEnoughData, "Not enough data for cond")
        })?;
        let cond = match &mut node.hir {
            HirKind::Cond(c) => c,
            _ => unreachable!(),
        };
        let mut first_branch_stack = TypeStack::default();
        let mut first_branch = true;
        for CondBranch { pattern, body } in &mut cond.branches {
            let pat_ty = match &pattern.hir {
                HirKind::Literal(pat) => match pat {
                    IConst::Bool(_) => Type::BOOL,
                    IConst::U64(_) => Type::U64,
                    IConst::I64(_) => Type::I64,
                    IConst::Char(_) => Type::CHAR,
                    IConst::Str(_) => todo!(),
                    IConst::Ptr(_) => Type::ptr_to(Type::ANY),
                },
                HirKind::Word(const_name) if is_const(const_name, &self.items) => {
                    self.typecheck_const(const_name)?;
                    let const_ = self.items[const_name].0.as_const().ok_or_else(|| {
                        TypecheckError::new(
                            pattern.span.clone(),
                            Unexpected,
                            "Recursive const definition",
                        )
                    })?;
                    if const_.outs.len() != 1 {
                        return error(
                            pattern.span.clone(),
                            Unexpected,
                            "Cond only supports single-value consts",
                        );
                    }
                    const_.outs[0]
                }
                HirKind::Word(_) => {
                    return error(
                        pattern.span.clone(),
                        Unexpected,
                        "Cond only supports constant patterns",
                    )
                }
                HirKind::IgnorePattern => Type::ANY,
                hir => unreachable!("{:?}", hir),
            };
            if !ty.type_eq(&pat_ty) {
                return error(
                    pattern.span.clone(),
                    TypeMismatch {
                        expected: vec![ty],
                        actual: vec![pat_ty],
                    },
                    "Wrong type for cond pattern",
                );
            }
            if first_branch {
                self.typecheck_body(
                    name,
                    &mut *body,
                    &mut first_branch_stack,
                    in_const,
                    bindings,
                )?;
            } else {
                let mut branch_stack = TypeStack::default();
                self.typecheck_body(name, &mut *body, &mut branch_stack, in_const, bindings)?;
                if !first_branch_stack.eq(&branch_stack, &self.heap) {
                    return error(
                        node.span.clone(),
                        TypeMismatch {
                            expected: first_branch_stack.into_vec(&self.heap),
                            actual: branch_stack.into_vec(&self.heap),
                        },
                        "Type mismatch between cond branches",
                    );
                }
            }
            first_branch = false;
        }

        // let mut default_branch_stack = TypeStack::default();
        // typecheck_body(
        //     name,
        //     &cond.other,
        //     &mut default_branch_stack,
        //     heap,
        //     items,
        //     in_const,
        //     bindings,
        // )?;
        // if !first_branch_stack.eq(&default_branch_stack, heap) {
        //     return error(
        //         node.span.clone(),
        //         TypeMismatch {
        //             expected: first_branch_stack.into_vec(heap),
        //             actual: default_branch_stack.into_vec(heap),
        //         },
        //         "Type mismatch between cond branches",
        //     );
        // }

        let first_branch_stack = first_branch_stack.into_vec(&self.heap);
        for ty in first_branch_stack.into_iter() {
            stack.push(&mut self.heap, ty)
        }

        ().okay()
    }

    fn typecheck_const(&mut self, const_name: &str) -> Result<()> {
        let (const_, typechecked) = self.items.get_mut(const_name).ok_or_else(|| {
            TypecheckError::new(
                Span::point("".to_string(), 0),
                Undefined(const_name.to_string()),
                format!("Const `{}` does not exist", const_name),
            )
        })?;
        let const_ = match const_ {
            TopLevel::Const(c) => c,
            _ => unreachable!("This can't not be const"),
        };
        if *typechecked {
            return ().okay();
        }

        let mut heap = THeap::default();
        let mut actual = TypeStack::default();
        let mut expected = TypeStack::default();
        for ty in &const_.outs {
            if ty.is_ptr() {
                return error(
                    Span::point("".to_string(), 0),
                    TypeMismatch {
                        expected: vec![Type::ANY],
                        actual: vec![*ty],
                    },
                    format!("Const `{}` does not exist", const_name),
                );
            }
            expected.push(&mut heap, *ty);
        }
        let mut bindings = Vec::new();
        self.typecheck_body(const_name, const_.body, &mut actual, true, &mut bindings)?;

        if actual.eq(&expected, &heap) {
            let (_, typechecked) = self.items.get_mut(const_name).unwrap();
            *typechecked = true;
            ().okay()
        } else {
            error(
                const_.span.clone(),
                TypeMismatch {
                    expected: const_.outs,
                    actual: actual.into_vec(&heap),
                },
                "Const body does not equal const type",
            )
        }
    }

    fn typecheck_mem(&mut self, mem_name: &str) -> Result<()> {
        let (mem, typechecked) = self.items.get_mut(mem_name).ok_or_else(|| {
            TypecheckError::new(
                Span::point("".to_string(), 0),
                Undefined(mem_name.to_string()),
                format!("Mem `{}` does not exist", mem_name),
            )
        })?;
        let mem = match mem {
            TopLevel::Mem(m) => m,
            _ => unreachable!("This can't not be const"),
        };
        if *typechecked {
            return ().okay();
        }

        let mut heap = THeap::default();
        let mut actual = TypeStack::default();
        let mut expected = TypeStack::default();

        expected.push(&mut heap, Type::U64);

        let mut bindings = Vec::new();
        self.typecheck_body(mem_name, &mut mem.body, &mut actual, true, &mut bindings)?;

        if actual.eq(&expected, &heap) {
            let (_, typechecked) = self.items.get_mut(mem_name).unwrap();
            *typechecked = true;
            ().okay()
        } else {
            error(
                mem.span.clone(),
                TypeMismatch {
                    expected: vec![Type::U64],
                    actual: actual.into_vec(&heap),
                },
                "Mem body must evaluate to U64",
            )
        }
    }

    fn typecheck_if(
        &mut self,
        name: &str,
        if_: &mut If,
        span: &Span,
        stack: &mut TypeStack,
        in_const: bool,
        bindings: &mut Vec<Vec<(String, Type)>>,
    ) -> Result<()> {
        let (mut truth, mut lie) = (stack.clone(), stack.clone());
        self.typecheck_body(name, &mut if_.truth, &mut truth, in_const, bindings)?;
        if let Some(lie_body) = &mut if_.lie {
            self.typecheck_body(name, &mut *lie_body, &mut lie, in_const, bindings)?;
        } else {
            return ().okay();
        }
        if truth.eq(&lie, &self.heap) {
            *stack = truth;
            ().okay()
        } else {
            let (actual, expected) = (truth.into_vec(&self.heap), lie.into_vec(&self.heap));
            error(
                span.clone(),
                TypeMismatch { actual, expected },
                "If branches must leave stack in the same state",
            )
        }
    }

    fn typecheck_boolean(&mut self, stack: &mut TypeStack, node: &HirNode) -> Result<()> {
        let b = stack.pop(&self.heap).ok_or_else(|| {
            TypecheckError::new(
                node.span.clone(),
                NotEnoughData,
                "Not enough data for binary operation",
            )
        })?;
        let a = stack.pop(&self.heap).ok_or_else(|| {
            TypecheckError::new(
                node.span.clone(),
                NotEnoughData,
                "Not enough data for binary operation",
            )
        })?;
        match (a, b) {
            (a, b) if a.type_eq(&b) => stack.push(&mut self.heap, Type::BOOL),
            (a, b) => {
                return error(
                    node.span.clone(),
                    TypeMismatch {
                        actual: vec![b, a],
                        expected: vec![a, a],
                    },
                    "Wrong types for boolean operation",
                )
            }
        }
        ().okay()
    }

    fn typecheck_divmod(&mut self, stack: &mut TypeStack, node: &HirNode) -> Result<()> {
        self.typecheck_binop(stack, node)?;
        stack.push(&mut self.heap, Type::U64);
        ().okay()
    }

    fn typecheck_binop(&mut self, stack: &mut TypeStack, node: &HirNode) -> Result<()> {
        let b = stack.pop(&mut self.heap).ok_or_else(|| {
            TypecheckError::new(
                node.span.clone(),
                NotEnoughData,
                "Not enough data for binary operation",
            )
        })?;
        let a = stack.pop(&mut self.heap).ok_or_else(|| {
            TypecheckError::new(
                node.span.clone(),
                NotEnoughData,
                "Not enough data for binary operation",
            )
        })?;

        if a == Type::U64 && b == Type::U64 {
            stack.push(&mut self.heap, Type::U64)
        } else if a == Type::I64 && b == Type::I64 {
            stack.push(&mut self.heap, Type::I64)
        } else {
            return error(
                node.span.clone(),
                TypeMismatch {
                    actual: vec![b, a],
                    expected: vec![b, b],
                },
                "Wrong types for binary operation, must be 2 operands of type uint|int",
            );
        }

        ().okay()
    }

    fn typecheck_body(
        &mut self,
        name: &str,
        body: &mut [HirNode],
        stack: &mut TypeStack,
        in_const: bool,
        bindings: &mut Vec<Vec<(String, Type)>>,
    ) -> Result<()> {
        for node in body {
            match &mut node.hir {
                HirKind::Literal(c) => match c {
                    IConst::Bool(_) => stack.push(&mut self.heap, Type::BOOL),
                    IConst::U64(_) => stack.push(&mut self.heap, Type::U64),
                    IConst::I64(_) => stack.push(&mut self.heap, Type::I64),
                    IConst::Ptr(_) => stack.push(&mut self.heap, Type::ptr_to(Type::U64)),
                    IConst::Char(_) => stack.push(&mut self.heap, Type::CHAR),
                    IConst::Str(_) => {
                        stack.push(&mut self.heap, Type::U64);
                        stack.push(&mut self.heap, Type::ptr_to(Type::CHAR));
                    }
                },
                HirKind::Cond(_) => self.typecheck_cond(name, node, stack, in_const, bindings)?,
                HirKind::Return => match self.items.get(name) {
                    Some((TopLevel::Proc(p), _)) => {
                        let mut expected = TypeStack::default();
                        for &ty in &p.outs {
                            expected.push(&mut self.heap, ty)
                        }
                        if !expected.eq(stack, &self.heap) {
                            return error(
                                node.span.clone(),
                                TypeMismatch {
                                    expected: p.outs.clone(),
                                    actual: stack.clone().into_vec(&self.heap),
                                },
                                "Type mismatched types for early return",
                            );
                        }
                    }
                    Some(_) => {
                        return error(
                            node.span.clone(),
                            Unexpected,
                            "Return is not allowed in const",
                        )
                    }
                    None => unreachable!(),
                },
                HirKind::Word(w) => match w.as_str() {
                    rec if rec == name => {
                        let proc = &self.items[rec].0.as_proc().ok_or_else(|| {
                            TypecheckError::new(
                                node.span.clone(),
                                Unexpected,
                                "Recursive const definition",
                            )
                        })?;
                        for ty_expected in proc.ins.iter().rev() {
                            let ty_actual = stack.pop(&self.heap).ok_or_else(|| {
                                TypecheckError::new(
                                    node.span.clone(),
                                    NotEnoughData,
                                    format!("Not enough data for proc invocation {}", rec),
                                )
                            })?;
                            if !ty_expected.type_eq(&ty_actual) {
                                return error(
                                    node.span.clone(),
                                    TypeMismatch {
                                        expected: vec![*ty_expected],
                                        actual: vec![ty_actual],
                                    },
                                    format!("Wrong types for proc invocation `{}`", rec),
                                );
                            }
                        }
                        for ty in &proc.outs {
                            stack.push(&mut self.heap, *ty)
                        }
                    }
                    proc_name if is_proc(proc_name, &self.items) => {
                        if in_const {
                            return error(
                                node.span.clone(),
                                CallInConst,
                                "Proc calls not allowed in const context",
                            );
                        }
                        let proc = self.items[proc_name].0.as_proc().ok_or_else(|| {
                            TypecheckError::new(
                                node.span.clone(),
                                Unexpected,
                                "Recursive const definition",
                            )
                        })?;
                        for ty_expected in proc.ins.iter().rev() {
                            let ty_actual = stack.pop(&self.heap).ok_or_else(|| {
                                TypecheckError::new(
                                    node.span.clone(),
                                    NotEnoughData,
                                    format!("Not enough data for proc invocation {}", proc_name),
                                )
                            })?;
                            if !ty_expected.type_eq(&ty_actual) {
                                return error(
                                    node.span.clone(),
                                    TypeMismatch {
                                        expected: vec![*ty_expected],
                                        actual: vec![ty_actual],
                                    },
                                    format!("Wrong types for proc invocation {}", proc_name),
                                );
                            }
                        }
                        self.typecheck_proc(proc_name)?;
                        let proc = self.items[proc_name].0.as_proc().ok_or_else(|| {
                            TypecheckError::new(
                                node.span.clone(),
                                Unexpected,
                                "Recursive const definition",
                            )
                        })?;
                        for ty in &proc.outs {
                            stack.push(&mut self.heap, *ty)
                        }
                    }
                    const_name if is_const(const_name, &self.items) => {
                        self.typecheck_const(const_name)?;
                        let const_ = self.items[const_name].0.as_const().ok_or_else(|| {
                            TypecheckError::new(
                                node.span.clone(),
                                Unexpected,
                                "Recursive const definition",
                            )
                        })?;
                        for ty in &const_.outs {
                            stack.push(&mut self.heap, *ty);
                        }
                    }
                    mem_name if is_mem(mem_name, &self.items) => {
                        self.typecheck_mem(mem_name)?;

                        stack.push(&mut self.heap, Type::ptr_to(Type::U8));
                    }
                    lvar_name if is_local_var(name, lvar_name, &self.items) => {
                        let ty = {
                            let (p, _) = &self.items[name];
                            let p = p.as_proc().unwrap();
                            p.vars[lvar_name].ty
                        };
                        stack.push(&mut self.heap, Type::ptr_to(ty));
                    }
                    gvar_name if is_global_var(gvar_name, &self.items) => {
                        let (gvar, _) = &self.items[gvar_name];
                        let gvar = gvar.as_var().unwrap();
                        stack.push(&mut self.heap, Type::ptr_to(gvar.ty));
                    }
                    binding_name if is_binding(binding_name, bindings) => {
                        let ty = bindings
                            .iter()
                            .rev()
                            .find_map(|bs| {
                                bs.iter().find_map(|b| {
                                    if b.0 == binding_name {
                                        b.1.some()
                                    } else {
                                        None
                                    }
                                })
                            })
                            .unwrap();
                        stack.push(&mut self.heap, ty);
                    }
                    word => {
                        return error(
                            node.span.clone(),
                            Undefined(word.to_string()),
                            "Encountered undefined word".to_string(),
                        )
                    }
                },
                HirKind::Intrinsic(i) => match i {
                    Intrinsic::ReadU64 => {
                        let ty = stack.pop(&self.heap).ok_or_else(|| {
                            TypecheckError::new(
                                node.span.clone(),
                                NotEnoughData,
                                "Not enough data for @u64",
                            )
                        })?;
                        if !ty.is_ptr_to(Type::U64) {
                            return error(
                                node.span.clone(),
                                TypeMismatch {
                                    actual: vec![ty],
                                    expected: vec![Type::ptr_to(Type::U64)],
                                },
                                "Wrong types for @u64",
                            );
                        }
                        stack.push(&mut self.heap, Type::U64)
                    }
                    Intrinsic::ReadU8 => {
                        let ty = stack.pop(&self.heap).ok_or_else(|| {
                            TypecheckError::new(
                                node.span.clone(),
                                NotEnoughData,
                                "Not enough data for @u8",
                            )
                        })?;
                        if !ty.is_ptr_to(Type::U8) {
                            return error(
                                node.span.clone(),
                                TypeMismatch {
                                    actual: vec![ty],
                                    expected: vec![Type::ptr_to(Type::U8)],
                                },
                                "Wrong types for @u8",
                            );
                        }
                        stack.push(&mut self.heap, Type::U8)
                    }
                    Intrinsic::WriteU64 => {
                        let ty = stack.pop(&self.heap).ok_or_else(|| {
                            TypecheckError::new(
                                node.span.clone(),
                                NotEnoughData,
                                "Not enough data for !u64",
                            )
                        })?;
                        let ty_store = stack.pop(&self.heap).ok_or_else(|| {
                            TypecheckError::new(
                                node.span.clone(),
                                NotEnoughData,
                                "Not enough data for !u64",
                            )
                        })?;
                        if !(ty.is_ptr_to(Type::U64) && ty_store == Type::U64) {
                            return error(
                                node.span.clone(),
                                TypeMismatch {
                                    actual: vec![ty, ty_store],
                                    expected: vec![Type::ptr_to(Type::U64), Type::U64],
                                },
                                "Wrong types for !u8",
                            );
                        }
                    }
                    Intrinsic::WriteU8 => {
                        let ty = stack.pop(&self.heap).ok_or_else(|| {
                            TypecheckError::new(
                                node.span.clone(),
                                NotEnoughData,
                                "Not enough data for !u8",
                            )
                        })?;
                        let ty_store = stack.pop(&self.heap).ok_or_else(|| {
                            TypecheckError::new(
                                node.span.clone(),
                                NotEnoughData,
                                "Not enough data for !u8",
                            )
                        })?;
                        if !(ty.is_ptr_to(Type::U8) && ty_store == Type::U8) {
                            return error(
                                node.span.clone(),
                                TypeMismatch {
                                    actual: vec![ty, ty_store],
                                    expected: vec![Type::ptr_to(Type::U8), Type::U8],
                                },
                                "Wrong types for !u8",
                            );
                        }
                    }
                    &mut Intrinsic::Cast(ty) => {
                        if !self.expect_arity(1, stack) {
                            return error(
                                node.span.clone(),
                                NotEnoughData,
                                "Not enough data on the stck for cast operation",
                            );
                        }
                        stack.push(&mut self.heap, ty)
                    }

                    Intrinsic::CompStop => {
                        let types: Vec<_> = stack.clone().into_vec(&self.heap);
                        println!("{:?}", types);
                        return error(node.span.clone(), CompStop, "");
                    }

                    Intrinsic::Syscall0 => {
                        if !self.expect_arity(1, stack) {
                            return error(
                                node.span.clone(),
                                NotEnoughData,
                                "Not enough data for syscall3",
                            );
                        }
                        stack.push(&mut self.heap, Type::U64);
                    }
                    Intrinsic::Syscall1 => {
                        if !self.expect_arity(2, stack) {
                            return error(
                                node.span.clone(),
                                NotEnoughData,
                                "Not enough data for syscall3",
                            );
                        }
                        stack.push(&mut self.heap, Type::U64);
                    }
                    Intrinsic::Syscall2 => {
                        if !self.expect_arity(3, stack) {
                            return error(
                                node.span.clone(),
                                NotEnoughData,
                                "Not enough data for syscall3",
                            );
                        }
                        stack.push(&mut self.heap, Type::U64);
                    }
                    Intrinsic::Syscall3 => {
                        if !self.expect_arity(4, stack) {
                            return error(
                                node.span.clone(),
                                NotEnoughData,
                                "Not enough data for syscall3",
                            );
                        }
                        stack.push(&mut self.heap, Type::U64);
                    }
                    Intrinsic::Syscall4 => {
                        if !self.expect_arity(5, stack) {
                            return error(
                                node.span.clone(),
                                NotEnoughData,
                                "Not enough data for syscall3",
                            );
                        }
                        stack.push(&mut self.heap, Type::U64);
                    }
                    Intrinsic::Syscall5 => {
                        if !self.expect_arity(6, stack) {
                            return error(
                                node.span.clone(),
                                NotEnoughData,
                                "Not enough data for syscall3",
                            );
                        }
                        stack.push(&mut self.heap, Type::U64);
                    }
                    Intrinsic::Syscall6 => {
                        if !self.expect_arity(7, stack) {
                            return error(
                                node.span.clone(),
                                NotEnoughData,
                                "Not enough data for syscall3",
                            );
                        }
                        stack.push(&mut self.heap, Type::U64);
                    }

                    Intrinsic::Argc => {
                        stack.push(&mut self.heap, Type::U64);
                    }
                    Intrinsic::Argv => {
                        stack.push(&mut self.heap, Type::ptr_to(Type::ptr_to(Type::CHAR)));
                    }

                    Intrinsic::Print | Intrinsic::Drop => {
                        stack.pop(&self.heap).ok_or_else(|| {
                            TypecheckError::new(
                                node.span.clone(),
                                NotEnoughData,
                                "Not enough data to pop",
                            )
                        })?;
                    }

                    Intrinsic::Dup => {
                        let ty = stack.pop(&self.heap).ok_or_else(|| {
                            TypecheckError::new(
                                node.span.clone(),
                                NotEnoughData,
                                "Not enough data to dup",
                            )
                        })?;
                        stack.push(&mut self.heap, ty);
                        stack.push(&mut self.heap, ty);
                    }
                    Intrinsic::Swap => {
                        let a = stack.pop(&self.heap).ok_or_else(|| {
                            TypecheckError::new(
                                node.span.clone(),
                                NotEnoughData,
                                "Not enough data to swap",
                            )
                        })?;
                        let b = stack.pop(&self.heap).ok_or_else(|| {
                            TypecheckError::new(
                                node.span.clone(),
                                NotEnoughData,
                                "Not enough data to swap",
                            )
                        })?;
                        stack.push(&mut self.heap, a);
                        stack.push(&mut self.heap, b);
                    }
                    Intrinsic::Over => {
                        let a = stack.pop(&self.heap).ok_or_else(|| {
                            TypecheckError::new(
                                node.span.clone(),
                                NotEnoughData,
                                "Not enough data to over",
                            )
                        })?;
                        let b = stack.pop(&self.heap).ok_or_else(|| {
                            TypecheckError::new(
                                node.span.clone(),
                                NotEnoughData,
                                "Not enough data to over",
                            )
                        })?;
                        stack.push(&mut self.heap, b);
                        stack.push(&mut self.heap, a);
                        stack.push(&mut self.heap, b);
                    }
                    Intrinsic::Add | Intrinsic::Sub | Intrinsic::Mul => {
                        self.typecheck_binop(stack, node)?
                    }
                    Intrinsic::Divmod => self.typecheck_divmod(stack, node)?,
                    Intrinsic::Eq
                    | Intrinsic::Ne
                    | Intrinsic::Lt
                    | Intrinsic::Le
                    | Intrinsic::Gt
                    | Intrinsic::Ge => self.typecheck_boolean(stack, &node)?,
                    Intrinsic::Dump => (),
                },
                HirKind::If(cond) => {
                    let ty = stack.pop(&self.heap).ok_or_else(|| {
                        TypecheckError::new(
                            node.span.clone(),
                            NotEnoughData,
                            "Not enough data for if",
                        )
                    })?;
                    if !ty.type_eq(&Type::BOOL) {
                        return error(
                            node.span.clone(),
                            TypeMismatch {
                                actual: vec![ty],
                                expected: vec![Type::BOOL],
                            },
                            "If expects to consume a bool",
                        );
                    }
                    self.typecheck_if(name, cond, &node.span.clone(), stack, in_const, bindings)?;
                }
                HirKind::While(while_) => {
                    let stack_before = stack.clone().into_vec(&self.heap);
                    self.typecheck_body(name, &mut while_.cond, stack, in_const, bindings)?;
                    let ty = stack.pop(&self.heap).ok_or_else(|| {
                        TypecheckError::new(
                            node.span.clone(),
                            NotEnoughData,
                            "Not enough data for while",
                        )
                    })?;
                    if !ty.type_eq(&Type::BOOL) {
                        return error(
                            node.span.clone(),
                            TypeMismatch {
                                actual: vec![ty],
                                expected: vec![Type::BOOL],
                            },
                            "While expects to consume a bool",
                        );
                    }
                    self.typecheck_body(name, &mut while_.body, stack, in_const, bindings)?;
                    if stack.clone().into_vec(&self.heap) != stack_before {
                        return error(node.span.clone(), InvalidWhile, "Invalid while");
                    }
                }
                HirKind::Bind(bind) => {
                    let mut new_bindings = Vec::new();
                    for binding in bind.bindings.iter().rev() {
                        match binding {
                            Binding::Ignore => {
                                stack.pop(&self.heap).ok_or_else(|| {
                                    TypecheckError::new(
                                        node.span.clone(),
                                        NotEnoughData,
                                        "Not enough data for binding",
                                    )
                                })?;
                            }
                            Binding::Bind { name, ty } => {
                                let actual = stack.pop(&self.heap).ok_or_else(|| {
                                    TypecheckError::new(
                                        node.span.clone(),
                                        NotEnoughData,
                                        "Not enough data for binding",
                                    )
                                })?;
                                if !actual.type_eq(ty) {
                                    return error(
                                        node.span.clone(),
                                        TypeMismatch {
                                            expected: vec![*ty],
                                            actual: vec![actual],
                                        },
                                        "Mismatched types for binding",
                                    );
                                }
                                new_bindings.push((name.clone(), *ty));
                            }
                        }
                    }
                    bindings.push(new_bindings);
                    self.typecheck_body(name, &mut bind.body, stack, in_const, bindings)?;
                }
                HirKind::IgnorePattern => todo!(), // noop
                HirKind::FieldAccess(f) => {
                    dbg! {&f};
                    let ty = stack.pop(&self.heap).ok_or_else(|| {
                        TypecheckError::new(
                            node.span.clone(),
                            NotEnoughData,
                            "Not enough data for field access",
                        )
                    })?;
                    let field = {
                        if let ValueType::Struct(s) = ty.value_type {
                            f.ty = s.some();
                            &self.structs[s].fields[&f.field]
                        } else {
                            return error(
                                node.span.clone(),
                                Unexpected,
                                format!("Expected pointer to struct, got {:?}", ty),
                            );
                        }
                    };
                    dbg! {&f};
                    stack.push(&mut self.heap, Type::ptr_to(field.ty))
                }
            }
        }
        ().okay()
    }

    fn expect_arity(&self, arity: usize, stack: &mut TypeStack) -> bool {
        for _ in 0..arity {
            if stack.pop(&self.heap).is_none() {
                return false;
            }
        }
        true
    }
}

fn is_proc(name: &str, items: &FnvHashMap<String, (TopLevel, bool)>) -> bool {
    matches!(items.get(name), Some((TopLevel::Proc(_), _)))
}
fn is_mem(name: &str, items: &FnvHashMap<String, (TopLevel, bool)>) -> bool {
    matches!(items.get(name), Some((TopLevel::Mem(_), _)))
}
fn is_binding(name: &str, bindings: &[Vec<(String, Type)>]) -> bool {
    bindings.iter().flatten().any(|b| b.0 == name)
}
fn is_const(name: &str, items: &FnvHashMap<String, (TopLevel, bool)>) -> bool {
    matches!(items.get(name), Some((TopLevel::Const(_), _)))
}
fn is_local_var(cur_proc: &str, name: &str, items: &FnvHashMap<String, (TopLevel, bool)>) -> bool {
    items
        .get(cur_proc)
        .and_then(|(proc, _)| proc.as_proc())
        .and_then(|proc| proc.vars.get(name))
        .is_some()
}
fn is_global_var(name: &str, items: &FnvHashMap<String, (TopLevel, bool)>) -> bool {
    matches!(items.get(name), Some((TopLevel::Var(_), _)))
}

#[derive(Clone, Default)]
struct TypeStack {
    top: Option<TRef>,
}

impl TypeStack {
    pub fn push(&mut self, heap: &mut THeap, ty: Type) {
        let frame = TypeFrame {
            ty,
            prev: self.top.clone(),
        };
        self.top = heap.alloc(frame).some();
    }

    pub fn pop(&mut self, heap: &THeap) -> Option<Type> {
        if let Some(top) = self.top.clone() {
            let top = top.deref(heap).unwrap();
            let prev = top.prev.clone();
            self.top = prev;
            top.ty.some()
        } else {
            None
        }
    }

    pub fn eq(&self, other: &Self, heap: &THeap) -> bool {
        let (mut next_left, mut next_right) = (&self.top, &other.top);
        loop {
            match (next_left, next_right) {
                (Some(lhs), Some(rhs)) => {
                    let lhs = lhs.deref(heap).unwrap();
                    let rhs = rhs.deref(heap).unwrap();
                    if !lhs.ty.type_eq(&rhs.ty) {
                        break false;
                    }
                    next_left = &lhs.prev;
                    next_right = &rhs.prev;
                    continue;
                }
                (None, None) => break true,
                _ => break false,
            }
        }
    }

    pub fn into_vec(self, heap: &THeap) -> Vec<Type> {
        let mut res = VecDeque::new();
        let mut next = self.top;
        while let Some(top) = next {
            let top = top.deref(heap).unwrap();
            res.push_front(top.ty);
            next = top.prev.clone()
        }
        res.into()
    }
}

#[test]
fn test() {
    let mut heap = THeap::default();
    let mut stack = TypeStack::default();
    let mut stack2 = TypeStack::default();
    let mut stack3 = TypeStack::default();
    let mut stack4 = TypeStack::default();
    stack.push(&mut heap, Type::I64);
    stack.push(&mut heap, Type::U64);
    stack2.push(&mut heap, Type::I64);
    stack2.push(&mut heap, Type::U64);
    stack3.push(&mut heap, Type::U64);
    stack3.push(&mut heap, Type::I64);
    stack4.push(&mut heap, Type::I64);

    assert!(stack.eq(&stack2, &heap));
    assert!(!stack.eq(&stack3, &heap));
    assert!(!stack.eq(&stack4, &heap));
}

#[derive(Clone)]
struct TypeFrame {
    ty: Type,
    prev: Option<TRef>,
}

type TRef = Ref<TypeFrame, 0>;
type THeap = Heap<TypeFrame, 0>;

#[test]
fn test_typecheck() {
    use super::hir::{HirKind, HirNode, Proc};
    use std::assert_matches::assert_matches;
    let mut procs = [(
        "main".to_string(),
        (
            TopLevel::Proc(Proc {
                ins: vec![],
                outs: vec![Type::U64],
                body: vec![HirNode {
                    span: Span::point("".to_string(), 0),
                    hir: HirKind::Literal(IConst::U64(1)),
                }],
                span: Span::point("".to_string(), 0),
                vars: Default::default(),
            }),
            false,
        ),
    )]
    .into_iter()
    .collect();
    assert_matches!(
        typecheck_proc("main", &mut procs, &StructIndex::default()),
        Ok(())
    );
}
