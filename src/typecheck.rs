use std::collections::{HashMap, VecDeque};

use simplearena::{Heap, Ref};
use somok::Somok;

use crate::{
    hir::{Binding, CondBranch, HirKind, HirNode, If, Intrinsic, TopLevel},
    iconst::IConst,
    span::Span,
    types::Type,
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

pub fn typecheck_program(
    items: HashMap<String, TopLevel>,
) -> Result<HashMap<String, (TopLevel, bool)>> {
    let mut items = items
        .into_iter()
        .map(|(name, item)| (name, (item, false)))
        .collect();

    typecheck_proc("main", &mut items)?;
    items.okay()
}

fn typecheck_const(const_name: &str, items: &mut HashMap<String, (TopLevel, bool)>) -> Result<()> {
    let (const_, typechecked) = items
        .get(const_name)
        .ok_or_else(|| {
            TypecheckError::new(
                Span::point("".to_string(), 0),
                Undefined(const_name.to_string()),
                format!("Const `{}` does not exist", const_name),
            )
        })?
        .clone();
    let const_ = match const_ {
        TopLevel::Const(c) => c,
        _ => unreachable!("This can't not be const"),
    };
    if typechecked {
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
    typecheck_body(
        const_name,
        &const_.body,
        &mut actual,
        &mut heap,
        items,
        true,
        &mut bindings,
    )?;

    if actual.eq(&expected, &heap) {
        let (_, typechecked) = items.get_mut(const_name).unwrap();
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

fn typecheck_mem(mem_name: &str, items: &mut HashMap<String, (TopLevel, bool)>) -> Result<()> {
    let (mem, typechecked) = items
        .get(mem_name)
        .ok_or_else(|| {
            TypecheckError::new(
                Span::point("".to_string(), 0),
                Undefined(mem_name.to_string()),
                format!("Mem `{}` does not exist", mem_name),
            )
        })?
        .clone();
    let mem = match mem {
        TopLevel::Mem(m) => m,
        _ => unreachable!("This can't not be const"),
    };
    if typechecked {
        return ().okay();
    }

    let mut heap = THeap::default();
    let mut actual = TypeStack::default();
    let mut expected = TypeStack::default();

    expected.push(&mut heap, Type::U64);

    let mut bindings = Vec::new();
    typecheck_body(
        mem_name,
        &mem.body,
        &mut actual,
        &mut heap,
        items,
        true,
        &mut bindings,
    )?;

    if actual.eq(&expected, &heap) {
        let (_, typechecked) = items.get_mut(mem_name).unwrap();
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

fn typecheck_proc(name: &str, items: &mut HashMap<String, (TopLevel, bool)>) -> Result<()> {
    let (proc, typechecked) = items
        .get(name)
        .ok_or_else(|| {
            TypecheckError::new(
                Span::point("".to_string(), 0),
                Undefined(name.to_string()),
                format!("Proc `{}` does not exist", name),
            )
        })?
        .clone();
    let proc = match proc {
        TopLevel::Proc(p) => p,
        _ => unreachable!("This can't not be proc"),
    };
    if typechecked {
        return ().okay();
    }
    if name == "main" && (!proc.ins.is_empty() || !(proc.outs[..] == [Type::U64])) {
        return error(
            proc.span.clone(),
            InvalidMain,
            "Main must have no inputs and a single uint output",
        );
    }

    let mut heap = THeap::default();
    let mut actual = TypeStack::default();
    let mut expected = TypeStack::default();
    for ty in &proc.ins {
        actual.push(&mut heap, *ty)
    }
    for ty in &proc.outs {
        expected.push(&mut heap, *ty)
    }
    let mut bindings = Vec::new();
    typecheck_body(
        name,
        &proc.body,
        &mut actual,
        &mut heap,
        items,
        false,
        &mut bindings,
    )?;

    if !actual.eq(&expected, &heap) {
        error(
            proc.span.clone(),
            TypeMismatch {
                actual: actual.into_vec(&heap),
                expected: expected.into_vec(&heap),
            },
            "Type mismatch: proc body does not equal proc outputs",
        )
    } else {
        let (_, typechecked) = items.get_mut(name).unwrap();
        *typechecked = true;
        ().okay()
    }
}

fn is_proc(name: &str, items: &HashMap<String, (TopLevel, bool)>) -> bool {
    matches!(items.get(name), Some((TopLevel::Proc(_), _)))
}
fn is_mem(name: &str, items: &HashMap<String, (TopLevel, bool)>) -> bool {
    matches!(items.get(name), Some((TopLevel::Mem(_), _)))
}
fn is_binding(name: &str, bindings: &[Vec<(String, Type)>]) -> bool {
    bindings.iter().flatten().any(|b| b.0 == name)
}
fn is_const(name: &str, items: &HashMap<String, (TopLevel, bool)>) -> bool {
    matches!(items.get(name), Some((TopLevel::Const(_), _)))
}

fn typecheck_body(
    name: &str,
    body: &[HirNode],
    stack: &mut TypeStack,
    heap: &mut THeap,
    items: &mut HashMap<String, (TopLevel, bool)>,
    in_const: bool,
    bindings: &mut Vec<Vec<(String, Type)>>,
) -> Result<()> {
    for node in body {
        match &node.hir {
            HirKind::Literal(c) => match c {
                IConst::Bool(_) => stack.push(heap, Type::BOOL),
                IConst::U64(_) => stack.push(heap, Type::U64),
                IConst::I64(_) => stack.push(heap, Type::I64),
                IConst::Ptr(_) => stack.push(heap, Type::ptr_to(Type::U64)),
                IConst::Char(_) => stack.push(heap, Type::CHAR),
                IConst::Str(_) => {
                    stack.push(heap, Type::U64);
                    stack.push(heap, Type::ptr_to(Type::CHAR));
                }
            },
            HirKind::Cond(_) => typecheck_cond(name, node, stack, heap, items, in_const, bindings)?,
            HirKind::Return => match items.get(name) {
                Some((TopLevel::Proc(p), _)) => {
                    let mut expected = TypeStack::default();
                    for &ty in &p.outs {
                        expected.push(heap, ty)
                    }
                    if !expected.eq(stack, heap) {
                        return error(
                            node.span.clone(),
                            TypeMismatch {
                                expected: p.outs.clone(),
                                actual: stack.clone().into_vec(heap),
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
                    let proc = &items[rec].0.as_proc().ok_or_else(|| {
                        TypecheckError::new(
                            node.span.clone(),
                            Unexpected,
                            "Recursive const definition",
                        )
                    })?;
                    for ty_expected in proc.ins.iter().rev() {
                        let ty_actual = stack.pop(heap).ok_or_else(|| {
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
                        stack.push(heap, *ty)
                    }
                }
                proc_name if is_proc(proc_name, items) => {
                    if in_const {
                        return error(
                            node.span.clone(),
                            CallInConst,
                            "Proc calls not allowed in const context",
                        );
                    }
                    let proc = items[proc_name].0.as_proc().ok_or_else(|| {
                        TypecheckError::new(
                            node.span.clone(),
                            Unexpected,
                            "Recursive const definition",
                        )
                    })?;
                    for ty_expected in proc.ins.iter().rev() {
                        let ty_actual = stack.pop(heap).ok_or_else(|| {
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
                    typecheck_proc(proc_name, items)?;
                    let proc = items[proc_name].0.as_proc().ok_or_else(|| {
                        TypecheckError::new(
                            node.span.clone(),
                            Unexpected,
                            "Recursive const definition",
                        )
                    })?;
                    for ty in &proc.outs {
                        stack.push(heap, *ty)
                    }
                }
                const_name if is_const(const_name, items) => {
                    typecheck_const(const_name, items)?;
                    let const_ = items[const_name].0.as_const().ok_or_else(|| {
                        TypecheckError::new(
                            node.span.clone(),
                            Unexpected,
                            "Recursive const definition",
                        )
                    })?;
                    for ty in &const_.outs {
                        stack.push(heap, *ty);
                    }
                }
                mem_name if is_mem(mem_name, items) => {
                    typecheck_mem(mem_name, items)?;

                    stack.push(heap, Type::ptr_to(Type::U8));
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
                    stack.push(heap, ty);
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
                    let ty = stack.pop(heap).ok_or_else(|| {
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
                    stack.push(heap, Type::U64)
                }
                Intrinsic::ReadU8 => {
                    let ty = stack.pop(heap).ok_or_else(|| {
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
                    stack.push(heap, Type::U8)
                }
                Intrinsic::WriteU64 => {
                    let ty = stack.pop(heap).ok_or_else(|| {
                        TypecheckError::new(
                            node.span.clone(),
                            NotEnoughData,
                            "Not enough data for !u64",
                        )
                    })?;
                    let ty_store = stack.pop(heap).ok_or_else(|| {
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
                    let ty = stack.pop(heap).ok_or_else(|| {
                        TypecheckError::new(
                            node.span.clone(),
                            NotEnoughData,
                            "Not enough data for !u8",
                        )
                    })?;
                    let ty_store = stack.pop(heap).ok_or_else(|| {
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
                &Intrinsic::Cast(ty) => {
                    if !expect_arity(1, stack, heap) {
                        return error(
                            node.span.clone(),
                            NotEnoughData,
                            "Not enough data on the stck for cast operation",
                        );
                    }
                    stack.push(heap, ty)
                }

                Intrinsic::CompStop => {
                    let types: Vec<_> = stack.clone().into_vec(heap);
                    println!("{:?}", types);
                    return error(node.span.clone(), CompStop, "");
                }

                Intrinsic::Syscall0 => {
                    if !expect_arity(1, stack, heap) {
                        return error(
                            node.span.clone(),
                            NotEnoughData,
                            "Not enough data for syscall3",
                        );
                    }
                    stack.push(heap, Type::U64);
                }
                Intrinsic::Syscall1 => {
                    if !expect_arity(2, stack, heap) {
                        return error(
                            node.span.clone(),
                            NotEnoughData,
                            "Not enough data for syscall3",
                        );
                    }
                    stack.push(heap, Type::U64);
                }
                Intrinsic::Syscall2 => {
                    if !expect_arity(3, stack, heap) {
                        return error(
                            node.span.clone(),
                            NotEnoughData,
                            "Not enough data for syscall3",
                        );
                    }
                    stack.push(heap, Type::U64);
                }
                Intrinsic::Syscall3 => {
                    if !expect_arity(4, stack, heap) {
                        return error(
                            node.span.clone(),
                            NotEnoughData,
                            "Not enough data for syscall3",
                        );
                    }
                    stack.push(heap, Type::U64);
                }
                Intrinsic::Syscall4 => {
                    if !expect_arity(5, stack, heap) {
                        return error(
                            node.span.clone(),
                            NotEnoughData,
                            "Not enough data for syscall3",
                        );
                    }
                    stack.push(heap, Type::U64);
                }
                Intrinsic::Syscall5 => {
                    if !expect_arity(6, stack, heap) {
                        return error(
                            node.span.clone(),
                            NotEnoughData,
                            "Not enough data for syscall3",
                        );
                    }
                    stack.push(heap, Type::U64);
                }
                Intrinsic::Syscall6 => {
                    if !expect_arity(7, stack, heap) {
                        return error(
                            node.span.clone(),
                            NotEnoughData,
                            "Not enough data for syscall3",
                        );
                    }
                    stack.push(heap, Type::U64);
                }

                Intrinsic::Argc => {
                    stack.push(heap, Type::U64);
                }
                Intrinsic::Argv => {
                    stack.push(heap, Type::ptr_to(Type::ptr_to(Type::CHAR)));
                }

                Intrinsic::Print | Intrinsic::Drop => {
                    stack.pop(heap).ok_or_else(|| {
                        TypecheckError::new(
                            node.span.clone(),
                            NotEnoughData,
                            "Not enough data to pop",
                        )
                    })?;
                }

                Intrinsic::Dup => {
                    let ty = stack.pop(heap).ok_or_else(|| {
                        TypecheckError::new(
                            node.span.clone(),
                            NotEnoughData,
                            "Not enough data to dup",
                        )
                    })?;
                    stack.push(heap, ty);
                    stack.push(heap, ty);
                }
                Intrinsic::Swap => {
                    let a = stack.pop(heap).ok_or_else(|| {
                        TypecheckError::new(
                            node.span.clone(),
                            NotEnoughData,
                            "Not enough data to swap",
                        )
                    })?;
                    let b = stack.pop(heap).ok_or_else(|| {
                        TypecheckError::new(
                            node.span.clone(),
                            NotEnoughData,
                            "Not enough data to swap",
                        )
                    })?;
                    stack.push(heap, a);
                    stack.push(heap, b);
                }
                Intrinsic::Over => {
                    let a = stack.pop(heap).ok_or_else(|| {
                        TypecheckError::new(
                            node.span.clone(),
                            NotEnoughData,
                            "Not enough data to over",
                        )
                    })?;
                    let b = stack.pop(heap).ok_or_else(|| {
                        TypecheckError::new(
                            node.span.clone(),
                            NotEnoughData,
                            "Not enough data to over",
                        )
                    })?;
                    stack.push(heap, b);
                    stack.push(heap, a);
                    stack.push(heap, b);
                }
                Intrinsic::Add | Intrinsic::Sub | Intrinsic::Mul => {
                    typecheck_binop(stack, heap, node)?
                }
                Intrinsic::Divmod => typecheck_divmod(stack, heap, node)?,
                Intrinsic::Eq
                | Intrinsic::Ne
                | Intrinsic::Lt
                | Intrinsic::Le
                | Intrinsic::Gt
                | Intrinsic::Ge => typecheck_boolean(stack, heap, node)?,
                Intrinsic::Dump => (),
            },
            HirKind::If(cond) => {
                let ty = stack.pop(heap).ok_or_else(|| {
                    TypecheckError::new(node.span.clone(), NotEnoughData, "Not enough data for if")
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
                typecheck_if(
                    name,
                    cond,
                    &node.span.clone(),
                    heap,
                    stack,
                    items,
                    in_const,
                    bindings,
                )?;
            }
            HirKind::While(while_) => {
                let stack_before = stack.clone().into_vec(heap);
                typecheck_body(name, &while_.cond, stack, heap, items, in_const, bindings)?;
                let ty = stack.pop(heap).ok_or_else(|| {
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
                typecheck_body(name, &while_.body, stack, heap, items, in_const, bindings)?;
                if stack.clone().into_vec(heap) != stack_before {
                    return error(node.span.clone(), InvalidWhile, "Invalid while");
                }
            }
            HirKind::Bind(bind) => {
                let mut new_bindings = Vec::new();
                for binding in bind.bindings.iter().rev() {
                    match binding {
                        Binding::Ignore => {
                            stack.pop(heap).ok_or_else(|| {
                                TypecheckError::new(
                                    node.span.clone(),
                                    NotEnoughData,
                                    "Not enough data for binding",
                                )
                            })?;
                        }
                        Binding::Bind { name, ty } => {
                            let actual = stack.pop(heap).ok_or_else(|| {
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
                typecheck_body(name, &bind.body, stack, heap, items, in_const, bindings)?;
            }
            HirKind::IgnorePattern => todo!(), // noop
        }
    }
    ().okay()
}

fn typecheck_cond(
    name: &str,
    node: &HirNode,
    stack: &mut TypeStack,
    heap: &mut THeap,
    items: &mut HashMap<String, (TopLevel, bool)>,
    in_const: bool,
    bindings: &mut Vec<Vec<(String, Type)>>,
) -> Result<()> {
    let ty = stack.pop(heap).ok_or_else(|| {
        TypecheckError::new(node.span.clone(), NotEnoughData, "Not enough data for cond")
    })?;
    let cond = match &node.hir {
        HirKind::Cond(c) => c,
        _ => unreachable!(),
    };
    let mut first_branch_stack = TypeStack::default();
    let mut first_branch = true;
    for CondBranch { pattern, body } in &cond.branches {
        let pat_ty = match &pattern.hir {
            HirKind::Literal(pat) => match pat {
                IConst::Bool(_) => Type::BOOL,
                IConst::U64(_) => Type::U64,
                IConst::I64(_) => Type::I64,
                IConst::Char(_) => Type::CHAR,
                IConst::Str(_) => todo!(),
                IConst::Ptr(_) => Type::ptr_to(Type::ANY),
            },
            HirKind::Word(const_name) if is_const(const_name, items) => {
                typecheck_const(const_name, items)?;
                let const_ = items[const_name].0.as_const().ok_or_else(|| {
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
            typecheck_body(
                name,
                body,
                &mut first_branch_stack,
                heap,
                items,
                in_const,
                bindings,
            )?;
        } else {
            let mut branch_stack = TypeStack::default();
            typecheck_body(
                name,
                body,
                &mut branch_stack,
                heap,
                items,
                in_const,
                bindings,
            )?;
            if !first_branch_stack.eq(&branch_stack, heap) {
                return error(
                    node.span.clone(),
                    TypeMismatch {
                        expected: first_branch_stack.into_vec(heap),
                        actual: branch_stack.into_vec(heap),
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

    let first_branch_stack = first_branch_stack.into_vec(heap);
    for ty in first_branch_stack.into_iter() {
        stack.push(heap, ty)
    }

    ().okay()
}

fn expect_arity(arity: usize, stack: &mut TypeStack, heap: &mut THeap) -> bool {
    for _ in 0..arity {
        if stack.pop(heap).is_none() {
            return false;
        }
    }
    true
}

fn typecheck_divmod(stack: &mut TypeStack, heap: &mut THeap, node: &HirNode) -> Result<()> {
    typecheck_binop(stack, heap, node)?;
    stack.push(heap, Type::U64);
    ().okay()
}

fn typecheck_binop(stack: &mut TypeStack, heap: &mut THeap, node: &HirNode) -> Result<()> {
    let b = stack.pop(heap).ok_or_else(|| {
        TypecheckError::new(
            node.span.clone(),
            NotEnoughData,
            "Not enough data for binary operation",
        )
    })?;
    let a = stack.pop(heap).ok_or_else(|| {
        TypecheckError::new(
            node.span.clone(),
            NotEnoughData,
            "Not enough data for binary operation",
        )
    })?;

    if a == Type::U64 && b == Type::U64 {
        stack.push(heap, Type::U64)
    } else if a == Type::I64 && b == Type::I64 {
        stack.push(heap, Type::I64)
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

fn typecheck_boolean(stack: &mut TypeStack, heap: &mut THeap, node: &HirNode) -> Result<()> {
    let b = stack.pop(heap).ok_or_else(|| {
        TypecheckError::new(
            node.span.clone(),
            NotEnoughData,
            "Not enough data for binary operation",
        )
    })?;
    let a = stack.pop(heap).ok_or_else(|| {
        TypecheckError::new(
            node.span.clone(),
            NotEnoughData,
            "Not enough data for binary operation",
        )
    })?;
    match (a, b) {
        (a, b) if a.type_eq(&b) => stack.push(heap, Type::BOOL),
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

#[allow(clippy::too_many_arguments)]
fn typecheck_if(
    name: &str,
    if_: &If,
    span: &Span,
    heap: &mut THeap,
    stack: &mut TypeStack,
    procs: &mut HashMap<String, (TopLevel, bool)>,
    in_const: bool,
    bindings: &mut Vec<Vec<(String, Type)>>,
) -> Result<()> {
    let (mut truth, mut lie) = (stack.clone(), stack.clone());
    typecheck_body(
        name, &if_.truth, &mut truth, heap, procs, in_const, bindings,
    )?;
    if let Some(lie_body) = &if_.lie {
        typecheck_body(name, &*lie_body, &mut lie, heap, procs, in_const, bindings)?;
    } else {
        return ().okay();
    }
    if truth.eq(&lie, heap) {
        *stack = truth;
        ().okay()
    } else {
        let (actual, expected) = (truth.into_vec(heap), lie.into_vec(heap));
        error(
            span.clone(),
            TypeMismatch { actual, expected },
            "If branches must leave stack in the same state",
        )
    }
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
            }),
            false,
        ),
    )]
    .into_iter()
    .collect();
    assert_matches!(typecheck_proc("main", &mut procs), Ok(()));
}
