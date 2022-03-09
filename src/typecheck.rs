use std::collections::{HashMap, VecDeque};

use simplearena::{Heap, Ref};
use somok::Somok;

use crate::{
    hir::{AstKind, AstNode, IConst, If, Intrinsic, Signature, TopLevel, Type},
    span::Span,
};

#[derive(Debug)]
pub struct Error {
    pub span: Span,
    pub kind: ErrorKind,
    pub message: String,
}
impl Error {
    fn new(span: Span, kind: ErrorKind, message: impl ToString) -> Error {
        Error {
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
    Error::new(span, kind, message).error()
}

pub type Result<T> = std::result::Result<T, Error>;

pub fn typecheck_program(
    items: HashMap<String, (TopLevel, Span)>,
) -> Result<HashMap<String, (TopLevel, Span, bool)>> {
    let mut items = items
        .into_iter()
        .map(|(k, (p, s))| (k, (p, s, false)))
        .collect();

    typecheck_proc("main", &mut items)?;
    items.okay()
}

fn typecheck_const(
    const_name: &str,
    items: &mut HashMap<String, (TopLevel, Span, bool)>,
) -> Result<()> {
    let (const_, span, typechecked) = items
        .get(const_name)
        .ok_or_else(|| {
            Error::new(
                Span::point(0),
                Undefined(const_name.to_string()),
                format!("Proc `{}` does not exist", const_name),
            )
        })?
        .clone();
    let const_ = match const_ {
        TopLevel::Const(c) => c,
        TopLevel::Proc(_) => {
            return error(
                span,
                Unexpected,
                format!("Unexpected const {}, expected proc", const_name),
            )
        }
    };
    if typechecked {
        return ().okay();
    }

    let mut heap = THeap::default();
    let mut actual = TypeStack::default();
    let mut expected = TypeStack::default();
    expected.push(&mut heap, const_.ty);

    typecheck_body(
        const_name,
        &const_.body,
        &mut actual,
        &mut heap,
        items,
        true,
    )?;

    if actual.eq(&expected, &heap) {
        let (_, _, typechecked) = items.get_mut(const_name).unwrap();
        *typechecked = true;
        ().okay()
    } else {
        error(
            span,
            TypeMismatch {
                expected: vec![const_.ty],
                actual: actual.into_deq(&heap).into(),
            },
            "Const body does not equal const type",
        )
    }
}

fn typecheck_proc(name: &str, items: &mut HashMap<String, (TopLevel, Span, bool)>) -> Result<()> {
    let (proc, span, typechecked) = items
        .get(name)
        .ok_or_else(|| {
            Error::new(
                Span::point(0),
                Undefined(name.to_string()),
                format!("Proc `{}` does not exist", name),
            )
        })?
        .clone();
    let proc = match proc {
        TopLevel::Proc(p) => p,
        TopLevel::Const(_) => {
            return error(
                span,
                Unexpected,
                format!("Unexpected const {}, expected proc", name),
            )
        }
    };
    if typechecked {
        return ().okay();
    }
    if name == "main"
        && (!proc.signature.ins.is_empty() || !matches!(&proc.signature.outs[..], [Type::U64]))
    {
        return error(
            span,
            InvalidMain,
            "Main must have no inputs and a single uint output",
        );
    }

    let mut heap = THeap::default();
    let Signature { ins, outs } = &proc.signature;
    let mut actual = TypeStack::default();
    let mut expected = TypeStack::default();
    for ty in ins {
        actual.push(&mut heap, *ty)
    }
    for ty in outs {
        expected.push(&mut heap, *ty)
    }

    typecheck_body(name, &proc.body, &mut actual, &mut heap, items, false)?;

    if !actual.eq(&expected, &heap) {
        error(
            span,
            TypeMismatch {
                actual: actual.into_deq(&heap).into(),
                expected: expected.into_deq(&heap).into(),
            },
            "Type mismatch: proc body does not equal proc outputs",
        )
    } else {
        let (_, _, typechecked) = items.get_mut(name).unwrap();
        *typechecked = true;
        ().okay()
    }
}

fn is_proc(name: &str, items: &HashMap<String, (TopLevel, Span, bool)>) -> bool {
    match items.get(name) {
        Some((t, _, _)) => match t {
            TopLevel::Proc(_) => true,
            TopLevel::Const(_) => false,
        },
        None => false,
    }
}
fn is_const(name: &str, items: &HashMap<String, (TopLevel, Span, bool)>) -> bool {
    match items.get(name) {
        Some((t, _, _)) => match t {
            TopLevel::Proc(_) => false,
            TopLevel::Const(_) => true,
        },
        None => false,
    }
}

fn typecheck_body(
    name: &str,
    body: &[AstNode],
    stack: &mut TypeStack,
    heap: &mut THeap,
    items: &mut HashMap<String, (TopLevel, Span, bool)>,
    in_const: bool,
) -> Result<()> {
    for node in body {
        match &node.ast {
            AstKind::Literal(c) => match c {
                IConst::Bool(_) => stack.push(heap, Type::Bool),
                IConst::U64(_) => stack.push(heap, Type::U64),
                IConst::I64(_) => stack.push(heap, Type::I64),
                IConst::Ptr(_) => stack.push(heap, Type::Ptr),
                IConst::Str(_) => {
                    stack.push(heap, Type::U64);
                    stack.push(heap, Type::Ptr);
                }
            },
            AstKind::Word(w) => match w.as_str() {
                rec if rec == name => {
                    let proc = &items[rec].0.as_proc().ok_or_else(|| {
                        Error::new(node.span, Unexpected, "Recursive const definition")
                    })?;
                    for ty_expected in proc.signature.ins.iter().rev() {
                        let ty_actual = stack.pop(heap).ok_or_else(|| {
                            Error::new(
                                node.span,
                                NotEnoughData,
                                format!("Not enough data for proc invocation {}", rec),
                            )
                        })?;
                        if *ty_expected != ty_actual {
                            return error(
                                node.span,
                                TypeMismatch {
                                    expected: vec![*ty_expected],
                                    actual: vec![ty_actual],
                                },
                                format!("Wrong types for proc invocation {}", rec),
                            );
                        }
                    }
                    for ty in &proc.signature.outs {
                        stack.push(heap, *ty)
                    }
                }
                proc_name if is_proc(proc_name, items) => {
                    if in_const {
                        return error(
                            node.span,
                            CallInConst,
                            "Proc calls not allowed in const context",
                        );
                    }
                    let proc = items[proc_name].0.as_proc().ok_or_else(|| {
                        Error::new(node.span, Unexpected, "Recursive const definition")
                    })?;
                    for ty_expected in proc.signature.ins.iter().rev() {
                        let ty_actual = stack.pop(heap).ok_or_else(|| {
                            Error::new(
                                node.span,
                                NotEnoughData,
                                format!("Not enough data for proc invocation {}", proc_name),
                            )
                        })?;
                        if *ty_expected != ty_actual {
                            return error(
                                node.span,
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
                        Error::new(node.span, Unexpected, "Recursive const definition")
                    })?;
                    for ty in &proc.signature.outs {
                        stack.push(heap, *ty)
                    }
                }
                const_name if is_const(const_name, items) => {
                    typecheck_const(const_name, items)?;
                    let const_ = items[const_name].0.as_const().ok_or_else(|| {
                        Error::new(node.span, Unexpected, "Recursive const definition")
                    })?;
                    stack.push(heap, const_.ty)
                }
                word => {
                    return error(
                        node.span,
                        Undefined(word.to_string()),
                        "Encountered undefined word".to_string(),
                    )
                }
            },
            AstKind::Intrinsic(i) => match i {
                Intrinsic::ReadU8 => {
                    let ty = stack.pop(heap).ok_or_else(|| {
                        Error::new(node.span, NotEnoughData, "Not enough data to pop")
                    })?;
                    if !matches!(ty, Type::Ptr) {
                        return error(
                            node.span,
                            TypeMismatch {
                                actual: vec![ty],
                                expected: vec![Type::Ptr],
                            },
                            "Wrong types for @u8",
                        );
                    }
                    stack.push(heap, Type::U64)
                }
                Intrinsic::WriteU8 => {
                    todo!()
                }
                Intrinsic::PtrAdd => {
                    let offset = stack.pop(heap).ok_or_else(|| {
                        Error::new(node.span, NotEnoughData, "Not enough data to pop")
                    })?;
                    let pointer = stack.pop(heap).ok_or_else(|| {
                        Error::new(node.span, NotEnoughData, "Not enough data to pop")
                    })?;
                    if !matches!((pointer, offset), (Type::Ptr, Type::U64)) {
                        return error(
                            node.span,
                            TypeMismatch {
                                actual: vec![pointer, offset],
                                expected: vec![Type::Ptr, Type::U64],
                            },
                            "Wrong types for ptr+",
                        );
                    }
                    stack.push(heap, Type::Ptr)
                }
                Intrinsic::PtrSub => {
                    let offset = stack.pop(heap).ok_or_else(|| {
                        Error::new(node.span, NotEnoughData, "Not enough data to pop")
                    })?;
                    let pointer = stack.pop(heap).ok_or_else(|| {
                        Error::new(node.span, NotEnoughData, "Not enough data to pop")
                    })?;
                    if !matches!((pointer, offset), (Type::Ptr, Type::U64)) {
                        return error(
                            node.span,
                            TypeMismatch {
                                actual: vec![pointer, offset],
                                expected: vec![Type::Ptr, Type::U64],
                            },
                            "Wrong types for ptr-",
                        );
                    }
                    stack.push(heap, Type::Ptr)
                }

                Intrinsic::CompStop => {
                    let types: Vec<_> = stack.clone().into_deq(heap).into();
                    println!("{:?}", types);
                    return error(node.span, CompStop, "");
                }
                Intrinsic::Print | Intrinsic::Drop | Intrinsic::PutC => {
                    stack.pop(heap).ok_or_else(|| {
                        Error::new(node.span, NotEnoughData, "Not enough data to pop")
                    })?;
                }

                Intrinsic::Dup => {
                    let ty = stack.pop(heap).ok_or_else(|| {
                        Error::new(node.span, NotEnoughData, "Not enough data to dup")
                    })?;
                    stack.push(heap, ty);
                    stack.push(heap, ty);
                }
                Intrinsic::Swap => {
                    let a = stack.pop(heap).ok_or_else(|| {
                        Error::new(node.span, NotEnoughData, "Not enough data to swap")
                    })?;
                    let b = stack.pop(heap).ok_or_else(|| {
                        Error::new(node.span, NotEnoughData, "Not enough data to swap")
                    })?;
                    stack.push(heap, a);
                    stack.push(heap, b);
                }
                Intrinsic::Over => {
                    let a = stack.pop(heap).ok_or_else(|| {
                        Error::new(node.span, NotEnoughData, "Not enough data to over")
                    })?;
                    let b = stack.pop(heap).ok_or_else(|| {
                        Error::new(node.span, NotEnoughData, "Not enough data to over")
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
            AstKind::If(cond) => {
                let ty = stack.pop(heap).ok_or_else(|| {
                    Error::new(node.span, NotEnoughData, "Not enough data for if")
                })?;
                if ty != Type::Bool {
                    return error(
                        node.span,
                        TypeMismatch {
                            actual: vec![ty],
                            expected: vec![Type::Bool],
                        },
                        "If expects a bool",
                    );
                }
                typecheck_if(name, cond, &node.span, heap, stack, items, in_const)?;
            }
            AstKind::While(while_) => {
                let stack_before = stack.clone().into_deq(heap);
                typecheck_body(name, &while_.cond, stack, heap, items, in_const)?;
                let ty = stack.pop(heap).ok_or_else(|| {
                    Error::new(node.span, NotEnoughData, "Not enough data for while")
                })?;
                if ty != Type::Bool {
                    return error(
                        node.span,
                        TypeMismatch {
                            actual: vec![ty],
                            expected: vec![Type::Bool],
                        },
                        "While expects to consume a bool",
                    );
                }
                typecheck_body(name, &while_.body, stack, heap, items, in_const)?;
                if stack.clone().into_deq(heap) != stack_before {
                    return error(
                        node.span,
                        InvalidWhile,
                        "While must leave stack in the same state as it is before",
                    );
                }
            }
            AstKind::Bind(_bind) => todo!(),
        }
    }
    ().okay()
}

fn typecheck_divmod(stack: &mut TypeStack, heap: &mut THeap, node: &AstNode) -> Result<()> {
    typecheck_binop(stack, heap, node)?;
    stack.push(heap, Type::U64);
    ().okay()
}

fn typecheck_binop(stack: &mut TypeStack, heap: &mut THeap, node: &AstNode) -> Result<()> {
    let b = stack.pop(heap).ok_or_else(|| {
        Error::new(
            node.span,
            NotEnoughData,
            "Not enough data for binary operation",
        )
    })?;
    let a = stack.pop(heap).ok_or_else(|| {
        Error::new(
            node.span,
            NotEnoughData,
            "Not enough data for binary operation",
        )
    })?;
    match (a, b) {
        (Type::U64, Type::U64) => stack.push(heap, Type::U64),
        (Type::I64, Type::I64) => stack.push(heap, Type::I64),
        (a, b) => {
            return error(
                node.span,
                TypeMismatch {
                    actual: vec![b, a],
                    expected: vec![b, b],
                },
                "Wrong types for binary operation, must be 2 operands of type uint|int",
            )
        }
    }
    ().okay()
}

fn typecheck_boolean(stack: &mut TypeStack, heap: &mut THeap, node: &AstNode) -> Result<()> {
    let b = stack.pop(heap).ok_or_else(|| {
        Error::new(
            node.span,
            NotEnoughData,
            "Not enough data for binary operation",
        )
    })?;
    let a = stack.pop(heap).ok_or_else(|| {
        Error::new(
            node.span,
            NotEnoughData,
            "Not enough data for binary operation",
        )
    })?;
    match (a, b) {
        (Type::U64, Type::U64) => stack.push(heap, Type::Bool),
        (Type::I64, Type::I64) => stack.push(heap, Type::Bool),
        (a, b) => {
            return error(
                node.span,
                TypeMismatch {
                    actual: vec![b, a],
                    expected: vec![Type::U64, Type::U64],
                },
                "Wrong types for boolean operation",
            )
        }
    }
    ().okay()
}

fn typecheck_if(
    name: &str,
    cond: &If,
    span: &Span,
    heap: &mut THeap,
    stack: &mut TypeStack,
    procs: &mut HashMap<String, (TopLevel, Span, bool)>,
    in_const: bool,
) -> Result<()> {
    let (mut truth, mut lie) = (stack.clone(), stack.clone());
    typecheck_body(name, &cond.truth, &mut truth, heap, procs, in_const)?;
    if let Some(lie_body) = &cond.lie {
        typecheck_body(name, &*lie_body, &mut lie, heap, procs, in_const)?;
    } else {
        return ().okay();
    }
    if truth.eq(&lie, heap) {
        *stack = truth;
        ().okay()
    } else {
        let (actual, expected) = (truth.into_deq(heap).into(), lie.into_deq(heap).into());
        error(
            *span,
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
                    if lhs.ty != rhs.ty {
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

    pub fn into_deq(self, heap: &THeap) -> VecDeque<Type> {
        let mut res = VecDeque::new();
        let mut next = self.top;
        while let Some(top) = next {
            let top = top.deref(heap).unwrap();
            res.push_front(top.ty);
            next = top.prev.clone()
        }
        res
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
    use super::hir::{AstKind, AstNode, Proc};
    use std::assert_matches::assert_matches;
    let mut procs = [(
        "main".to_string(),
        (
            TopLevel::Proc(Proc {
                signature: Signature {
                    ins: vec![],
                    outs: vec![Type::U64],
                },
                body: vec![AstNode {
                    span: Span::point(0),
                    ast: AstKind::Literal(IConst::U64(1)),
                }],
            }),
            Span::point(0),
            false,
        ),
    )]
    .into_iter()
    .collect();
    assert_matches!(typecheck_proc("main", &mut procs), Ok(()));
}
