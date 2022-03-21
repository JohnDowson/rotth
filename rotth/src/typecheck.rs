use simplearena::{Heap, Ref};
use somok::Somok;
use std::collections::VecDeque;

use crate::{span::Span, tir, types::Type, Error};

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
        expected: Vec<tir::Type>,
        actual: Vec<tir::Type>,
    },
    NotEnoughData,
    Undefined,
    InvalidMain,
    InvalidWhile,
    CompStop,
    Unexpected,
    CallInConst,
}
pub fn error<T>(span: Span, kind: ErrorKind, message: impl ToString) -> Result<T> {
    Error::Typecheck(TypecheckError::new(span, kind, message)).error()
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Clone, Default)]
pub struct TypeStack {
    top: Option<TRef>,
}

impl TypeStack {
    pub fn push(&mut self, heap: &mut THeap, ty: tir::Type) {
        let frame = TypeFrame {
            ty,
            prev: self.top.clone(),
        };
        self.top = heap.alloc(frame).some();
    }

    pub fn pop(&mut self, heap: &THeap) -> Option<tir::Type> {
        if let Some(top) = self.top.clone() {
            let top = top.deref(heap).unwrap();
            let prev = top.prev.clone();
            self.top = prev;
            top.ty.clone().some()
        } else {
            None
        }
    }

    pub fn eq(&self, other: &Self, heap: &THeap) -> bool {
        let (mut next_left, mut next_right) = (&self.top, &other.top);
        loop {
            match (next_left, next_right) {
                (Some(lhs), Some(rhs)) => {
                    let lhs = if let Some(lhs) = lhs.deref(heap) {
                        lhs
                    } else {
                        return false;
                    };
                    let rhs = if let Some(rhs) = rhs.deref(heap) {
                        rhs
                    } else {
                        return false;
                    };
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

    pub fn into_vec(self, heap: &THeap) -> Vec<tir::Type> {
        let mut res = VecDeque::new();
        let mut next = self.top;
        while let Some(top) = next {
            let top = top.deref(heap).unwrap();
            res.push_front(top.ty.clone());
            next = top.prev.clone()
        }
        res.into()
    }
}

#[derive(Debug, Clone)]
pub struct TypeFrame {
    ty: tir::Type,
    prev: Option<TRef>,
}

type TRef = Ref<TypeFrame, 0>;
pub type THeap = Heap<TypeFrame, 0>;
