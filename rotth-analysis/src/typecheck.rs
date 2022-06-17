use simplearena::{Heap, Ref};
use somok::Somok;
use std::collections::VecDeque;

use crate::inference::TermId;

#[derive(Clone, Default)]
pub struct TypeStack {
    top: Option<TRef>,
}

impl TypeStack {
    pub fn push(&mut self, heap: &mut THeap, ty: TermId) {
        let frame = TypeFrame {
            ty,
            prev: self.top.clone(),
        };
        self.top = heap.alloc(frame).some();
    }

    pub fn pop(&mut self, heap: &THeap) -> Option<TermId> {
        if let Some(top) = self.top.clone() {
            let top = top.deref(heap).unwrap();
            let prev = top.prev.clone();
            self.top = prev;
            top.ty.some()
        } else {
            None
        }
    }

    pub fn from_iter(tys: impl Iterator<Item = TermId>, heap: &mut THeap) -> Self {
        let mut stack = Self::default();
        for ty in tys {
            stack.push(heap, ty)
        }
        stack
    }

    pub fn into_vec(self, heap: &THeap) -> Vec<TermId> {
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

#[derive(Debug, Clone)]
pub struct TypeFrame {
    ty: TermId,
    prev: Option<TRef>,
}

type TRef = Ref<TypeFrame, 0>;
pub type THeap = Heap<TypeFrame, 0>;
