use std::{fmt::Debug, marker::PhantomData};

#[derive(Clone, Copy, PartialEq)]
struct AllocId<const ID: u32>(u32);
impl<const ID: u32> AllocId<ID> {
    fn as_usize(&self) -> usize {
        self.0 as usize
    }
}

impl<const ID: u32> From<usize> for AllocId<ID> {
    fn from(u: usize) -> Self {
        Self(u as u32)
    }
}

#[derive(Clone, Copy)]
pub struct RefFmt<'h, 'r, T, const ID: u32>(&'r AllocId<ID>, &'h Heap<T, ID>)
where
    T: Debug;

impl<'h, 'r, T, const ID: u32> Debug for RefFmt<'h, 'r, T, ID>
where
    T: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.1.get(self.0))
    }
}

#[derive(Clone, Copy, PartialEq)]
pub struct Ref<T, const ID: u32>(AllocId<ID>, PhantomData<Heap<T, ID>>)
where
    T: 'static;

impl<'h, T, const ID: u32> Ref<T, ID> {
    pub fn as_usize(&self) -> usize {
        self.0.as_usize()
    }

    fn new(alloc_id: AllocId<ID>) -> Self {
        Self(alloc_id, Default::default())
    }

    pub fn deref(&self, heap: &'h Heap<T, ID>) -> Option<&'h T> {
        heap.get(&self.0)
    }
    pub fn deref_mut(&mut self, heap: &'h mut Heap<T, ID>) -> Option<&'h mut T> {
        heap.get_mut(&mut self.0)
    }
}
impl<'h, T, const ID: u32> Ref<T, ID>
where
    T: Debug,
{
    pub fn ref_fmt<'r>(&'r self, heap: &'h Heap<T, ID>) -> RefFmt<'h, 'r, T, ID> {
        RefFmt(&self.0, heap)
    }
}

pub enum Container<T> {
    Free,
    Value(T),
}

impl<T: Debug> Debug for Container<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Free => write!(f, "[]"),
            Self::Value(value) => write!(f, "[{:?}]", value),
        }
    }
}

impl<T> Container<T> {
    fn free(&mut self) {
        *self = Self::Free;
    }

    pub fn as_value(&self) -> Option<&T> {
        if let Self::Value(v) = self {
            Some(v)
        } else {
            None
        }
    }
    pub fn as_value_mut(&mut self) -> Option<&mut T> {
        if let Self::Value(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

pub struct Heap<T, const ID: u32> {
    values: Vec<Container<T>>,
    free: Vec<usize>,
}

impl<T: Debug, const ID: u32> std::fmt::Debug for Heap<T, ID> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Heap")
            .field("inner", &self.values.iter().enumerate().collect::<Vec<_>>())
            .finish()
    }
}

impl<'h, T, const ID: u32> Heap<T, ID> {
    pub fn new() -> Self {
        Self {
            values: Default::default(),
            free: Default::default(),
        }
    }

    pub fn alloc(&mut self, value: T) -> Ref<T, ID> {
        if let Some(slot) = self.free.pop() {
            self.values[slot] = Container::Value(value);
            Ref::new(slot.into())
        } else {
            self.values.push(Container::Value(value));
            Ref::new((self.values.len() - 1).into())
        }
    }

    pub fn drop(&mut self, vref: Ref<T, ID>) {
        self.values[vref.as_usize()].free();
        self.free.push(vref.as_usize())
    }

    fn get(&self, rf: &AllocId<ID>) -> Option<&T> {
        let cont = self.values.get(rf.as_usize())?;
        cont.as_value()
    }

    fn get_mut(&mut self, vref: &mut AllocId<ID>) -> Option<&mut T> {
        let cont = self.values.get_mut(vref.as_usize())?;
        cont.as_value_mut()
    }
}

impl<'h, T, const ID: u32> Default for Heap<T, ID> {
    fn default() -> Self {
        Self::new()
    }
}
