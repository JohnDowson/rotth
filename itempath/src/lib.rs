#![feature(iter_intersperse)]

use std::{
    borrow::Borrow,
    fmt::{Debug, Display},
    ops::{Deref, DerefMut},
};

use internment::Intern;
use serde::{de::Visitor, Deserialize, Serialize};

#[derive(PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ItemPath {
    segments: [Intern<String>],
}

impl PartialEq<&ItemPath> for ItemPathBuf {
    fn eq(&self, other: &&ItemPath) -> bool {
        self.segments == other.segments
    }
}

impl PartialEq<ItemPath> for ItemPathBuf {
    fn eq(&self, other: &ItemPath) -> bool {
        self.segments == other.segments
    }
}

impl PartialEq<ItemPathBuf> for ItemPath {
    fn eq(&self, other: &ItemPathBuf) -> bool {
        self.segments == other.segments
    }
}

impl ItemPath {
    pub fn iter(&'_ self) -> impl Iterator<Item = &'_ Intern<String>> {
        self.segments.iter()
    }

    pub fn first(&self) -> Option<Intern<String>> {
        self.segments.first().copied()
    }

    pub fn last(&self) -> Option<Intern<String>> {
        self.segments.last().cloned()
    }

    pub fn drop_first(&self) -> Option<(Intern<String>, &Self)> {
        self.segments.split_first().map(|(first, rest)| unsafe {
            (
                *first,
                std::mem::transmute::<&[Intern<String>], &ItemPath>(rest),
            )
        })
    }

    pub fn parent(&self) -> Option<&ItemPath> {
        if self.segments.is_empty() {
            None
        } else {
            let segments = &self.segments[..self.segments.len() - 1];
            let parent = unsafe { std::mem::transmute::<&[Intern<String>], &ItemPath>(segments) };
            Some(parent)
        }
    }

    pub fn join(&self, other: &Self) -> ItemPathBuf {
        let mut segments = self.segments.to_owned();
        segments.extend_from_slice(&other.segments);
        ItemPathBuf { segments }
    }

    pub fn child(&self, segment: impl Into<Intern<String>>) -> ItemPathBuf {
        let mut segments = self.segments.to_owned();
        segments.push(segment.into());
        ItemPathBuf { segments }
    }

    pub fn segment_mut(&mut self, segment: usize) -> Option<&mut Intern<String>> {
        self.segments.get_mut(segment)
    }

    pub fn len(&self) -> usize {
        self.segments.len()
    }

    pub fn is_empty(&self) -> bool {
        self.segments.is_empty()
    }
}

#[macro_export]
macro_rules! path {
    ( $( $s:tt )::+ ) => {{
        let segs = vec![$(stringify!($s).to_string().into(),)*];
        $crate::ItemPathBuf::from(segs)
    }};
    () => {{
        $crate::ItemPathBuf::new()
    }};
}

#[derive(PartialEq, Eq, Hash, Default, Clone)]
#[repr(transparent)]
pub struct ItemPathBuf {
    segments: Vec<Intern<String>>,
}

struct ITPVisitor;

impl<'de> Visitor<'de> for ITPVisitor {
    type Value = ItemPathBuf;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("an item path")
    }

    fn visit_borrowed_str<E>(self, v: &'de str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        let mut itp = ItemPathBuf::new();
        for seg in v.split("::") {
            itp.push(seg.to_string().into());
        }
        Ok(itp)
    }

    fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        self.visit_borrowed_str(&v)
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        self.visit_borrowed_str(v)
    }
}

impl<'de> Deserialize<'de> for ItemPathBuf {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_str(ITPVisitor)
    }
}

impl Serialize for ItemPathBuf {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let separator = Intern::from("::".to_string());
        let segments = self.segments.iter();
        let iter: String = segments
            .intersperse(&separator)
            .map(ToString::to_string)
            .collect();
        serializer.serialize_str(&iter)
    }
}

impl Deref for ItemPathBuf {
    type Target = ItemPath;

    fn deref(&self) -> &Self::Target {
        unsafe { std::mem::transmute::<&[Intern<String>], &ItemPath>(&*self.segments) }
    }
}

impl DerefMut for ItemPathBuf {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { std::mem::transmute::<&mut [Intern<String>], &mut ItemPath>(&mut *self.segments) }
    }
}

impl From<Vec<Intern<String>>> for ItemPathBuf {
    fn from(segments: Vec<Intern<String>>) -> Self {
        Self {
            segments: segments
                .into_iter()
                .map(|s| Intern::new(s.as_ref().into()))
                .collect(),
        }
    }
}

impl Borrow<ItemPath> for ItemPathBuf {
    fn borrow(&self) -> &ItemPath {
        self.deref()
    }
}

impl ToOwned for ItemPath {
    type Owned = ItemPathBuf;

    fn to_owned(&self) -> Self::Owned {
        let segments = self.segments.to_owned();
        Self::Owned { segments }
    }
}

impl ItemPathBuf {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, segment: Intern<String>) {
        self.segments.push(segment)
    }
}

impl Debug for ItemPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let separator = Intern::from("::".to_string());
        let segments = self.segments.iter();
        let iter = segments.intersperse(&separator);
        for s in iter {
            write!(f, "{s}")?
        }
        Ok(())
    }
}

impl Display for ItemPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self, f)
    }
}

impl Debug for ItemPathBuf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self.deref(), f)
    }
}

impl Display for ItemPathBuf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.deref(), f)
    }
}
