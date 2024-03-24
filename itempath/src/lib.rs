#![feature(iter_intersperse)]

use std::{
    borrow::Borrow,
    fmt::{Debug, Display},
    ops::{Deref, DerefMut},
};

use serde::{de::Visitor, Deserialize, Serialize};
use smol_str::SmolStr;

#[derive(PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct ItemPath {
    segments: [SmolStr],
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
    pub fn iter(&'_ self) -> impl Iterator<Item = &'_ SmolStr> {
        self.segments.iter()
    }

    pub fn only(&self) -> Option<&str> {
        self.segments.first().map(SmolStr::as_str)
    }

    pub fn last(&self) -> Option<SmolStr> {
        self.segments.last().cloned()
    }

    pub fn segment_mut(&mut self, n: usize) -> Option<&mut SmolStr> {
        self.segments.get_mut(n)
    }

    pub fn drop_first(&self) -> Option<&Self> {
        self.segments
            .split_first()
            .and_then(|(_, b)| if b.is_empty() { None } else { Some(b) })
            .map(|s| unsafe { std::mem::transmute::<&[SmolStr], &ItemPath>(s) })
    }

    pub fn parent(&self) -> Option<&ItemPath> {
        if self.segments.is_empty() {
            None
        } else {
            let segments = &self.segments[..self.segments.len() - 1];
            let parent = unsafe { std::mem::transmute::<&[SmolStr], &ItemPath>(segments) };
            Some(parent)
        }
    }

    pub fn join(&self, other: &Self) -> ItemPathBuf {
        let mut segments = self.segments.to_owned();
        segments.extend_from_slice(&other.segments);
        ItemPathBuf { segments }
    }

    pub fn child(&self, segment: impl Into<SmolStr>) -> ItemPathBuf {
        let mut segments = self.segments.to_owned();
        segments.push(segment.into());
        ItemPathBuf { segments }
    }
}

#[macro_export]
macro_rules! path {
    ( $( $s:tt )::+ ) => {{
        let mut path = $crate::ItemPathBuf::new();
        $(path.push(stringify!($s));)*
        path
    }};
    () => {{
        ItemPathBuf::new()
    }};
}

#[derive(PartialEq, Eq, Hash, Default, Clone)]
#[repr(transparent)]
pub struct ItemPathBuf {
    segments: Vec<SmolStr>,
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
            itp.push(seg);
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
        let separator = SmolStr::from("::");
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
        unsafe { std::mem::transmute::<&[SmolStr], &ItemPath>(&*self.segments) }
    }
}

impl DerefMut for ItemPathBuf {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { std::mem::transmute::<&mut [SmolStr], &mut ItemPath>(&mut *self.segments) }
    }
}

impl<T: Into<SmolStr>> From<Vec<T>> for ItemPathBuf {
    fn from(segments: Vec<T>) -> Self {
        Self {
            segments: segments.into_iter().map(Into::into).collect(),
        }
    }
}

impl From<SmolStr> for ItemPathBuf {
    fn from(segment: SmolStr) -> Self {
        Self {
            segments: vec![segment],
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

    pub fn push(&mut self, segment: impl Into<SmolStr>) {
        self.segments.push(segment.into())
    }
}

impl Debug for ItemPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let separator = SmolStr::from("::");
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
