use std::{
    ops::Range,
    path::{Path, PathBuf},
};

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct Span {
    pub file: PathBuf,
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(file: impl Into<PathBuf>, start: usize, end: usize) -> Self {
        Self {
            file: file.into(),
            start,
            end,
        }
    }
    pub fn point(file: impl Into<PathBuf>, point: usize) -> Self {
        Self {
            file: file.into(),
            start: point,
            end: point + 1,
        }
    }
    pub fn length(&self) -> usize {
        self.end - self.start
    }

    pub fn merge(self, other: Self) -> Self {
        assert!(self.file == other.file);
        assert!(self.start < other.start);
        Self {
            file: self.file,
            start: self.start,
            end: other.end,
        }
    }
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            write!(f, "{:?}[{}..{}]", &self.file, &self.start, &self.end)
        } else {
            write!(f, "[{}..{}]", &self.start, &self.end)
        }
    }
}

impl ariadne::Span for Span {
    type SourceId = Path;

    fn source(&self) -> &Self::SourceId {
        &self.file
    }

    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.end
    }
}

impl chumsky::Span for Span {
    type Context = PathBuf;

    type Offset = usize;

    fn new(file: Self::Context, range: Range<Self::Offset>) -> Self {
        Self::new(file, range.start, range.end)
    }

    fn context(&self) -> Self::Context {
        self.file.clone()
    }

    fn start(&self) -> Self::Offset {
        self.start
    }

    fn end(&self) -> Self::Offset {
        self.end
    }
}
