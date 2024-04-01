use std::{
    fmt::Debug,
    ops::Range,
    path::{Path, PathBuf},
};

use internment::Intern;

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct Spanned<T> {
    pub span: Span,
    pub inner: T,
}

impl<T: Debug> Debug for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            write!(f, "{:#?}@{:#?}", self.inner, self.span)
        } else {
            write!(f, "{:?}@{:?}", self.inner, self.span)
        }
    }
}

impl<T> Spanned<T> {
    pub fn map_ref<F, B>(&self, f: F) -> Spanned<B>
    where
        F: FnOnce(&T) -> B,
    {
        Spanned {
            span: self.span,
            inner: f(&self.inner),
        }
    }
    pub fn map<F, B>(self, f: F) -> Spanned<B>
    where
        F: FnOnce(T) -> B,
    {
        Spanned {
            span: self.span,
            inner: f(self.inner),
        }
    }
}

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct Span {
    pub file: Intern<PathBuf>,
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(file: Intern<PathBuf>, start: usize, end: usize) -> Self {
        Self { file, start, end }
    }
    pub fn point(file: Intern<PathBuf>, point: usize) -> Self {
        Self {
            file,
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

impl Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            write!(
                f,
                "{}[{}..{}]",
                self.file.to_string_lossy(),
                &self.start,
                &self.end
            )
        } else {
            write!(
                f,
                "{}[{}..{}]",
                self.file.file_name().unwrap().to_string_lossy(),
                &self.start,
                &self.end
            )
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

impl chumsky::span::Span for Span {
    type Context = Intern<PathBuf>;

    type Offset = usize;

    fn new(file: Self::Context, range: Range<Self::Offset>) -> Self {
        Self::new(file, range.start, range.end)
    }

    fn context(&self) -> Self::Context {
        self.file
    }

    fn start(&self) -> Self::Offset {
        self.start
    }

    fn end(&self) -> Self::Offset {
        self.end
    }
}
