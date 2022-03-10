use std::ops::Range;

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct Span {
    pub file: String,
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(file: String, start: usize, end: usize) -> Self {
        Self { file, start, end }
    }
    pub fn point(file: String, point: usize) -> Self {
        Self {
            file,
            start: point,
            end: point + 1,
        }
    }
    pub fn length(&self) -> usize {
        self.end - self.start
    }
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            write!(f, "{}[{}..{}]", &self.file, &self.start, &self.end)
        } else {
            write!(f, "[{}..{}]", &self.start, &self.end)
        }
    }
}

impl ariadne::Span for Span {
    type SourceId = String;

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
    type Context = String;

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
