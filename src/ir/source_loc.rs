use std::{fmt, path::PathBuf};

/// The source of the IR.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Source {
    /// The IR is stored in a file.
    ///
    /// This also indicates that the IR is parsed from a file.
    File { path: PathBuf },
    /// The IR is stored in memory.
    InMemory { name: String },
}

impl fmt::Display for Source {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Source::File { path } => write!(f, "{}", path.display()),
            Source::InMemory { name } => write!(f, "in memory: {}", name),
        }
    }
}

impl Default for Source {
    fn default() -> Source {
        Source::InMemory {
            name: "<default>".to_string(),
        }
    }
}

impl Source {
    pub fn file(path: PathBuf) -> Source { Source::File { path } }

    pub fn in_memory(name: impl Into<String>) -> Source { Source::InMemory { name: name.into() } }
}

/// A location in the source.
#[derive(Default, Clone, Copy, PartialEq, Eq)]
pub enum Loc {
    /// A location in a source file.
    ///
    /// The source of the index is stored in [Context](crate::ir::Context).
    Source { idx: usize },

    /// No location information.
    #[default]
    None,
}

impl From<usize> for Loc {
    fn from(idx: usize) -> Loc { Loc::source(idx) }
}

impl Loc {
    pub fn source(idx: usize) -> Loc { Loc::Source { idx } }
}

impl fmt::Debug for Loc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Loc::Source { idx } => write!(f, "{}", idx),
            Loc::None => write!(f, "<none>"),
        }
    }
}

/// A span in the source.
///
/// # Notes
///
/// Though [PartialEq] and [Eq] is implemented for [Span], the equality of any
/// IR entities should not be determined by the equality of their spans. So,
/// **DERIVE WITH CAUTION** and **ADD TESTS**.
#[derive(Default, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: Loc,
    pub end: Loc,
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}:{:?}", self.start, self.end)
    }
}

impl Span {
    pub fn new(start: Loc, end: Loc) -> Span { Span { start, end } }
}

impl From<(Loc, Loc)> for Span {
    fn from((start, end): (Loc, Loc)) -> Span { Span::new(start, end) }
}
