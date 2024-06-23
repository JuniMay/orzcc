use std::{fmt, path::PathBuf};

/// The source of the IR.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
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
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
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

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: Loc,
    pub end: Loc,
}

impl Span {
    pub fn new(start: Loc, end: Loc) -> Span { Span { start, end } }
}

impl From<(Loc, Loc)> for Span {
    fn from((start, end): (Loc, Loc)) -> Span { Span::new(start, end) }
}
