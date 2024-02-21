use std::fmt;

use crate::ir::values::{BinaryOp, UnaryOp};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Pos {
    row: usize,
    col: usize,
}

impl Default for Pos {
    fn default() -> Self {
        Self { row: 1, col: 0 }
    }
}

impl Pos {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn update(&mut self, c: char) {
        if c == '\n' {
            self.row += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }
    }
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.row, self.col)
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Span {
    start: Pos,
    end: Pos,
}

impl Span {
    pub fn new(start: Pos) -> Self {
        let end = start.clone();
        Self { start, end }
    }

    pub fn update(&mut self, end: Pos) {
        self.end = end;
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct Token {
    pub(super) span: Span,
    pub(super) kind: TokenKind,
}

impl Token {
    pub fn new(span: Span, kind: TokenKind) -> Self {
        Self { span, kind }
    }
}

/// Kinds of tokens.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum TokenKind {
    /// A label identifier
    ///
    /// A label starts with a `^` and followed by a sequence of alphanumeric characters.
    LabelIdent(String),

    /// A global identifier
    ///
    /// A global identifier starts with a `@` and followed by a sequence of alphanumeric characters.
    GlobalIdent(String),

    /// A local identifier
    ///
    /// A local identifier starts with a `%` and followed by a sequence of alphanumeric characters.
    LocalIdent(String),

    /// A type identifier
    ///
    /// A type identifier starts with a `$` and followed by a sequence of alphanumeric characters.
    TypeIdent(String),

    /// A number
    Bytes(Vec<u8>),

    /// A keyword
    Keyword(Keyword),

    /// An instruction operator
    Inst(Inst),

    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Colon,
    Semicolon,
    Arrow,
    Equal,

    /// End of file
    Eof,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum Keyword {
    /// `fn`
    Fn,

    /// `decl`
    Decl,

    /// `iX`
    Int(usize),

    /// `half`
    Half,

    /// `float`
    Float,

    /// `double`
    Double,

    /// `ptr`
    Ptr,

    /// `void`
    Void,

    /// `undef`
    Undef,

    /// `zero`
    Zero,

    /// `global`
    Global,

    /// `const`
    Const,

    /// `Type`
    Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) enum Inst {
    /// A binary operator
    Binary(BinaryOp),

    /// A unary operator
    Unary(UnaryOp),

    /// `store`
    Store,

    /// `load`
    Load,

    /// `alloc`
    Alloc,

    /// `jump`
    Jump,

    /// `br`
    Branch,

    /// `ret`
    Return,

    /// `call`
    Call,

    /// `getelemptr`
    GetElemPtr,
}
