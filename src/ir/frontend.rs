use super::values::{BinaryOp, UnaryOp};

pub(self) mod ast;
pub(self) mod lexer;

pub(in crate::ir) mod parser;
pub(in crate::ir) mod tokens;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(self) enum KeywordKind {
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
pub(self) enum InstKind {
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
