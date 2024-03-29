use super::values::{BinaryOp, CastOp, UnaryOp};

mod lexer;

pub mod ast;
pub mod convert;
pub mod parser;
pub mod tokens;

#[derive(Debug, Clone, PartialEq, Eq)]
enum KeywordKind {
    /// `func`
    Func,

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

    /// `type`
    Type,

    /// `slot`
    Slot,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InstKind {
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

    /// `cast`
    Cast(CastOp),

    /// `getelemptr`
    GetElemPtr,
}
