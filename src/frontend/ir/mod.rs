//! Parser for IR.
//!
//! The current implementation is not very good, but sufficient for debug
//! purposes.

mod ast;
mod parser;

pub use ast::into_ir;
pub use parser::Parser;
