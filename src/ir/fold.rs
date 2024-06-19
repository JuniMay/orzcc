//! # Infrastructure for Constant Folding
//!
//! The constant folding is built as an infrastructure, because it can be used
//! not only in optimization passes, but also helpful to build an execution
//! engine for the IR.

use std::collections::HashMap;

use super::{Constant, Context, Inst, Value};

/// The context of the constant folding.
///
/// The context is used to store the corresponding [Constant] for the values in
/// the IR.
#[derive(Default)]
pub struct FoldContext {
    /// The folded constant values.
    values: HashMap<Value, Constant>,
}

impl FoldContext {
    /// Lookup the constant value of the accepted value.
    pub fn lookup(&self, value: Value) -> Option<&Constant> { self.values.get(&value) }

    /// Set the constant value of the accepted value.
    pub fn set(&mut self, value: Value, constant: Constant) { self.values.insert(value, constant); }
}

impl Inst {
    /// Fold the instruction with a given constant folding context.
    pub fn fold(self, _ctx: &Context, _fold_ctx: &mut FoldContext) -> Option<Constant> { todo!() }
}
