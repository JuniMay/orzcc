//! # Infrastructure for Constant Folding
//!
//! The constant folding is built as an infrastructure, because it can be used
//! not only in optimization passes, but also helpful to build an execution
//! engine for the IR.

use std::collections::HashMap;

use super::{constant::FloatConstant, Context, Inst, Value};
use crate::collections::apint::ApInt;

pub enum FoldedConstant {
    Integer(ApInt),
    Float(FloatConstant),
    // TODO: SIMD & struct support
}

/// The context of the constant folding.
///
/// The context is used to store the corresponding [Constant] for the values in
/// the IR.
#[derive(Default)]
pub struct FoldContext {
    /// The folded constant values.
    values: HashMap<Value, FoldedConstant>,
}

impl FoldContext {
    /// Lookup the constant value of the accepted value.
    pub fn lookup(&self, value: Value) -> Option<&FoldedConstant> { self.values.get(&value) }

    /// Set the constant value of the accepted value.
    pub fn set(&mut self, value: Value, constant: FoldedConstant) {
        self.values.insert(value, constant);
    }
}

pub enum FoldResult {
    /// The results of the instruction is folded to constants.
    ///
    /// This is a list of constants, because the instruction may have multiple
    /// results.
    Constants(Vec<FoldedConstant>),
    /// The results of the instruction is not folded.
    NotFolded,
}

impl Inst {
    /// Fold the instruction with a given constant folding context.
    pub fn fold(self, _ctx: &Context, _fold_ctx: &mut FoldContext) -> FoldResult { todo!() }
}
