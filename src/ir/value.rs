//! # Values in IR
//!
//! The value in IR represents an SSA form value, and can either be the result
//! of an instruction, or the parameter of a block. Each value also should be
//! associated with a type and def-use chain.

use std::collections::HashSet;

use super::{def_use::Usable, inst::Inst, Context, Ty};
use crate::{
    collections::storage::{ArenaPtr, ArenaPtrLike},
    impl_arena,
    ir::Block,
};

/// The kinds of a value.
pub enum ValueKind {
    InstResult { inst: Inst },
    BlockParam { block: Block, idx: usize },
}

pub struct ValueData {
    /// The self reference of the value.
    this: Value,
    /// The type of the value.
    ty: Ty,
    /// The kind of the value.
    kind: ValueKind,
    /// The users of the value.
    users: HashSet<Inst>,
}

impl ValueData {
    /// Create a new instruction result value data.
    ///
    /// This will just use the accepted instruction as the value.
    pub(super) fn new_inst_result(this: Value, ty: Ty, inst: Inst) -> Self {
        Self {
            this,
            ty,
            kind: ValueKind::InstResult { inst },
            users: HashSet::new(),
        }
    }

    /// Create a new block parameter value data.
    ///
    /// This will just use the accepted block as the parent block.
    pub(super) fn new_block_param(this: Value, ty: Ty, block: Block, idx: usize) -> Self {
        Self {
            this,
            ty,
            kind: ValueKind::BlockParam { block, idx },
            users: HashSet::new(),
        }
    }
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub struct Value(ArenaPtr<ValueData>);

impl_arena!(Context, ValueData, Value, values);

impl Value {
    pub fn ty(self, ctx: &Context) -> Ty { self.deref(ctx).ty }
}

impl Usable<Inst> for Value {
    fn users(self, ctx: &Context) -> Vec<Inst> { self.deref(ctx).users.iter().copied().collect() }

    fn add_user(self, ctx: &mut Context, user: Inst) { self.deref_mut(ctx).users.insert(user); }

    fn remove_user(self, ctx: &mut Context, user: Inst) { self.deref_mut(ctx).users.remove(&user); }
}
