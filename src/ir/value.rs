//! # Values in IR
//!
//! The value in IR represents an SSA form value, and can either be the result
//! of an instruction, or the parameter of a block. Each value also should be
//! associated with a type and def-use chain.

use std::collections::HashSet;

use super::{def_use::Usable, inst::Inst, Context, Ty};
use crate::{
    collections::storage::{ArenaFree, ArenaPtr, BaseArenaPtr},
    impl_arena,
    ir::Block,
};

/// The kinds of a value.
pub enum ValueKind {
    /// The value is the result of an instruction.
    InstResult {
        /// The source instruction.
        inst: Inst,
        /// The index of the result.
        ///
        /// Typically, an instruction can only have one result, but in some
        /// cases, one instruction can have multiple results, so just in case we
        /// need it in the future.
        idx: usize,
    },
    /// The value is a block parameter.
    BlockParam {
        /// The block.
        block: Block,
        /// The index of the parameter in the parameter list.
        idx: usize,
    },
}

pub struct ValueData {
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
    pub(in crate::ir) fn new_inst_result(ty: Ty, inst: Inst, idx: usize) -> Self {
        Self {
            ty,
            kind: ValueKind::InstResult { inst, idx },
            users: HashSet::new(),
        }
    }

    /// Create a new block parameter value data.
    ///
    /// This will just use the accepted block as the parent block.
    pub(in crate::ir) fn new_block_param(ty: Ty, block: Block, idx: usize) -> Self {
        Self {
            ty,
            kind: ValueKind::BlockParam { block, idx },
            users: HashSet::new(),
        }
    }
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub struct Value(BaseArenaPtr<ValueData>);

impl_arena!(Context, ValueData, Value, values);

impl Value {
    pub fn ty(self, ctx: &Context) -> Ty { self.deref(ctx).ty }

    /// Free the value from the context.
    ///
    /// # Panics
    ///
    /// - Panics if the value still has users.
    /// - Panics if the value is a block parameter, use [Block::drop_param]
    ///   instead.
    ///
    /// TODO: After adding a name allocator of values, we should also check if
    /// the value has a name and free the name entry.
    pub(in crate::ir) fn drop(self, ctx: &mut Context) {
        if !Usable::<Inst>::users(self, ctx).is_empty() {
            panic!("cannot remove a value that still has users.");
        }
        if let ValueKind::BlockParam { .. } = self.deref(ctx).kind {
            panic!("cannot remove a block parameter value, use `Block::drop_param` instead.");
        }
        ctx.free(self);
    }
}

impl Usable<Inst> for Value {
    fn users(self, ctx: &Context) -> Vec<Inst> { self.deref(ctx).users.iter().copied().collect() }

    fn add_user(self, ctx: &mut Context, user: Inst) { self.deref_mut(ctx).users.insert(user); }

    fn remove_user(self, ctx: &mut Context, user: Inst) { self.deref_mut(ctx).users.remove(&user); }
}
