//! # Values in IR
//!
//! The value in IR represents an SSA form value, and can either be the result
//! of an instruction, or the parameter of a block. Each value also should be
//! associated with a type and def-use chain.

use std::collections::HashSet;

use super::{Block, Context, Inst, Ty};
use crate::{
    collections::storage::{ArenaFree, ArenaPtr, BaseArenaPtr},
    impl_arena,
    utils::def_use::Usable,
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
    pub(super) fn new_inst_result(ty: Ty, inst: Inst, idx: usize) -> Self {
        Self {
            ty,
            kind: ValueKind::InstResult { inst, idx },
            users: HashSet::new(),
        }
    }

    /// Create a new block parameter value data.
    ///
    /// This will just use the accepted block as the parent block.
    pub(super) fn new_block_param(ty: Ty, block: Block, idx: usize) -> Self {
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
    pub(super) fn drop(self, ctx: &mut Context) {
        if !Usable::users(self, ctx).is_empty() {
            panic!("cannot remove a value that still has users.");
        }
        ctx.value_name_alloc.remove_by_ptr(self);
        ctx.free(self);
    }

    /// Assign a name for the value.
    ///
    /// # Panics
    ///
    /// - Panics if the name is already assigned to another value.
    /// - Panics if the name is empty.
    /// - Panics if this value is already assigned a name.
    pub fn assign_name(self, ctx: &mut Context, name: String) {
        ctx.value_name_alloc.assign_name(self, name);
    }

    /// Allocate a name for the value
    ///
    /// # Parameters
    ///
    /// - `ctx`: The context.
    /// - `prefix`: The prefix of the name.
    ///
    /// # Panics
    ///
    /// - Panics if this value is already assigned a name.
    pub fn alloc_name(self, ctx: &mut Context, prefix: String) -> &String {
        ctx.value_name_alloc.alloc_name(self, prefix)
    }

    /// Get the index of the value.
    ///
    /// For an instruction result, this is the index in the result list, for a
    /// block parameter, this is the index in the parameter list.
    pub fn idx(self, ctx: &Context) -> usize {
        match self.deref(ctx).kind {
            ValueKind::InstResult { idx, .. } => idx,
            ValueKind::BlockParam { idx, .. } => idx,
        }
    }

    /// Get the name of the value.
    ///
    /// # Returns
    ///
    /// - `Some(name)`: The name of the value.
    /// - `None`: The value is not assigned/allocated a name yet.
    pub fn name(self, ctx: &Context) -> Option<&String> { ctx.value_name_alloc.get_name(self) }

    pub fn name_or_alloc(self, ctx: &mut Context, prefix: String) -> &String {
        if self.name(ctx).is_none() {
            self.alloc_name(ctx, prefix)
        } else {
            self.name(ctx).unwrap()
        }
    }
}

impl Usable for Value {
    type U = Inst;

    fn users(self, ctx: &Context) -> Vec<Inst> { self.deref(ctx).users.iter().copied().collect() }

    fn add_user(self, ctx: &mut Context, user: Inst) { self.deref_mut(ctx).users.insert(user); }

    fn remove_user(self, ctx: &mut Context, user: Inst) { self.deref_mut(ctx).users.remove(&user); }
}
