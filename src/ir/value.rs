//! # Values in IR
//!
//! The value in IR represents an SSA form value, and can either be the result
//! of an instruction, or the parameter of a block. Each value also should be
//! associated with a type and def-use chain.

use rustc_hash::FxHashMap;

use super::{Block, Context, Func, Inst, Ty};
use crate::{
    collections::{
        linked_list::LinkedListNodePtr,
        storage::{ArenaFree, ArenaPtr, BaseArenaPtr},
    },
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
    self_ptr: Value,
    /// The type of the value.
    ty: Ty,
    /// The kind of the value.
    kind: ValueKind,
    /// The users of the value.
    users: FxHashMap<Inst, usize>,
}

impl ValueData {
    pub(super) fn self_ptr(&self) -> Value { self.self_ptr }

    /// Create a new instruction result value data.
    ///
    /// This will just use the accepted instruction as the value.
    pub(super) fn new_inst_result(self_ptr: Value, ty: Ty, inst: Inst, idx: usize) -> Self {
        Self {
            self_ptr,
            ty,
            kind: ValueKind::InstResult { inst, idx },
            users: FxHashMap::default(),
        }
    }

    /// Create a new block parameter value data.
    ///
    /// This will just use the accepted block as the parent block.
    pub(super) fn new_block_param(self_ptr: Value, ty: Ty, block: Block, idx: usize) -> Self {
        Self {
            self_ptr,
            ty,
            kind: ValueKind::BlockParam { block, idx },
            users: FxHashMap::default(),
        }
    }
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub struct Value(BaseArenaPtr<ValueData>);

impl_arena!(Context, ValueData, Value, values);

impl Value {
    pub(super) fn id(self) -> usize { self.0.id() }

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

    pub fn kind(self, ctx: &Context) -> &ValueKind { &self.deref(ctx).kind }

    pub fn kind_mut(self, ctx: &mut Context) -> &mut ValueKind { &mut self.deref_mut(ctx).kind }

    /// Assign a name for the value.
    ///
    /// # Panics
    ///
    /// - Panics if the name is already assigned to another value.
    /// - Panics if the name is empty.
    /// - Panics if this value is already assigned a name.
    pub fn assign_name(self, ctx: &mut Context, name: impl Into<String>) {
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
    pub fn alloc_name(self, ctx: &mut Context, prefix: impl Into<String>) -> &String {
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

    pub fn name_or_alloc(self, ctx: &mut Context, prefix: impl Into<String>) -> &String {
        if self.name(ctx).is_none() {
            self.alloc_name(ctx, prefix)
        } else {
            self.name(ctx).unwrap()
        }
    }

    pub fn parent_func(self, ctx: &Context) -> Func {
        match self.deref(ctx).kind {
            ValueKind::InstResult { inst, .. } => {
                inst.container(ctx).unwrap().container(ctx).unwrap()
            }
            ValueKind::BlockParam { block, .. } => block.container(ctx).unwrap(),
        }
    }

    pub fn is_block_param(self, ctx: &Context) -> bool {
        matches!(self.deref(ctx).kind, ValueKind::BlockParam { .. })
    }

    pub fn def_inst(self, ctx: &Context) -> Option<Inst> {
        match self.deref(ctx).kind {
            ValueKind::InstResult { inst, .. } => Some(inst),
            ValueKind::BlockParam { .. } => None,
        }
    }
}

impl Usable for Value {
    type U = Inst;

    fn users(self, ctx: &Context) -> Vec<Inst> {
        // keys
        self.deref(ctx).users.keys().copied().collect()
    }

    fn add_user(self, ctx: &mut Context, user: Inst) {
        self.deref_mut(ctx)
            .users
            .entry(user)
            .and_modify(|v| *v += 1)
            .or_insert(1);
    }

    fn remove_user(self, ctx: &mut Context, user: Inst) {
        // decrease the counter, if the counter is 0, remove the user
        if let Some(v) = self.deref_mut(ctx).users.get_mut(&user) {
            *v -= 1;
            if *v == 0 {
                self.deref_mut(ctx).users.remove(&user);
            }
        } else {
            panic!("the user is not found.");
        }
    }

    fn total_uses(self, ctx: &Context) -> usize { self.deref(ctx).users.values().sum() }
}
